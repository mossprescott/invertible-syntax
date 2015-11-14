/*
 * Copyright 2014 - 2015 SlamData Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package invertible

import scalaz._
import Scalaz._
import scala.util.parsing.input._

trait Syntax[F[_]] extends IsoFunctor[F] with ProductFunctor[F] with Alternative[F] {
  // // IsoFunctor
  // def <>[A, B](iso: Iso[A, B], p: F[A]): F[B]
  //
  // // ProductFunctor
  // def <*>[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  //
  // // Alternative
  // def <|>[A](f1: F[A], f2: F[A]): F[A]

  def empty[A]: F[A]

  // Defined directly in Syntax
  def pure[A](a: A)(implicit E: Equal[A]): F[A]

  /** Pull(push) a single char from(to) the text. */
  def token: F[Char]

  // optimization(?)
  /** Pull(push) a fixed number of characters at once from(to) the text. */
  def tokenStr(length: Int): F[String]

  /** Records the position before and after parsing some value. */
  def pos[A](p: F[A]): F[(A, Syntax.Pos)]
}
object Syntax {
  import Iso._

  def many[A, F[_]](f: F[A])(implicit S: Syntax[F]): F[List[A]] =
    (Iso.nil <> S.pure(())) <|> many1(f)

  def many1[A, F[_]](f: F[A])(implicit S: Syntax[F]): F[List[A]] =
    (Iso.cons <> (f <*> many(f)))

  def text[A, F[_]](s: String)(implicit S: Syntax[F]): F[Unit] =
    if (s == "") S.pure(())
    else
      element(s).inverse <> S.tokenStr(s.length)

  def digit[F[_]](implicit S: Syntax[F]): F[Char] =
    subset[Char](_.isDigit) <> S.token

  def letter[F[_]](implicit S: Syntax[F]): F[Char] =
    subset[Char](_.isLetter) <> S.token

  def *>[A, F[_]](f: F[Unit], g: F[A])(implicit S: Syntax[F]): F[A] = {
    // HACK:
    val unit2 = Iso.iso[A, (Unit, A)](
      { case a => ((), a) },
      { case ((), a) => a })
    unit2.inverse <> (f <*> g)
    // unit.inverse <> (f <*> g)  // with a commute somewhere
  }

  def <*[A, F[_]](f: F[A], g: F[Unit])(implicit S: Syntax[F]): F[A] =
    unit.inverse <> (f <*> g)

  def between[A, F[_]](f: F[Unit], g: F[Unit])(h: F[A])(implicit S: Syntax[F]): F[A] =
    f *> h <* g

  def optional[A, F[_]](f: F[A])(implicit S: Syntax[F]): F[Option[A]] =
    (some[A] <> f) <|> (none[A] <> text(""))

  /**
    arg: a parser/printer for each term, which will handle higher-precedence ops.
    op: a parser/printer for _all_ infix operators.
    f: an iso which applies only to operators (B) with this precedence.
    */
  def chainl1[A, B, F[_]](arg: F[A], op: F[B], f: Iso[(A, (B, A)), A])(implicit S: Syntax[F]): F[A] =
    foldl(f) <> (arg <*> many (op <*> arg))

  /** Accept 0 or more spaces, emit none. */
  def skipSpace[F[_]](implicit S: Syntax[F]): F[Unit] =
    ignore(List[Unit]()) <> many(text(" "))

  /** Accept 0 or more spaces, emit one. */
  def optSpace[F[_]](implicit S: Syntax[F]): F[Unit] =
    ignore(List(())) <> many(text(" "))

  /** Accept 1 or more spaces, emit one. */
  def sepSpace[F[_]](implicit S: Syntax[F]): F[Unit] =
    text(" ") <* skipSpace

  // Finally, some implicit trickery to supply infix operators:
  implicit class SyntaxOps1[A, B, F[_]](iso: Iso[A, B])(implicit S: Syntax[F]) {
    def <>(f: F[A]): F[B] = S.<>(iso, f)
  }
  implicit class SyntaxOps2[A, F[_]](f: F[A])(implicit S: Syntax[F]) {
    def <*>[B](g: => F[B]) = S.<*>(f, g)
    def <|>(g: => F[A]) = S.<|>(f, g)
    def <*(g: F[Unit]) = Syntax.<*(f, g)

    def <+>[B](g: F[B]): F[A \/ B] = (left <> f) <|> (right <> g)
  }
  implicit class SyntaxOps3[F[_]](f: F[Unit])(implicit S: Syntax[F]) {
    def *>[A](g: F[A]) = Syntax.*>(f, g)
  }

  type Pos = (Position, Position) // HACK: probably just need our own simple type

  case class Parser[A] (p: CharSequenceReader => List[(A, CharSequenceReader)]) {
    // TODO: actual error handling. Parse to ParseError \/ A, and record
    // location and what parser failed in the error?
    def parse(s: String): List[String \/ A] = {
      val r = new CharArrayReader(s.toCharArray)
      for { (x, rem) <- p(r) } yield if (rem.atEnd) \/-(x) else -\/("error:\n" + rem.pos.longString)
    }
  }

  val ParserSyntax = new Syntax[Parser] {
    def <>[A, B](iso: Iso[A, B], p: Parser[A]) =
      Parser[B](s =>
        for {
          (x, s1) <- p.p(s)
          y <- iso.app(x)
        } yield (y, s1)
      )

    def <*>[A, B](fa: Parser[A], fb: => Parser[B]) =
      Parser[(A, B)](s =>
        for {
          (a, s1) <- fa.p(s)
          (b, s2) <- fb.p(s1)
        } yield (a, b) -> s2)

    def <|>[A](f1: Parser[A], f2: => Parser[A]) =
      Parser[A](s => f1.p(s) ++ f2.p(s))

    def empty[A]: Parser[A] =
      Parser(s => Nil)

    def pure[A](a: A)(implicit E: Equal[A]) =
      Parser(s => (a, s) :: Nil)

    def token: Parser[Char] =
      Parser { r =>
        val fst = r.first
        if (fst == CharSequenceReader.EofCh) Nil
        else (fst, r.rest) :: Nil
      }

    def tokenStr(length: Int): Parser[String] =
      Parser { r =>
        val s = r.source.subSequence(r.offset, r.offset+length).toString
        if (s.length < length) Nil
        else (s, r.drop(length)) :: Nil
      }

    def pos[A](p: Parser[A]): Parser[(A, Pos)] =
      Parser { r =>
        val before = r.pos
        p.p(r).map {
          case (a, r1) => ((a, (before, r1.pos)), r1)
        }
      }
  }

  final case class Printer[A](print: A => Option[Cord])

  val PrinterSyntax = new Syntax[Printer] {
    def <>[A, B](iso: Iso[A, B], p: Printer[A]) =
      Printer(b => iso.unapp(b).flatMap(p.print))

    def <*>[A, B](fa: Printer[A], fb: => Printer[B]) =
      Printer { case (a, b) => (fa.print(a) |@| fb.print(b))(_ ++ _) }

    def <|>[A](f1: Printer[A], f2: => Printer[A]) =
      Printer(a => f1.print(a).orElse(f2.print(a)))

    def empty[A]: Printer[A] =
      Printer(_ => None)

    def pure[A](a: A)(implicit E: Equal[A]) =
      Printer(x => if (x == a) Some("") else None)

    def token: Printer[Char] =
      Printer(c => Some(c.toString))

    def tokenStr(length: Int): Printer[String] =
      Printer(c => Some(c))

    def pos[A](p: Printer[A]): Printer[(A, Pos)] =
      Printer { case (a, _) => p.print(a) }
  }
}
