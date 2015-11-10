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

  // def empty[A]: F[A]

  // Defined directly in Syntax
  def pure[A](a: A)(implicit E: Equal[A]): F[A]

  /** Pull(push) a single char from(to) the text. */
  def token: F[Char]

  // optimization(?)
  /** Pull(push) a fixed number of characters at once from(to) the text. */
  def tokenStr(length: Int): F[String]

  /** Records the position before and after parsing some value. */
  def pos[A](p: F[A]): F[(A, Syntax.Pos)]

  /** Wrap a parser with a label used in error reporting. */
  def label[A](p: F[A], expected: => String): F[A]
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
      S.label(element(s).inverse <> S.tokenStr(s.length), "\"" + s + "\"")

  def digit[F[_]](implicit S: Syntax[F]): F[Char] =
    S.label(subset[Char](_.isDigit) <> S.token, "digit")

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
  }
  implicit class SyntaxOps3[F[_]](f: F[Unit])(implicit S: Syntax[F]) {
    def *>[A](g: F[A]) = Syntax.*>(f, g)
  }

  type Pos = (Position, Position) // HACK: probably just need our own simple type

  final case class ParseFailure(pos: Position, expected: List[String], found: Option[String]) {
    override def toString = "expected: " + expected.mkString(" or ") + found.fold("")("; found: '" + _ + "'") + "\n" + pos.longString
  }

  implicit val ParseFailureSemigroup = new Semigroup[ParseFailure] {
    def append(f1: ParseFailure, f2: => ParseFailure) =
      if (f1.pos < f2.pos) f2
      else if (f1.pos == f2.pos) ParseFailure(f1.pos, f1.expected ++ f2.expected, f1.found)
      else f1
  }

  // FIXME: this is almost certainly _not_ the right way to capture failures.
  // We need to return multiple partial results from inner parsers, as well
  // as capturing all of the potential matches for <|>, and this allows for
  // that but probably also lots of other values that are not useful.
  final case class Parser[A] (p: CharSequenceReader => List[ParseFailure \/ (A, CharSequenceReader)]) {
    def parse(s: String): ParseFailure \/ A = {
      val r = new CharArrayReader(s.toCharArray)
      val ts = p(r)
      // println(ts.mkString("\n"))
      ts.collect { case \/-((a, rem)) if rem.atEnd => a } match {
        case a :: Nil => \/-(a)
        case _ => -\/(ts.collect { case -\/(e) => e }.reduce (_ |+| _))  // HACK
      }
    }
  }

  val ParserSyntax = new Syntax[Parser] {
    def <>[A, B](iso: Iso[A, B], p: Parser[A]) =
      Parser[B](s =>
        for {
          v <- p.p(s)
        } yield v.flatMap { case (x, s1) => iso.app(x).map((_, s1)) \/> ParseFailure(s.pos, List("[iso failed]"), Some(s.first.toString)) })

    def <*>[A, B](fa: Parser[A], fb: => Parser[B]) =
      Parser[(A, B)](s =>
        for {
          v1 <- fa.p(s)
          v2 <- v1.fold(
            err => List(-\/(err)),
            { case (a, s1) =>
              fb.p(s1).map(_.map { case (b, s2) => ((a, b), s2) })
            })
            // fb.p(s1) match {
            //   case Nil => println(s"expected ${fb.expected()}; found: '${s1.first}'\n${s1.pos.longString}"); Nil
            //   case bs => bs
            // }
        } yield v2)

    def <|>[A](f1: Parser[A], f2: => Parser[A]) =
      Parser[A](
        r => f1.p(r) ++ f2.p(r)  // This is the important bit? Run them both and return both results/failures.
        // FIXME: not right; commits too early. Need to return multiple good (partial) parses?
        // r => f1.p(r).fold(_ => f2.p(r), \/.right),
        // r =>  {
        //   val t1 = f1.p(r)
        //   t1.fold(
        //     err => f2.p(r),
        //     { case (a, s1) => \/-(a, s1) })
        // }
      )

    // def empty[A]: Parser[A] =
    //   Parser(r => Nil, () => "nothing")

    def pure[A](a: A)(implicit E: Equal[A]) =
      Parser(r => List(\/-((a, r))))

    def token: Parser[Char] =
      Parser({ r =>
        val fst = r.first
        if (fst == CharSequenceReader.EofCh) List(-\/(ParseFailure(r.pos, List("any char"), Some(r.first.toString))))
        else List(\/-((fst, r.rest)))
      })

    def tokenStr(length: Int): Parser[String] =
      Parser({ r =>
        val s = r.source.subSequence(r.offset, r.offset+length).toString
        if (s.length < length) List(-\/(ParseFailure(r.pos, List("any " + length + " chars"), Some(r.first.toString))))
        else List(\/-((s, r.drop(length))))
      })

    def pos[A](p: Parser[A]): Parser[(A, Pos)] =
      Parser({ r =>
        val before = r.pos
        p.p(r).map(_.map {
          case (a, r1) => ((a, (before, r1.pos)), r1)
        })
      })

    def label[A](p: Parser[A], expected: => String) =
      Parser(r => p.p(r).map(_.leftMap(_ => ParseFailure(r.pos, List(expected), Some(r.first.toString)))))
  }

  final case class Printer[A](print: A => Option[Cord])

  val PrinterSyntax = new Syntax[Printer] {
    def <>[A, B](iso: Iso[A, B], p: Printer[A]) =
      Printer(b => iso.unapp(b).flatMap(p.print))

    def <*>[A, B](fa: Printer[A], fb: => Printer[B]) =
      Printer { case (a, b) => (fa.print(a) |@| fb.print(b))(_ ++ _) }

    def <|>[A](f1: Printer[A], f2: => Printer[A]) =
      Printer(a => f1.print(a).orElse(f2.print(a)))

    // def empty[A]: Printer[A] =
    //   Printer(_ => None)

    def pure[A](a: A)(implicit E: Equal[A]) =
      Printer(x => if (x == a) Some("") else None)

    def token: Printer[Char] =
      Printer(c => Some(c.toString))

    def tokenStr(length: Int): Printer[String] =
      Printer(c => Some(c))

    def pos[A](p: Printer[A]): Printer[(A, Pos)] =
      Printer { case (a, _) => p.print(a) }

    def label[A](p: Printer[A], expected: => String) = p
  }
}
