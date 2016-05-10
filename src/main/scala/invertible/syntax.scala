/*
 * Copyright 2014 - 2016 SlamData Inc.
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

import scalaz._, Scalaz._

trait Syntax[F[_]] extends IsoFunctor[F] with ProductFunctor[F] with Alternative[F] {
  // // IsoFunctor
  // def <>[A, B](iso: Iso[A, B], p: F[A]): F[B]
  //
  // // ProductFunctor
  // def <*>[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  //
  // // Alternative
  // def <|>[A](f1: F[A], f2: F[A]): F[A]

  // Defined directly in Syntax
  def pure[A](a: A)(implicit E: Equal[A]): F[A]

  /** Pull(push) a single char from(to) the text. */
  def token: F[Char]

  // optimization(?)
  /** Pull(push) a fixed number of characters at once from(to) the text. */
  def tokenStr(length: Int): F[String]

  /** Records the position before and after parsing some value. */
  def pos[A](p: F[A]): F[(A, Position)]

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

  def letter[F[_]](implicit S: Syntax[F]): F[Char] =
    S.label(subset[Char](_.isLetter) <> S.token, "letter")

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

  /** A parser is simply a pure function from an input sequence to a tuple of:
   * - an optional failure, which represents the most advanced failure yet seen, and
   * - a list of possible results, each paired with the remaining input.
   */
  type Parser[A] = Source => (Option[ParseFailure], List[(A, Source)])

  val ParserSyntax = new Syntax[Parser] {
    def <>[A, B](iso: Iso[A, B], p: Parser[A]) = { r =>
      val (e, ps1) = p(r)
      (e,
        ps1.flatMap { case (a, r1) =>
          iso.app(a).fold[List[(B, Source)]](Nil)((_, r1) :: Nil)
        })
    }

    def <*>[A, B](fa: Parser[A], fb: => Parser[B]) = { r =>
      val (e1, ps1) = fa(r)
      val (e2s: List[Option[ParseFailure]], ps2s: List[List[((A, B), Source)]]) =
        ps1.map { case (a, r1) =>
          val (e, ps2) = fb(r1)
          (e, ps2.map { case (b, r2) => ((a, b), r2) })
        }.unzip
      ((None :: e1 :: e2s).reduce(_ |+| _),
        ps2s.flatten)
    }

    def <|>[A](f1: Parser[A], f2: => Parser[A]) = { r =>
      val (e1, ps1) = f1(r)
      val (e2, ps2) = f2(r)
      (e1 |+| e2, ps1 ++ ps2)
    }

    def pure[A](a: A)(implicit E: Equal[A]) =
      r => (None, List((a, r)))

    def token: Parser[Char] = r =>
      r.first.cata(
        c => (None, List((c, r.rest))),
        (Some(ParseFailure(r, "any char")), Nil))

    def tokenStr(length: Int): Parser[String] = { r =>
      r.prefix(length).cata(
        s => (None, List((s, r.drop(length)))),
        (Some(ParseFailure(r, "any " + length + " chars")), Nil))
    }

    def pos[A](p: Parser[A]): Parser[(A, Position)] = { r =>
      val before = r.pos
      p(r).map(_.map {
        case (a, r1) => ((a, before |+| r1.pos), r1)
      })
    }

    def label[A](p: Parser[A], expected: => String) = { r =>
      val (_, ps) = p(r)
      (if (ps.isEmpty) Some(ParseFailure(r, expected)) else None,
        ps)
    }
  }

  def parser[A](syntax: Syntax[Parser] => Parser[A]): String => (ParseFailure \/ A) = {
    val p = syntax(Syntax.ParserSyntax)

    { s =>
      val r = new Source(s, 0)
      val (err, ps) = p(r)
      val as = ps.collect { case (a, rem) if rem.atEnd => a }
      (err, as) match {
        case (_, a :: Nil)            => \/-(a)
        case (_, as) if as.length > 1 => -\/(sys.error("TODO: ambiguous parse"))
        case (Some(err), Nil)         => -\/(err)
        case (None, Nil)              => -\/(sys.error("TODO: no parse and no error"))
      }
    }
  }


  type Printer[A] = A => Option[Cord]

  val PrinterSyntax = new Syntax[Printer] {
    def <>[A, B](iso: Iso[A, B], p: Printer[A]) =
      b => iso.unapp(b).flatMap(p)

    def <*>[A, B](fa: Printer[A], fb: => Printer[B]) =
      { case (a, b) => (fa(a) |@| fb(b))(_ ++ _) }

    def <|>[A](f1: Printer[A], f2: => Printer[A]) =
      a => f1(a).orElse(f2(a))

    def pure[A](a: A)(implicit E: Equal[A]) =
      x => if (x == a) Some("") else None

    def token: Printer[Char] =
      c => Some(c.toString)

    def tokenStr(length: Int) =
      c => Some(c)

    def pos[A](p: Printer[A]) = { case (a, _) => p(a) }

    def label[A](p: Printer[A], expected: => String) = p
  }

  def printer[A](syntax: Syntax[Printer] => Printer[A]): A => Option[Cord] =
    syntax(Syntax.PrinterSyntax)
}
