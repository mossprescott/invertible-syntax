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

import scalaz._, Scalaz._

import invertible._

/** Dummy functor-ized AST representing a subset of JavaScript expressions.
  * Using a functor for the AST allows the syntax to decorate the nodes with
  * the source location of each node.
  */
sealed trait Expr[+A]
object Expr {
  case object Null extends Expr[Nothing]
  final case class Bool(value: Boolean) extends Expr[Nothing]
  final case class Num(value: Int) extends Expr[Nothing]
  final case class BinOp[A](op: BinaryOperator, l: A, r: A) extends Expr[A]

  abstract sealed class BinaryOperator(val js: String)
  case object Add extends BinaryOperator("+")
  case object BitAnd extends BinaryOperator("&")
  case object BitLShift extends BinaryOperator("<<")
  case object BitNot extends BinaryOperator("~")
  case object BitOr extends BinaryOperator("|")
  case object BitRShift extends BinaryOperator(">>")
  case object BitXor extends BinaryOperator("^")
  case object Lt extends BinaryOperator("<")
  case object Lte extends BinaryOperator("<=")
  case object Gt extends BinaryOperator(">")
  case object Gte extends BinaryOperator(">=")
  case object Eq extends BinaryOperator("===")
  case object Neq extends BinaryOperator("!==")
  case object Div extends BinaryOperator("/")
  case object In extends BinaryOperator("in")
  case object And extends BinaryOperator("&&")
  case object Or extends BinaryOperator("||")
  case object Mod extends BinaryOperator("%")
  case object Mult extends BinaryOperator("*")
  case object Sub extends BinaryOperator("-")

  implicit val ExprTraverse: Traverse[Expr] = new Traverse[Expr] {
    def traverseImpl[G[_], A, B](fa: Expr[A])(f: A => G[B])(implicit G: Applicative[G]): G[Expr[B]] = fa match {
      case `Null`          => G.point(Null)
      case Bool(value)     => G.point(Bool(value))
      case Num(value)      => G.point(Num(value))
      case BinOp(op, l, r) => (f(l) |@| f(r))((l, r) => BinOp(op, l, r))
    }
  }
}

object ExprConstructors {
  import Iso._
  import Expr._

  // Lift each constructor into an Iso. NB: these are entirely mechanical
  // and could/should be generated by a macro or derived from something
  // monocle can generate.

  def exprNull[A] = element[Expr[A]](Null)
  def exprBool[A] = iso[Boolean, Expr[A]](
    { case x => Bool(x) },
    { case Bool(x) => x })
  def exprNum[A] = iso[Int, Expr[A]](
    { case x => Num(x) },
    { case Num(x) => x } )

  def exprBinOp[A] = iso[(BinaryOperator, A, A), Expr[A]](
    { case (op, l, r) => BinOp(op, l, r) },
    { case BinOp(op, l, r) => (op, l, r) })
  // Variant with re-associated args to match chainl1's signture:
  def exprBinOp2[A] = iso[(A, (BinaryOperator, A)), Expr[A]](
    { case (l, (op, r)) => BinOp(op, l, r) },
    { case BinOp(op, l, r) => (l, (op, r)) })
}

object ExprSyntax {
  import Iso._
  import Syntax._
  import ExprConstructors._

  def fix[F[_], A] = Iso.total[(F[Cofree[F, A]], A), Cofree[F, A]](
    { case (f, a) => Cofree(a, f) },
    v => (v.tail, v.head))

  def fixUp[F[_]: Foldable, A: Semigroup, B] = Iso[F[Cofree[F, A]], Cofree[F, A]](
    f => f.foldMap1Opt(_.head).map(Cofree(_, f)),
    _.tail.some)

  // This is effectively the inverse of `fix <> S.pos(p)`
  // NB: the original Pos is discarded, so it has to be synthesized
  // when un-parsing, but of course we don't care, so just using
  // `null` here. A cleaner approach would combine uwrapping and
  // re-wrapping so that the actual Pos would be available.
  def unPos[F[_]](p: F[T])(implicit S: Syntax[F]) =
    unit.inverse <> (second(ignore[Pos](null)) <> (fix.inverse <> p))

  type P[F[_]] = Cofree[F, Pos]
  type T = P[Expr]

  def exprSyntax[F[_]](syntax: Syntax[F]): F[Cofree[Expr, Pos]] = {
    import Iso._
    import Syntax._
    import ExprConstructors._
    import Expr._

    implicit val S = syntax

    def parens(f: F[T]): F[T] = {
      val p = text("(") *> f <* text(")")
      // recapture the position to include the parens, which don't get a node of their own.
      fix <> S.pos(unPos(p))
    }

    def nullP  = fix <> S.pos(exprNull[T] <> text("null"))
    def trueP  = fix <> S.pos((element(true) >>> exprBool[T]) <> text("true"))
    def falseP = fix <> S.pos((element(false) >>> exprBool[T]) <> text("false"))

    def numP = {
      val toInt = Iso.total[List[Char], Int](
        _.mkString.toInt,
        _.toString.toList)
      fix <> S.pos((toInt >>> exprNum[T]) <> many1(digit))
    }

    def infixl(f0: F[T], ops: BinaryOperator*): F[T] = {
      def fOp(op: BinaryOperator) = element(op) <> text(op.js)
      val fOps = optSpace *> ops.map(fOp).reduce(_ <|> _) <* optSpace

      chainl1(f0, fOps, exprBinOp2 >>> fixUp)
    }

    def infix0 = nullP <|> trueP <|> falseP <|> numP <|>
        parens(skipSpace *> infix11 <* skipSpace)
    def infix1  = infixl(infix0, Mult, Div, Mod)
    def infix2  = infixl(infix1, Add, Sub)
    def infix3  = infixl(infix2, BitLShift, BitRShift)  // omitted: >>>
    def infix4  = infixl(infix3, Lt, Lte, Gt, Gte, In)  // omitted: instanceof
    def infix5  = infixl(infix4, Eq, Neq)  // omitted: ==, !=
    def infix6  = infixl(infix5, BitAnd)
    def infix7  = infixl(infix6, BitXor)
    def infix8  = infixl(infix7, BitOr)
    def infix9  = infixl(infix8, And)
    def infix10 = infixl(infix9, Or)
    // omitted: all assignment ops: "=", "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", ">>>=", "&=", "^=", "|="
    lazy val infix11: F[T] = infix10

    def js = skipSpace *> infix11 <* skipSpace

    js
  }

  val ExprParser = Syntax.parser(exprSyntax)
  val ExprPrinter = Syntax.printer(exprSyntax)

  def toTree[A](v: Cofree[Expr, A]): Tree[String] = {
    def label(s: String, a: A) = s"$s $a"
    v match {
      case Cofree(ann, Expr.Null)        => Tree.leaf(label("null", ann))
      case Cofree(ann, Expr.Bool(true))  => Tree.leaf(label("true", ann))
      case Cofree(ann, Expr.Bool(false)) => Tree.leaf(label("false", ann))
      case Cofree(ann, Expr.Num(x))      => Tree.leaf(label(x.toString, ann))
      case Cofree(ann, Expr.BinOp(op, l, r)) =>
        Tree.node(label(op.toString, ann),
          toTree(l) #:: toTree(r) #:: Stream.empty)
    }
  }
}

object Test extends App {
  import ExprSyntax._

  val examples = List(
    // "",
    // "null",
    // " null ",
    // "123",
    // " 123 ",
    // "1+2",
    // "1 + 2",
    // "1 + 2 + 3",
    // "(1+2)+3",  // Left-recursive, so equivalent to previous
    // "1+(2+3)",  // Not left-recursive, so the parens are not removed (preserving eval. order)
    // "1 - 2",
    // "1 - 2 + 3",
    // "1 - (2 + 3)",
    // "1 * 2",
    // "(1 + 2) * 3",
    " 1+( 2*3 ) ",
    // "true",
    // "   true ",
    // "false",
    // "1 < 2",
    // "(1 < 2) && true",  // For some reason this parses two ways, with the same result.
    "2 ^ 3 & 4 || 5 >> 7 <= 8 in 9",  // ~20s to parse this one! (not any more)

    // errors:
    "1 + )",
    "1 + (2*3 - )")

  examples.foreach { src =>
    println("\"" + src + "\"")
    ExprParser(src).fold(
      err => println(err),
      expr => {
        println(toTree(expr).drawTree)
        val p = ExprPrinter(expr)
        println("--> " + p.map("\"" + _ + "\"").getOrElse(""))
      })
    println("")
  }
}

import org.specs2.mutable._

class ExprSyntaxSpecs extends Specification {
  import Syntax._
  import Expr._
  import ExprSyntax._

  def show[A](v: Cofree[Expr, A]) = toTree(v).drawTree.toString

  def fix(v: Expr[Cofree[Expr, String]], s: String) = Cofree(s, v)

  "ExprParser" should {
    "parse simple values" in {
      ExprParser("null").map(show) must_== \/-(show(fix(Null, "1-5")))

      ExprParser(" 123 ").map(show) must_== \/-(show(fix(Num(123), "2-5")))
    }

    "parse non-trivial expression" in {
      ExprParser("(1 + 2)*3").map(show) must_== \/-(show(
        fix(
          BinOp(Mult,
            fix(
              BinOp(Add,
                fix(Num(1), "2-3"),
                fix(Num(2), "6-7")),
              "1-8"),
              fix(Num(3), "9-10")),
            "1-10")))
    }

    "fail with unexpected char" in {
      ExprParser("_").fold[org.specs2.execute.Result](
        {
          case ParseFailure(pos, exp, found) =>
            pos.column must_== 1
            exp must contain("\"null\"", "\"true\"", "\"false\"", "digit", "\"(\"", "\" \"")
            found must beSome("_")
        },
        a => failure(a.toString)
      )
    }

    "fail with missing token" in {
      ExprParser("(1 + 2").fold[org.specs2.execute.Result](
        {
          case ParseFailure(pos, exp, found) =>
            pos.column must_== 7
            exp must contain("\")\"")
            found must beNone
        },
        a => failure(a.toString)
      )
    }
  }
}
