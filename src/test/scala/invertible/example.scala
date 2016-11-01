/*
 * Copyright 2015 - 2016 Moss Prescott
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

/** Dummy functor-ized AST representing a subset of JavaScript expressions.
  * Using a functor for the AST allows the syntax to decorate the nodes with
  * the source location of each node.
  */
sealed trait Expr[+A]
object Expr {
  final case class Null() extends Expr[Nothing]
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
      case Null()          => G.point(Null())
      case Bool(value)     => G.point(Bool(value))
      case Num(value)      => G.point(Num(value))
      case BinOp(op, l, r) => (f(l) |@| f(r))((l, r) => BinOp(op, l, r))
    }
  }
}

object ExprConstructors {
  import Expr._

  def exprNull[A]  = gen.iso0[Expr[A], Null].apply
  def exprBool[A]  = gen.iso1[Expr[A], Bool].apply
  def exprNum[A]   = gen.iso1[Expr[A], Num].apply
  def exprBinOp[A] = gen.iso3[Expr[A], BinOp[A]].apply

  // NB: re-associated args to work better with `many`:
  def exprBinOpL[A] = toLeftAssoc.inverse >>> exprBinOp[A]

  private def toLeftAssoc[O, A]: Iso[(O, A, A), (A, (O, A))] =
    Iso.partial({ case (op, l, r) => (l, (op, r)) }, { case (l, (op, r)) => (op, l, r) })
}

object ExprSyntax {
  import Iso._
  import Syntax._
  import ExprConstructors._

  def fix[P[_], A] = Iso.total[(P[Cofree[P, A]], A), Cofree[P, A]](
    { case (f, a) => Cofree(a, f) },
    v => (v.tail, v.head))

  def fixUp[P[_]: Foldable, A: Semigroup] = Iso[P[Cofree[P, A]], Cofree[P, A]](
    f => f.foldMap1Opt(_.head).map(Cofree(_, f)),
    _.tail.some)

  // This is effectively the inverse of `fix <> S.pos(p)`
  // NB: the original Position is discarded, so it has to be synthesized
  // when un-parsing, but of course we don't care, so just using
  // `null` here. A cleaner approach would combine uwrapping and
  // re-wrapping so that the actual Position would be available.
  def unPos[P[_]](p: P[Cofree[Expr, Position]])(implicit S: Syntax[P]): P[Expr[Cofree[Expr, Position]]] =
    p ^ fix.inverse ^ second(ignore[Position](null)) ^ unit.inverse

  /** Faster version of chainl1, which parses any set of left-associative
    * infix operators and deals with recording source locations in Cofree.
    * This one is not as clean as chainl1; in particular, it needs to have
    * access to the actual operator values, so it can look them up when
    * unparsing (so it isn't a true combinator).
    */
  def chainlp[P[_], F[_], A](
      term: P[Cofree[F, Position]],
      ops: List[A],
      opf: A => P[A],
      cons: Iso[(Cofree[F, Position], (A, Cofree[F, Position])), F[Cofree[F, Position]]])(
      implicit S: Syntax[P]) = {
    val opP = ops.map(opf).reduce(_ | _)

    val flatten = Iso[(Cofree[F, Position], List[(A, Cofree[F, Position])]), Cofree[F, Position]](
      {
        case (x1, ts) =>
          ts.foldLeftM(x1) {
            // NB: accumulating positions by inspecting the sub-terms,
            // which seems like a hack. Where would we wrap with `S.pos`
            // to capture the position directly?
            case (acc @ Cofree(pos1, _), (op, x @ Cofree(pos2, _))) =>
              cons.app((acc, (op, x))).map(g => Cofree(pos1 |+| pos2, g))
          }
      },
      x => {
        def loop(x: Cofree[F, Position]): Option[(Cofree[F, Position], List[(A, Cofree[F, Position])])] =
          cons.unapp(x.tail).flatMap { case (l, (o, r)) =>
            if (ops contains o) {
              loop(l).map { case (lh, lt) =>
                (lh, (lt ++ List((o, r))))
              }
            }
            else (x, Nil).some
          }.orElse((x, Nil).some)

        loop(x)
      })

    (term * (opP * term).many) ^ flatten
  }

  type T = Cofree[Expr, Position]

  def exprSyntax[P[_]](syntax: Syntax[P]): P[Cofree[Expr, Position]] = {
    import Iso._
    import Syntax._
    import ExprConstructors._
    import Expr._

    implicit val S = syntax

    def node(p: P[Expr[T]]): P[T] = S.pos(p) ^ fix

    // NB: recapture the position to include the parens, which don't get a node of their own.
    def parens(f: P[T]): P[T] = node(text("(") *> unPos(f) <* text(")"))

    def nullP  = node(text("null") ^ exprNull[T])
    def trueP  = node(text("true") ^ (element(true) >>> exprBool[T]))
    def falseP = node(text("false") ^ (element(false) >>> exprBool[T]))

    def numP = {
      val toInt = Iso.total[List[Char], Int](
        _.mkString.toInt,
        _.toString.toList)
      node(digit.many1 ^ (toInt >>> exprNum[T]))
    }

    def infixl(f0: P[T], ops: BinaryOperator*): P[T] = {
      def opf(op: BinaryOperator) = optSpace *> (text(op.js) ^ element(op)) <* optSpace

      chainlp(f0, ops.toList, opf, exprBinOpL)

      // NB: this parses fine, but fails to print because fOps does not handle _all_
      // of the operators. But that requirement is what makes it slow, I think.
      //
      // val fOps = ops.map(op => optSpace *> fOp(op) <* optSpace).reduce(_ <|> _)
      // chainl1(f0, fOps, exprBinOp2 >>> fixUp)
    }

    def infix0 = nullP | trueP | falseP | numP |
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
    lazy val infix11: P[T] = infix10

    def js = skipSpace *> infix11 <* skipSpace

    js
  }

  val ExprParser = Syntax.parser(exprSyntax)
  val ExprPrinter = Syntax.printer(exprSyntax)

  def toTree[A](v: Cofree[Expr, A]): Tree[String] = {
    def label(s: String, a: A) = s"$s $a"
    v match {
      case Cofree(ann, Expr.Null())      => Tree.Leaf(label("null", ann))
      case Cofree(ann, Expr.Bool(true))  => Tree.Leaf(label("true", ann))
      case Cofree(ann, Expr.Bool(false)) => Tree.Leaf(label("false", ann))
      case Cofree(ann, Expr.Num(x))      => Tree.Leaf(label(x.toString, ann))
      case Cofree(ann, Expr.BinOp(op, l, r)) =>
        Tree.Node(label(op.toString, ann),
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
    "(1 + 2) << 3 < 4 && 5 !== 6 % 7 >> 8 ^ 9",
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
      ExprParser("null").map(show) must_== \/-(show(fix(Null(), "1-5")))

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
            pos.startColumn must_== 1
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
            pos.startColumn must_== 7
            exp must contain("\")\"")
            found must beNone
        },
        a => failure(a.toString)
      )
    }
  }

  "isomorphicity" should {
    def roundTrip(s: String): Option[String] =
      ExprParser(s).toOption.flatMap(ExprPrinter).map(_.toString)

    "round-trip with superfluous parens" in {
      val src = "(1 + 2) + 3"
      val exp = "1 + 2 + 3"
      roundTrip(src) must beSome(exp)
    }

    "round-trip with necessary parens" in {
      val src = "(1 + 2) * 3"
      roundTrip(src) must beSome(src)
    }

    "round-trip and fix white space" in {
      val src = "1+2  <  3*4"
      val exp = "1 + 2 < 3 * 4"
      roundTrip(src) must beSome(exp)
    }

    "round-trip a complex expression" in {
      val src = "1 + 2 << 3 < 4 && 5 !== 6 % 7 >> 8 ^ 9"
      roundTrip(src) must beSome(src)
    }
  }
}
