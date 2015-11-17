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

/** A direct port of the example from Tillmann Rendel and Klaus Ostermann. Invertible
  * syntax descriptions: Unifying parsing and pretty printing. Haskell symposium, 2010.
  * http://www.informatik.uni-marburg.de/~rendel/unparse/
  *
  * This parser is simple, elegant, and doesn't perform very well with non-trivial
  * input. See example.scala for a fancier language/parser, with source locations
  * and a more complicated but better-performing approach.
  */
object SimpleSyntax {
  import Iso._

  // 6.2 Abstract Syntax

  sealed trait Expression
  case class Variable(name: String) extends Expression
  case class Literal(value: Int) extends Expression
  case class BinOp(left: Expression, op: Operator, right: Expression)
    extends Expression
  case class IfZero(x: Expression, zero: Expression, otherwise: Expression)
    extends Expression

  sealed trait Operator
  case object AddOp extends Operator
  case object MulOp extends Operator

  // Note: in the paper, these are generated with `defineIsomorphisms`
  val variable: Iso[String, Expression] = iso(
    { case name => Variable(name) },
    { case Variable(name) => name })
  val literal: Iso[Int, Expression] = iso(
    { case value => Literal(value) },
    { case Literal(value) => value })
  val binOp: Iso[(Expression, (Operator, Expression)), Expression] = iso(
    { case (left, (op, right)) => BinOp(left, op, right) },
    { case BinOp(left, op, right) => (left, (op, right)) })
  val ifZero: Iso[(Expression, (Expression, Expression)), Expression] = iso(
    { case (x, (zero, otherwise)) => IfZero(x, zero, otherwise) },
    { case IfZero(x, zero, otherwise) => (x, (zero, otherwise)) })
  val addOp: Iso[Unit, Operator] = iso(
    { case () => AddOp },
    { case AddOp => () })
  val mulOp: Iso[Unit, Operator] = iso(
    { case () => MulOp },
    { case MulOp => () })

  // 6.4. Syntax descriptions

  def expressionSyntax[F[_]](syntax: Syntax[F]): F[Expression] = {
    import Syntax._
    implicit val S = syntax
    import S._

    val keywords = List("ifzero", "else")

    val identifier =
      (cons >>> chars >>> subset((s: String) => !(keywords contains s))) <>
        (letter <*> many(letter <|> digit))

    def keyword(str: String) = right.inverse <> (identifier <+> text(str))

    val integer =
      (chars >>> total[String, Int](_.toInt, _.toString)) <> many1(digit)

    val parens: F[Expression] => F[Expression] =
      between(text("("), text(")")) _

    val ops = (mulOp <> text("*")) <|> (addOp <> text("+"))

    val spacedOps = between(optSpace, optSpace)(ops)

    val priority: Operator => Int = {
      case MulOp => 1
      case AddOp => 2
    }

    def binOpPrio(p: Int) =
      subset[(Expression, (Operator, Expression))] {
        case (_, (op, _)) => priority(op) == p
      } >>> binOp

    // NB: getting no help from operator precedence here in Scala
    def ifzero = keyword("ifzero") *> optSpace *> parens(expression) <*> (optSpace *> parens(expression) <*> (optSpace *> keyword("else") *> optSpace *> parens(expression)))

    def exp0 = (literal <> integer) <|>
                (variable <> identifier) <|>
                (ifZero <> ifzero) <|>
                parens(skipSpace *> expression <* skipSpace)
    def exp1 = chainl1(exp0, spacedOps, binOpPrio(1))
    def exp2 = chainl1(exp1, spacedOps, binOpPrio(2))

    lazy val expression: F[Expression] = exp2

    expression
  }

  val ExpParser = Syntax.parser(expressionSyntax)
  val ExpPrinter = Syntax.printer(expressionSyntax)
}


import org.specs2.mutable._

class SimpleSyntaxSpec extends Specification {
  import SimpleSyntax._
  import scalaz._

  "Parser" should {
    "parse simple literal" in {
      ExpParser("1") must_== \/-(Literal(1))
    }

    "parse simple variable" in {
      ExpParser("x") must_== \/-(Variable("x"))
    }

    "parse example from the paper" in {
      ExpParser("ifzero (2+3*4) (5) else (6)") must_==
        \/-(
          IfZero(
            BinOp(Literal(2), AddOp, BinOp(Literal(3), MulOp, Literal(4))),
            Literal(5),
            Literal(6)))
    }
  }

  "Printer" should {
    "print example from the paper" in {
      val expr = BinOp(BinOp(Literal(7), AddOp, Literal(8)), MulOp, Literal(9))
      ExpPrinter(expr).map(_.toString) must beSome("(7 + 8) * 9")
    }
  }
}
