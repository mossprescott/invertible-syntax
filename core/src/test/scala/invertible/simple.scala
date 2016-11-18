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
  object AST {
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
  }
  import AST._

  // In the paper, these are generated with `defineIsomorphisms`. Here, we use
  // shapeless's Generic, which has to be in a separate scope from where the
  // sealed trait is defined.
  object Constr {
    val variable = gen.iso1[Expression, Variable].apply
    val literal = gen.iso1[Expression, Literal].apply
    val binOp: Iso[(Expression, (Operator, Expression)), Expression] =
      gen.iso3[Expression, BinOp].apply <<< flatten
    val ifZero: Iso[((Expression, Expression), Expression), Expression] =
      gen.iso3[Expression, IfZero].apply <<< flatten <<< associate.inverse
    val addOp = element[Operator](AddOp)
    val mulOp = element[Operator](MulOp)
  }
  import Constr._

  // 6.4. Syntax descriptions

  def expressionSyntax = new Syntax[Expression] {
    def apply[P[_]: Transcriber]: P[Expression] = {
      import Syntax._

      val keywords = List("ifzero", "else")

      val identifier =
        (letter * (letter | digit).many) ^
          (cons >>> chars >>> subset(s => !(keywords contains s)))

      def keyword(str: String) = (identifier |+| text(str)) ^ right.inverse

      val integer =
        digit.many1 ^ chars >>> total[String, Int](_.toInt, _.toString)

      val parens: P[Expression] => P[Expression] =
        _.between(text("("), text(")"))

      val ops = text("*") ^ mulOp |
                text("+") ^ addOp

      val spacedOps = ops.between(optSpace, optSpace)

      val priority: Operator => Int = {
        case MulOp => 1
        case AddOp => 2
      }

      def binOpPrio(p: Int) =
        subset[(Expression, (Operator, Expression))] {
          case (_, (op, _)) => priority(op) == p
        } >>> binOp

      def ifzero = keyword("ifzero") *> optSpace *> parens(expression) <*>
                  optSpace *> parens(expression) <*>
                  optSpace *> keyword("else") *> optSpace *> parens(expression)

      def exp0 = integer ^ literal |
                  identifier ^ variable |
                  ifzero ^ ifZero |
                  parens(skipSpace *> expression <* skipSpace)
      def exp1 = chainl1(exp0, spacedOps, binOpPrio(1))
      def exp2 = chainl1(exp1, spacedOps, binOpPrio(2))

      lazy val expression: P[Expression] = exp2

      expression
    }
  }
}


import org.specs2.mutable._

class SimpleSyntaxSpec extends Specification {
  import SimpleSyntax._, AST._
  import scalaz._

  "Parser" should {
    "parse simple literal" in {
      expressionSyntax.parse("1") must_== \/-(Literal(1))
    }

    "parse simple variable" in {
      expressionSyntax.parse("x") must_== \/-(Variable("x"))
    }

    "parse example from the paper" in {
      expressionSyntax.parse("ifzero (2+3*4) (5) else (6)") must_==
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
      expressionSyntax.print(expr).map(_.toString) must beSome("(7 + 8) * 9")
    }
  }
}
