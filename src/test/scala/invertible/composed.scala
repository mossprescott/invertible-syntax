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

import invertible._
import matryoshka._, TraverseT._, Recursive.ops._, Corecursive.ops._
import scalaz._, Scalaz._

/*
Syntax for a simple expression language, composed out of several distinct
syntaxes via Coproducts.

Here, matryoshka's Fix is used to turn the functors into recursive types (compare to
`example.scala`, where Cofree is used in a similar way, but also captures the source
position as an annotation on each node.)
*/

object Composed {

  // These constructors are each independent:
  final case class Num[A](value: Int)
  final case class Add[A](left: A, right: A)
  final case class Mul[A](left: A, right: A)

  // These two constructors go together, so extend a common trait:
  sealed trait Bound[A]
  final case class Let[A](sym: Symbol, expr: A, body: A) extends Bound[A]
  final case class Free[A](sym: Symbol) extends Bound[A]


  //
  // Iso instances for each constructor:
  //

  import Iso._

  def num[A] = total[Int, Num[A]](Num(_), _.value)
  def add[A] = iso[(A, A), Add[A]](
    { case (l, r) => Add(l, r) },
    { case Add(l, r) => (l, r) })
  def mul[A] = iso[(A, A), Mul[A]](
    { case (l, r) => Mul(l, r) },
    { case Mul(l, r) => (l, r) })

  def let[A] = iso[(Symbol, (A, A)), Bound[A]](
    { case (sym, (expr, body)) => Let(sym, expr, body) },
    { case Let(sym, expr, body) => (sym, (expr, body)) })
  def free[A] = iso[Symbol, Bound[A]](
    { case sym => Free(sym) },
    { case Free(sym) => sym })


  //
  // Some utilities:
  //

  def fixF[F[_], G[_]](implicit I: F :<: G): Iso[F[Fix[G]], Fix[G]] =
    Iso(
      f => Fix[G](I.inj(f)).some,
      g => I.prj(g.unFix))

  // def fix[F[_]]: Iso[F[Fix[F]], Fix[F]] = fixF[F, F]

  // def inject[F[_]: Traverse, G[_]: Functor](implicit I: F :<: G): Iso[Fix[F], Fix[G]] =
  //   Iso[Fix[F], Fix[G]](
  //     f => FunctorT[Fix].transAna[F, G](f)(I.inj).some,
  //     g => TraverseT[Fix].transAnaM[Option, G, F](g)(I.prj))


  import Syntax._

  def parens[P[_], T](f: P[T])(implicit S: Syntax[P]): P[T] =
    text("(") *> skipSpace *> f <* skipSpace <* text(")")

  val int = Iso.total[String, Int](_.toInt, _.toString)

  val symbol = Iso[Char, Symbol](
    c => Symbol(c.toString).some,
    sym => if (sym.name.length == 1) sym.name.charAt(0).some else None)


  /** An alias for syntax declarations, which are applied to some
    * implementation of the Syntax trait to produce a parser/printer.
    */
  // TODO: arguably, _this_ is they type that should be called `Syntax`
  type S[P[_], T] = Syntax[P] => P[T]

  /** Builds a recursive parser given three parser constructors:
    * - one to parse atomic values such as literals and identifiers
    * - one to parse non-atomic expressions
    * - one to handle grouping embedded sub-expressions (typically `parens`)
    */
  // TODO: somehow make these signatures less ridiculous
  def rec[P[_], T[_]](
    atom: S[P, Fix[T]],
    expr: S[P, Fix[T]] => S[P, Fix[T]],
    group: P[Fix[T]] => S[P, Fix[T]]
  ): S[P, Fix[T]] = { s =>

    def term: S[P, Fix[T]] = {
      implicit val S = s
      s => atom(s) <|> group(loop)(s)
    }

    def loop = expr(term)(s)

    loop
  }


  //
  // Syntax for each node:
  //

  def numSyntax[P[_], T[_]](implicit I: Num :<: T): Syntax[P] => P[Fix[T]] = { s =>
    implicit val S = s

    fixF[Num, T] <> ((chars >>> int >>> num[Fix[T]]) <> many1(digit))
  }
  val numParser = Syntax.parser[Fix[Num]](numSyntax)
  val numPrinter = Syntax.printer[Fix[Num]](numSyntax)


  def addSyntax[P[_], T[_]](termSyntax: Syntax[P] => P[Fix[T]])(implicit I: Add :<: T): Syntax[P] => P[Fix[T]] = { s =>
    implicit val S = s

    val term: P[Fix[T]] = termSyntax(s)

    val ts: P[(Fix[T], List[Fix[T]])] = (term <*> many (optSpace *> text("+") *> optSpace *> term))

    foldl(add[Fix[T]] >>> fixF[Add, T]) <> ts
  }
  def addParser[T[_]](termP: Syntax[Parser] => Parser[Fix[T]])(implicit I: Add :<: T) =
    Syntax.parser(addSyntax(termP))
  def addPrinter[T[_]](termP: Syntax[Printer] => Printer[Fix[T]])(implicit I: Add :<: T) =
    Syntax.printer(addSyntax(termP))


  def mulSyntax[P[_], T[_]](termSyntax: Syntax[P] => P[Fix[T]])(implicit I: Mul :<: T): Syntax[P] => P[Fix[T]] = { s =>
    implicit val S = s

    val term: P[Fix[T]] = termSyntax(s)

    val ts: P[(Fix[T], List[Fix[T]])] = (term <*> many (optSpace *> text("*") *> optSpace *> term))

    foldl(mul[Fix[T]] >>> fixF[Mul, T]) <> ts
  }
  def mulParser[T[_]](termP: Syntax[Parser] => Parser[Fix[T]])(implicit I: Mul :<: T) =
    Syntax.parser(mulSyntax(termP))
  def mulPrinter[T[_]](termP: Syntax[Printer] => Printer[Fix[T]])(implicit I: Mul :<: T) =
    Syntax.printer(mulSyntax(termP))


  // TODO: make this generic, too
  def boundSyntax[P[_]](s: Syntax[P]): P[Fix[Bound]] = {
    implicit val S = s

    val name: P[Symbol] = symbol <> letter

    def freeP: P[Fix[Bound]] = fixF[Bound, Bound] <> (free[Fix[Bound]] <> name)

    def letP: P[Fix[Bound]] = fixF[Bound, Bound] <> (let[Fix[Bound]] <>
                      (text("let") *> sepSpace *>
                        name <*>
                        optSpace *> text("=") *> optSpace *>
                        (bound <*>
                        sepSpace *> text("in") *> sepSpace *>
                        bound)))

    lazy val bound: P[Fix[Bound]] = letP <|> freeP

    bound
  }

  val boundParser = Syntax.parser(boundSyntax)
  val boundPrinter = Syntax.printer(boundSyntax)
}


import org.specs2.mutable._

class ComposedSpecs extends Specification {
  import Syntax._
  import Composed._

  // def show[A](v: Fix[Bound]) = Bound.toTree(v).drawTree.toString

  def fix(v: Expr[Cofree[Expr, String]], s: String) = Cofree(s, v)

  "NumParser" should {
    "parse 1" in {
      numParser("1") must_== \/-(
        Fix[Num](Num(1)))
    }

    "parse large number" in {
      numParser("123456789") must_== \/-(
        Fix[Num](Num(123456789)))
    }

    "fail with anything else" in {
      numParser(" ").toOption must beNone
    }
  }

  "MulParser" should {
    // Need a concrete type for the result nodes:
    type T[A] = Coproduct[Mul, Num, A]

    val p: String => ParseFailure \/ Fix[T] = mulParser[T](numSyntax)

    val injMul: Mul :<: T = implicitly
    val injNum: Num :<: T = implicitly

    "parse simple expr" in {
      p("1 * 2") must_== \/-(
        Fix[T](injMul(Mul(
          Fix(injNum(Num(1))),
          Fix(injNum(Num(2)))))))
    }

    "parse left assoc. expr" in {
      p("1 * 2 * 3") must_== \/-(
        Fix[T](injMul(Mul(
          Fix(injMul(Mul(
            Fix(injNum(Num(1))),
            Fix(injNum(Num(2)))))),
          Fix(injNum(Num(3)))))))
    }

    "fail with unexpected terms" in {
      p("a * b").toOption must beNone
    }

    "fail with unexpected operator" in {
      p("1 + 2").toOption must beNone
    }
  }

  "AddMulParser" should {
    // Nesting the Mul syntax directly inside Add means _only_ parsing one shape

    type T0[A] = Coproduct[Mul, Num, A]
    type T[A] = Coproduct[Add, T0, A]

    val injAdd: Add :<: T = implicitly
    val injMul: Mul :<: T = implicitly
    // NB: why do I need to do this explicitly?
    // val injNum: Num :<: T = implicitly
    implicit val injNum: Num :<: T = Inject.rightInjectInstance[Num, T0, Add](Inject[Num, T0])

    def syntax[P[_]] = addSyntax[P, T](mulSyntax(numSyntax))
    val parse = Syntax.parser(syntax)
    val print = Syntax.printer(syntax)

    "parse simple expr" in {
      parse("1 * 2 + 3 * 4") must_== \/-(
        Fix[T](injAdd(Add(
          Fix(injMul(Mul(
            Fix(injNum(Num(1))),
            Fix(injNum(Num(2)))))),
          Fix(injMul(Mul(
            Fix(injNum(Num(3))),
            Fix(injNum(Num(4))))))))))
    }

    "parse with only *" in {
      parse("1 * 2") must_== \/-(
        Fix[T](injMul(Mul(
            Fix(injNum(Num(1))),
            Fix(injNum(Num(2)))))))
    }

    "parse with only +" in {
      parse("1 + 2") must_== \/-(
        Fix[T](injAdd(Add(
            Fix(injNum(Num(1))),
            Fix(injNum(Num(2)))))))
    }

    "round-trip and fix white space" in {
      parse("1*2  +  3").toOption.flatMap(print).map(_.toString) must beSome("1 * 2 + 3")
    }
  }

  "RecursiveAddMulParser" should {
    // Adding an alternative to nest with parens makes the parser general

    type T0[A] = Coproduct[Mul, Num, A]
    type T[A] = Coproduct[Add, T0, A]

    val injAdd: Add :<: T = implicitly
    val injMul: Mul :<: T = implicitly
    // NB: why do I need to do this explicitly?
    // val injNum: Num :<: T = implicitly
    implicit val injNum: Num :<: T = Inject.rightInjectInstance[Num, T0, Add](Inject[Num, T0])

    def syntax[P[_]] = rec[P, T](
      numSyntax[P, T],
      t => addSyntax[P, T](mulSyntax(t)),
      p => s => parens(p)(s))

    val parse = Syntax.parser(syntax)
    val print = Syntax.printer(syntax)

    "parse simple expr" in {
      parse("1 * (2 + 3)") must_== \/-(
        Fix[T](injMul(Mul(
          Fix(injNum(Num(1))),
          Fix(injAdd(Add(
            Fix(injNum(Num(2))),
            Fix(injNum(Num(3))))))))))
    }

    "round-trip with nec. parens" in {
      parse("1 * (2+3)").toOption.flatMap(print).map(_.toString) must beSome("1 * (2 + 3)")
    }

    "round-trip and drop parens" in {
      parse("(1*2) + 3").toOption.flatMap(print).map(_.toString) must beSome("1 * 2 + 3")
    }
  }

  "BoundParser" should {
    "parse something" in {
      boundParser("let x = y in x") must_== \/-(
        Fix[Bound](Let(
          'x,
          Fix(Free('y)),
          Fix(Free('x)))))
    }
  }
}
