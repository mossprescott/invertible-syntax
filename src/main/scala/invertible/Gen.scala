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

import shapeless._

/** Isos from arguments to each of the cases of some (sum) type, accepting flat
  * tuples. */
object gen {
  /** Generate `Iso[Unit, T]` for a no-argument case class extending `T`.
  {{{
  scala> sealed trait Foo
  defined trait Foo

  scala> final case class Foo0() extends Foo
  defined class Foo0

  scala> gen.iso0[Foo, Foo0].apply
  res0: invertible.Iso[Unit,Foo] = Iso(<function1>,<function1>)
  }}}
  */
  object iso0 {
    def apply[S, T <: S]: Aux[S, T] = new Aux[S, T]

    final class Aux[S, T <: S] {
      def apply[P <: Coproduct](implicit
          genT: Generic.Aux[T, HNil],
          genS: Generic.Aux[S, P],
          sel: ops.coproduct.Selector[P, T]
          ): Iso[Unit, S] =
        Iso(
          a => Some(genT.from(HNil)),
          s => genS.to(s).select[T].map(_ => ()))
    }
  }

  /** Generate `Iso[A, T]` for a one-argument case class extending `T`.
  {{{
  scala> final case class Foo1(x: Int) extends Foo
  defined class Foo1

  scala> gen.iso1[Foo, Foo1].apply
  res1: invertible.Iso[Int,Foo] = Iso(<function1>,<function1>)
  }}}
  */
  object iso1 {
    def apply[S, T <: S]: Aux[S, T] = new Aux[S, T]

    final class Aux[S, T <: S] {
      def apply[P <: Coproduct, L <: HList, A](implicit
          genT: Generic.Aux[T, L],
          genS: Generic.Aux[S, P],
          sel: ops.coproduct.Selector[P, T],
          cons: ops.hlist.IsHCons.Aux[L, A, HNil],
          eq: (A :: HNil) =:= L
          ): Iso[A, S] =
        Iso(
          a => Some(genT.from(eq(a :: HNil))),
          s => genS.to(s).select[T].map(genT.to(_).head))
        }
  }

  /** Generate `Iso[(A, B), T]` for a two-argument case class extending `T`.
  {{{
  scala> final case class Foo2(x: Int, y: String) extends Foo
  defined class Foo1

  scala> gen.iso2[Foo, Foo2].apply
  res1: invertible.Iso[(Int, String),Foo] = Iso(<function1>,<function1>)
  }}}
  */
  object iso2 {
    def apply[S, T <: S]: Aux[S, T] = new Aux[S, T]

    final class Aux[S, T <: S] {
      def apply[P <: Coproduct, L <: HList, A, B](implicit
          genT: Generic.Aux[T, L],
          genS: Generic.Aux[S, P],
          sel: ops.coproduct.Selector[P, T],
          tup: ops.hlist.Tupler.Aux[L, (A, B)],
          eq: (A :: B :: HNil) =:= L
          ): Iso[(A, B), S] =
        Iso(
          { case (a, b) => Some(genT.from(a :: b :: HNil)) },
          s => genS.to(s).select[T].map(genT.to(_).tupled))
    }
  }

  /** Generate `Iso[(A, B, C), T]` for a three-argument case class extending `T`.
  {{{
  scala> final case class Foo3(x: Int, y: String, z: Double) extends Foo
  defined class Foo3

  scala> gen.iso3[Foo, Foo3].apply
  res3: invertible.Iso[(Int, String, Double),Foo] = Iso(<function1>,<function1>)
  }}}
  */
  object iso3 {
    def apply[S, T <: S]: Aux[S, T] = new Aux[S, T]

    final class Aux[S, T <: S] {
      def apply[P <: Coproduct, L <: HList, A, B, C](implicit
          genT: Generic.Aux[T, L],
          genS: Generic.Aux[S, P],
          sel: ops.coproduct.Selector[P, T],
          tup: ops.hlist.Tupler.Aux[L, (A, B, C)],
          eq: (A :: B :: C :: HNil) =:= L
          ): Iso[(A, B, C), S] =
        Iso(
          { case (a, b, c) => Some(genT.from(a :: b :: c :: HNil)) },
          s => genS.to(s).select[T].map(genT.to(_).tupled))
    }
  }

}
