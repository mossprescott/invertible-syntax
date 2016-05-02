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

/** Isos from arguments to each of the cases of some (sum) type.
  * Usage: `gen.iso0[MyTrait, MyCaseClass].apply`,
  * `gen.iso1[MyTrait, MyCaseClass, Arg1].apply`, etc.
  */
object gen {
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

  object iso1 {
    def apply[S, T <: S, A]: Aux[S, T, A] = new Aux[S, T, A]

    final class Aux[S, T <: S, A] {
      // TODO: could A be inferred here?
      def apply[P <: Coproduct](implicit
          genT: Generic.Aux[T, A :: HNil],
          genS: Generic.Aux[S, P],
          sel: ops.coproduct.Selector[P, T]
          ): Iso[A, S] =
        Iso(
          a => Some(genT.from(a :: HNil)),
          s => genS.to(s).select[T].map(genT.to(_).head))
    }
  }

  object iso2 {
    def apply[S, T <: S, A, B]: Aux[S, T, A, B] = new Aux[S, T, A, B]

    final class Aux[S, T <: S, A, B] {
      // TODO: could A and B be inferred here?
      def apply[P <: Coproduct](implicit
          genT: Generic.Aux[T, A :: B :: HNil],
          genS: Generic.Aux[S, P],
          sel: ops.coproduct.Selector[P, T]
          ): Iso[(A, B), S] =
        Iso(
          { case (a, b) => Some(genT.from(a :: b :: HNil)) },
          s => genS.to(s).select[T].map(genT.to(_).tupled))
    }
  }

  object iso3 {
    def apply[S, T <: S, A, B, C]: Aux[S, T, A, B, C] = new Aux[S, T, A, B, C]

    final class Aux[S, T <: S, A, B, C] {
      // TODO: could A, B, and C be inferred here?
      def apply[P <: Coproduct](implicit
          genT: Generic.Aux[T, A :: B :: C :: HNil],
          genS: Generic.Aux[S, P],
          sel: ops.coproduct.Selector[P, T]
          ): Iso[(A, B, C), S] =
        Iso(
          { case (a, b, c) => Some(genT.from(a :: b :: c :: HNil)) },
          s => genS.to(s).select[T].map(genT.to(_).tupled))
    }
  }

}
