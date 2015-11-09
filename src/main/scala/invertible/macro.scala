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

// Not working yet in any sense.
object IsoMacro {
  // import scala.language.experimental.macros
  // import scala.reflect.macros.blackbox.Context
  // import scala.reflect.runtime.universe._

  /** Synonym for `element`. */
  def iso0[T](t: T) = Iso.element(t)

  // def iso1[A, T, S <: T]: Iso[A, T] = macro constructorIso[A, T, S]
  //
  // def constructorIso[A: c.WeakTypeTag, T: c.WeakTypeTag, S <: T](c: Context): c.Expr[Iso[A, T]] =
  // {
  //   c.universe.reify(
  //     Iso[A, T](
  //      a => some(c.Expr[S](???).splice),
  //      _ match {
  //        // case c.Expr[T](???).splice => Some(c.Expr[A](???).splice)
  //        case _ => None
  //      }))
  // }
}
