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

final case class ParseFailure(pos: Position, expected: List[String], found: Option[String]) {
  override def toString = "expected: " + expected.mkString(" or ") + found.fold("")("; found: '" + _ + "'") + "\n" + pos.longString
}

object ParseFailure {
  def apply(r: Source, expected: String): ParseFailure =
    ParseFailure(r.pos, List(expected), r.first.map(_.toString))

  implicit val semigroup = new Semigroup[Option[ParseFailure]] {
    def append(of1: Option[ParseFailure], of2: => Option[ParseFailure]) = (of1, of2) match {
      case (None, _) => of2
      case (_, None) => of1
      case (Some(f1), Some(f2)) => Some(
        if (f1.pos < f2.pos) f2
        else if (f2.pos < f1.pos) f1
        else ParseFailure(f1.pos, f1.expected ++ f2.expected, f1.found))
    }
  }
}
