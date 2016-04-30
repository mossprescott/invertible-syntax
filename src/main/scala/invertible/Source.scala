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

class Source(chars: java.lang.CharSequence, offset: Int) {
  def atEnd: Boolean = offset >= chars.length

  def first: Option[Char] = if (atEnd) None else Some(chars.charAt(offset))

  def rest: Source = if (atEnd) this else new Source(chars, offset + 1)

  def prefix(n: Int): Option[String] =
    if ((offset + n) <= chars.length) Some(chars.subSequence(offset, offset + n).toString)
    else None

  def drop(n: Int): Source =
    if (atEnd) this
    else if ((offset + n) >= chars.length) new Source(chars, chars.length)
    else new Source(chars, offset + n)

  def pos: Position = Position(chars, offset, offset)
}
