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

case class Position(chars: java.lang.CharSequence, startOffset: Int, endOffset: Int) {
  /** Line number of the start of the position; line numbers start at 1. */
  def startLine: Int = line(startOffset)

  /** Column number of the start of the position; column numbers start at 1. */
  def startColumn: Int = column(startOffset)

  /** Line number of the end of the position; line numbers start at 1. */
  def endLine: Int = line(endOffset)

  /** Column number of the end of the position; column numbers start at 1. */
  def endColumn: Int = column(endOffset)

  /** A visual representation of the position, showing the line(s) and carets indicating the position(s). */
  // TODO: disambiguate 0-length from 1-character positions.
  def longString: String = {
    def ws(c: Char) = if (c == '\t') c else ' '

    // TODO: handle line-crossings
    val contents = lineContents(startLine)
    contents + "\n" + contents.take(startColumn-1).map(ws) + "^"*math.max(endOffset - startOffset, 1)
  }

  /** Compare start locations. */
  def <(that: Position): Boolean = {
    this.startOffset < that.startOffset
  }

  override def toString =
    if (lineIndex.length <= 2) {
      if (startOffset == endOffset)
        s"$startColumn"
      else
        s"$startColumn-$endColumn"
    }
    else if (startLine == endLine) {
      if (startOffset == endOffset)
        s"$startLine:$startColumn"
      else
        s"$startLine:$startColumn-$endColumn"
    }
    else
      s"$startLine:$startColumn-$endLine:$endColumn"

  private lazy val lineIndex: Vector[Int] =
    (0 until chars.length).filter(x => x == 0 || chars.charAt(x - 1) == '\n').toVector :+ chars.length+1

  private def line(offset: Int): Int = {
    @scala.annotation.tailrec
    def loop(lo: Int, hi: Int): Int =
      if (lo + 1 >= hi) lo
      else {
        val mid = (lo + hi)/2
        if (offset < lineIndex(mid)) loop(lo, mid)
        else loop(mid, hi)
      }

    loop(0, lineIndex.length-1) + 1
  }

  private def column(offset: Int): Int =
    offset - lineIndex(line(offset) - 1) + 1

  private def lineContents(line: Int): String =
    chars.subSequence(lineIndex(line-1), lineIndex(line)-1).toString
}

object Position {
  implicit val semigroup: Semigroup[Position] = new Semigroup[Position] {
    def append(p1: Position, p2: => Position) = Position(p1.chars, p1.startOffset, p2.endOffset)
  }
}
