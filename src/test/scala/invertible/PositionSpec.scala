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

import org.specs2.mutable._

class PositionSpec extends Specification {
  "lines and columns" should {
    "handle empty location with single line" in {
      Position("abc", 1, 1).toString must_== "2"
    }

    "handle empty location in first line" in {
      Position("abc\ndef", 1, 1).toString must_== "1:2"
    }

    "handle empty location in last line" in {
      Position("abc\ndef", 5, 5).toString must_== "2:2"
    }
  }

  "longString" should {
    "render empty location in single line" in {
      Position("abc", 1, 1).longString must_==
      """abc
        | ^""".stripMargin
    }

    "render non-empty location in single line" in {
      Position("abc def ghi", 4, 7).longString must_==
      """abc def ghi
        |    ^^^""".stripMargin
    }

    "render empty location in first line" in {
      Position("abc\ndef", 1, 1).longString must_==
      """abc
        | ^""".stripMargin
    }

    "render non-empty location in first line" in {
      Position("abc def ghi\njkl mno pqr", 4, 7).longString must_==
      """abc def ghi
        |    ^^^""".stripMargin
    }

    "render empty location in last line" in {
      Position("abc\ndef", 5, 5).longString must_==
      """def
        | ^""".stripMargin
    }

    "render non-empty location in last line" in {
      Position("abc def ghi\njkl mno pqr", 16, 19).longString must_==
      """jkl mno pqr
        |    ^^^""".stripMargin
    }

    "render line-crossing location" in {
      Position("abc def ghi\njkl mno pqr", 8, 15).longString must_==
      """abc def ghi
        |        ^^^
        | jkl mno pqr
        | ^^^""".stripMargin
    }.pendingUntilFixed("TODO")
  }
}
