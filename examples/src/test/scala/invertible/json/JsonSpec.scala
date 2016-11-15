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

package invertible.json

import org.specs2.mutable.Specification
import scala.collection.immutable.ListMap

class JsonSpec extends Specification with org.specs2.scalaz.ScalazMatchers {
  def trace(str: String) = {
    val (log, rez) = Json.syntax.traceParse(str)
    log.foreach(println)
    rez
  }

  "JsonSyntax" should {
    "parse simple string" in {
      Json.syntax.parse(""" "abc" """) must beRightDisjunction(
        Fix[Json](Str("abc")))
    }

    "parse Unicode escape" in {
      Json.syntax.parse(""" "\u03BA" """) must beRightDisjunction(
        Fix[Json](Str("κ")))
    }

    "parse simple num" in {
      Json.syntax.parse("""-123.456e789""") must beRightDisjunction(
        Fix[Json](Num(BigDecimal("-123.456"), Some(BigInt("789")))))
    }

    "parse nested arrays" in {
      Json.syntax.parse(""" [[]] """) must beRightDisjunction(
        Fix[Json](Arr(Vector(
          Fix(Arr(Vector()))))))
    }

    "parse simple array" in {
      Json.syntax.parse(""" [ null, true, "abc" ] """) must beRightDisjunction(
        Fix[Json](Arr(Vector(
          Fix(Null()),
          Fix(True()),
          Fix(Str("abc"))))))
    }

    "parse simple object" in {
      Json.syntax.parse(""" { "a": false } """) must beRightDisjunction(
        Fix[Json](Obj(ListMap(
          "a" -> Fix(False())))))
    }

    "print Unicode escape" in {
      Json.syntax.print(Fix(Str("\u0011"))) must beSome(
        "\"\\u0011\"")
    }

    "print num" in {
      Json.syntax.print(Fix[Json](Num(BigDecimal("-123.456"), Some(BigInt("789"))))) must beSome(
        "-123.456e789")
    }
  }
}
