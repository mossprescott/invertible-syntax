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

import scalaz._, Scalaz._
import scalaz.concurrent.Task

class JsonSpec extends Specification with org.specs2.scalaz.ScalazMatchers {
  def trace(str: String) = {
    val (log, rez) = Json.syntax.traceParse(str)
    log.foreach(println)
    rez
  }

  "JsonSyntax" should {
    "parse simple string" in {
      Json.syntax.parse(" \"abc\" ") must beRightDisjunction(
        Fix[Json](Str("abc")))
    }

    "parse Unicode escape" in {
      Json.syntax.parse(" \"\\u03BA\" ") must beRightDisjunction(
        Fix[Json](Str("κ")))
    }

    "parse simple num" in {
      Json.syntax.parse("-123.456e789") must beRightDisjunction(
        Fix[Json](Num(BigDecimal("-123.456"), Some(BigInt("789")))))
    }

    "parse nested arrays" in {
      Json.syntax.parse(" [[]] ") must beRightDisjunction(
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

  "test suite" should {
    val testDirUrl = "https://api.github.com/repos/nst/JSONTestSuite/contents/test_parsing"
    val downloadUrl = "https://raw.githubusercontent.com/nst/JSONTestSuite/master/test_parsing/"

    def fromUrl(url: String): Task[Option[String]] = Task.delay {
      // http://stackoverflow.com/a/13632114/101287
      Option(new java.util.Scanner(new java.net.URL(url).openStream(), "UTF-8").useDelimiter("\\A"))
        .flatMap(s => if (s.hasNext()) Option(s.next()) else Some(""))
    }

    val tests = for {
      index <- fromUrl(testDirUrl).flatMap(_.cata(
        Task.now,
        Task.fail(new RuntimeException("index not loaded"))))
      // _=println(index.substring(0, 100))
      files <- Json.syntax.parse(index).fold(
        err => Task.fail(new RuntimeException(err.toString)),
        Task.now)
      // _=println(files)
      names = (files match {
        case Fix(Arr(children)) =>
          children map {
            case Fix(Obj(m)) =>
              m.get("name") match {
                case Some(Fix(Str(n))) =>
                  Some(n)
                case _ =>
                  None
              }
            case _ =>
              None
          }
        case _ =>
          sys.error("no files found")
      }).flatten
      // _=println(names)
      tests = names map { n =>
        if (n ≟ "n_structure_100000_opening_arrays.json" ||
            n ≟ "i_structure_500_nested_arrays.json" ||
            n ≟ "n_structure_open_array_object.json")
          n in skipped("overflows stack")
        else if (n ≟ "y_number_double_close_to_zero.json")
          n in skipped("parsed but not printed properly")
        else
          n in {
            val url = downloadUrl + n.replace("#", "%23")
            fromUrl(url)
              .flatMap(_.cata(
                Task.now,
                Task.fail(new RuntimeException("test not loaded: " + n))))
              .map { str =>
                val parsed = Json.syntax.parse(str)

                val reparsed =
                  parsed.leftMap(_.toString)
                    .flatMap(js => Json.syntax.print(js) \/> "could not print")
                    .flatMap(str => Json.syntax.parse(str).leftMap(_.toString))

                // println(str + "; " + parsed)
                if (n startsWith "y_")
                  (parsed must beRightDisjunction) and
                    (reparsed must_== parsed)
                else if (n startsWith "n_")
                  parsed must beLeftDisjunction
                else if (n startsWith "i_")
                  ok
                else ok
              }
              .unsafePerformSync
          }
      }
    } yield tests

    // The github API is giving 403s, but only in Travis.
    if (sys.env.get("TRAVIS").isEmpty)
      tests.unsafePerformSync

    // what can I say? specs2 is mysterious
    "dummy" >> ok
  }
}
