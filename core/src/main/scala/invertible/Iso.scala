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

import scala.collection.immutable.ListMap
import scalaz._, Scalaz._

final case class Iso[A, B](app: A => Option[B], unapp: B => Option[A]) {
  def compose[C](that: Iso[C, A]) = Iso[C, B](
    c => that.app(c).flatMap(app),
    b => unapp(b).flatMap(that.unapp))

  /** Alias for `compose`. */
  def <<<[C](that: Iso[C, A]): Iso[C, B] = compose(that)

  /** Flipped `<<<`. */
  def >>>[C](that: Iso[B, C]) = Iso[A, C](
    a => app(a).flatMap(that.app),
    c => that.unapp(c).flatMap(unapp))

  def |(that: Iso[A, B]): Iso[A, B] = Iso(
    a => this.app(a).orElse(that.app(a)),
    b => this.unapp(b).orElse(that.unapp(b)))

  def inverse: Iso[B, A] = Iso(unapp, app)

  def ***[A2, B2](that: Iso[A2, B2]): Iso[(A, A2), (B, B2)] = Iso(
    { case (a, a2) => (this.app(a) |@| that.app(a2)).tupled },
    { case (b, b2) => (this.unapp(b) |@| that.unapp(b2)).tupled })
}

object Iso {
  def total[A, B](fa: A => B, fb: B => A): Iso[A, B] =
    Iso(a => Some(fa(a)), b => Some(fb(b)))

  def partial[A, B](pa: PartialFunction[A, B], pb: PartialFunction[B, A]): Iso[A, B] =
    Iso(pa.lift, pb.lift)

  def id[A]: Iso[A, A] =
    Iso(Some.apply, Some.apply)

  // Combinators:

  def ignore[A](a: A) = Iso[A, Unit](_ => Some(()), _ => Some(a))

  // TODO: several more combinators.

  def associate[A, B, C] = partial[(A, (B, C)), ((A, B), C)](
    { case (a, (b, c)) => ((a, b), c) },
    { case ((a, b), c) => (a, (b, c)) })

  def commute[A, B] = partial[(A, B), (B, A)](
    { case (a, b) => (b, a) },
    { case (b, a) => (a, b) })

  def unit[A] = partial[A, (A, Unit)](
    { case a => (a, ()) },
    { case (a, ()) => a })

  def flatten[A, B, C] = partial[(A, (B, C)), (A, B, C)](
    { case (a, (b, c)) => (a, b, c) },
    { case (a, b, c) => (a, (b, c)) })

  def element[B](x: B) = Iso[Unit, B](
    _ => Some(x),
    b => if (x == b) Some(()) else None)

  def subset[A](p: A => Boolean): Iso[A, A] = {
    def f(x: A) = if (p(x)) Some(x) else None
    Iso[A, A](f, f)
  }

  def first[A, B, C](iso: Iso[A, C]): Iso[(A, B), (C, B)] = Iso(
    { case (a, b) => iso.app(a).map((_, b)) },
    { case (c, b) => iso.unapp(c).map((_, b)) })

  def second[A, B, C](iso: Iso[B, C]): Iso[(A, B), (A, C)] = Iso(
    { case (a, b) => iso.app(b).map((a, _)) },
    { case (a, c) => iso.unapp(c).map((a, _)) })

  def iterate[A](step: Iso[A, A]): Iso[A, A] = {
    @annotation.tailrec
    def driver(step: A => Option[A], state: A): A =
      step(state) match {
        case Some(state1) => driver(step, state1)
        case None => state
      }
    Iso[A, A](
      a => Some(driver(step.app, a)),
      a => Some(driver(step.unapp, a)))
  }

  // Derived:

  def foldl[A, B](iso: Iso[(A, B), A]): Iso[(A, List[B]), A] = {
    def step: Iso[(A, List[B]), (A, List[B])] = {
      val first: Iso[(A, List[B]), (A, (B, List[B]))] = id *** cons[B].inverse
      val tuple: Iso[(A, (B, List[B])), ((A, B), List[B])] = associate
      val app: Iso[((A, B), List[B]), (A, List[B])] = iso *** id
      first >>> tuple >>> app
    }
    iterate(step) >>> (id *** nil.inverse) >>> unit.inverse
  }


  // Constructors:

  def nil[A] = partial[Unit, List[A]](
    { case () => Nil },
    { case Nil => () })
  def cons[A] = partial[(A, List[A]), List[A]](
    { case (x, xs) => x :: xs },
    { case x :: xs => (x, xs) })
  val chars = total[List[Char], String](_.mkString, _.toList)
  val int = Iso[String, BigInt](s => \/.fromTryCatchNonFatal(BigInt(s)).toOption, _.toString.some)
  def left[A, B]  = Iso[A, A \/ B](a => Some(-\/(a)), _.swap.toOption)
  def right[A, B] = Iso[B, A \/ B](b => Some(\/-(b)), _.toOption)
  def none[A] = Iso[Unit, Option[A]](_ => None, _.fold[Option[Unit]](Some(()))(_ => None))
  def some[A] = Iso[A, Option[A]](a => Some(Some(a)), identity)
  def listMap[A,B] = Iso.total[List[(A,B)], ListMap[A,B]](ListMap(_: _*), _.toList)
  def vector[A] = Iso.total[List[A], Vector[A]](_.toVector, _.toList)
}

trait IsoFunctor[F[_]] {
  def map[A, B](p: F[A], iso: Iso[A, B]): F[B]
}

trait ProductFunctor[F[_]] {
  def and[A, B](fa: F[A], fb: => F[B]): F[(A, B)]
}

trait Alternative[F[_]] {
  def or[A](f1: F[A], f2: => F[A]): F[A]
}
