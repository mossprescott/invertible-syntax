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

import scalaz._, Scalaz._

final case class Iso[A, B] (app: A => Option[B], unapp: B => Option[A]) {
  // def <>[F[_]](fa: F[A]):

  def >>>[C](that: Iso[B, C]) = Iso[A, C](
    a => app(a).flatMap(that.app),
    c => that.unapp(c).flatMap(unapp))

  def <|>(that: Iso[A, B]): Iso[A, B] = Iso(
    a => this.app(a).orElse(that.app(a)),
    b => this.unapp(b).orElse(that.unapp(b)))

  def inverse: Iso[B, A] = Iso(unapp, app)

  def ***[A2, B2](that: Iso[A2, B2]): Iso[(A, A2), (B, B2)] = Iso(
    { case (a, a2) => (this.app(a) |@| that.app(a2))(_ -> _) },
    { case (b, b2) => (this.unapp(b) |@| that.unapp(b2))(_ -> _) })
}

object Iso {
  def total[A, B](fa: A => B, fb: B => A): Iso[A, B] = Iso(a => Some(fa(a)), b => Some(fb(b)))

  def iso[A, B](pa: PartialFunction[A, B], pb: PartialFunction[B, A]): Iso[A, B] = Iso(pa.lift, pb.lift)

  def id[A] = Iso[A, A](Some.apply, Some.apply)

  // Combinators:

  def ignore[A](a: A) = Iso[A, Unit](_ => Some(()), _ => Some(a))

  // TODO: several more combinators.

  def associate[A, B, C] = Iso.iso[(A, (B, C)), ((A, B), C)](
    { case (a, (b, c)) => ((a, b), c) },
    { case ((a, b), c) => (a, (b, c)) })

  def commute[A, B] = Iso.iso[(A, B), (B, A)](
    { case (a, b) => (b, a) },
    { case (b, a) => (a, b) })

  def unit[A] = Iso.iso[A, (A, Unit)](
    { case a => (a, ()) },
    { case (a, ()) => a })

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
    def driver(step: A => Option[A], state: A): A = step(state) match {
      case Some(state1) => driver(step, state1)
      case None => state
    }
    Iso[A, A](
      a => Some(driver(step.app, a)),
      a => Some(driver(step.unapp, a)))
  }

  // A gross hack for subtyping:
  def widen[A: Manifest, B >: A] = Iso[A, B](
    Some.apply,
    b => if (manifest[A].runtimeClass.isAssignableFrom(b.getClass)) Some(b.asInstanceOf[A]) else None)

  // Derived:

  def foldl[A, B](iso: Iso[(A, B), A]): Iso[(A, List[B]), A] = {
    def step: Iso[(A, List[B]), (A, List[B])] = {
      val i1: Iso[(A, List[B]), (A, (B, List[B]))] = id *** Iso.cons[B].inverse
      val i2: Iso[(A, (B, List[B])), ((A, B), List[B])] = Iso.associate
      val i3: Iso[((A, B), List[B]), (A, List[B])] = iso *** id
      i1 >>> i2 >>> i3
    }
    Iso.iterate(step) >>> (id *** Iso.nil.inverse) >>> Iso.unit.inverse
  }


  // Constructors:

  def nil[A] = Iso.iso[Unit, List[A]](
    { case () => Nil },
    { case Nil => () })
  def cons[A] = Iso.iso[(A, List[A]), List[A]](
    { case (x, xs) => x :: xs },
    { case x :: xs => (x, xs) })
  def listCases[A] = Iso.iso[Unit \/ (A, List[A]), List[A]](
    { case -\/(()) => Nil;     case \/-((x, xs)) => x :: xs },
    { case Nil     => -\/(()); case x :: xs      => \/-((x, xs)) })
  val chars = Iso.total[List[Char], String](_.mkString, _.toList)
  def left[A, B]  = Iso[A, A \/ B](a => Some(-\/(a)), _.swap.toOption)
  def right[A, B] = Iso[B, A \/ B](b => Some(\/-(b)), _.toOption)
  def none[A] = Iso[Unit, Option[A]](_ => None, _.fold[Option[Unit]](Some(()))(_ => None))
  def some[A] = Iso[A, Option[A]](a => Some(Some(a)), identity)
}

trait IsoFunctor[F[_]] {
  // NB: Scala doesn't allow <$>
  // def <>[A, B](iso: Iso[A, B]): F[A] => F[B]
  def <>[A, B](iso: Iso[A, B], p: F[A]): F[B]
}

trait ProductFunctor[F[_]] {
  def <*>[A, B](fa: F[A], fb: => F[B]): F[(A, B)]
}

trait Alternative[F[_]] {
  def <|>[A](f1: F[A], f2: => F[A]): F[A]
}
