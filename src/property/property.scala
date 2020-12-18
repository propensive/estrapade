/*

    Probably, version 0.3.0. Copyright 2017-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package probably

import magnolia._

import language.experimental.macros

object Arbitrary {
  object Seed { def apply(any: Any): Seed = Seed(any.hashCode) }
  case class Seed(value: Long) {
    def apply(): Long = value
    def stream(count: Int): Stream[Seed] = {
      val rnd = new java.util.Random(value)
      Stream.continually(Seed(rnd.nextLong)).take(count)
    }
  }

  type Typeclass[T] = Arbitrary[T]
  implicit def gen[T]: Arbitrary[T] = macro Magnolia.gen[T]

  def combine[T](ctx: CaseClass[Arbitrary, T]): Arbitrary[T] = (seed, n) => ctx.rawConstruct {
    ctx.parameters.zip(spread(seed, n, ctx.parameters.size)).zip(seed.stream(ctx.parameters.size)).map {
      case ((param, i), s) => param.typeclass(s, i)
    } }

  def dispatch[T](ctx: SealedTrait[Arbitrary, T]): Arbitrary[T] =
    (seed, n) => ctx.subtypes(seed.value.toInt%ctx.subtypes.size).typeclass(seed, n)

  val interestingInts = Vector(0, 1, -1, 2, -2, 42, Int.MaxValue, Int.MinValue, Int.MaxValue - 1,
      Int.MinValue + 1, 128, -128, -129, 255, 256, 100)

  implicit val int: Arbitrary[Int] =
    (seed, n) => interestingInts.lift(n).getOrElse(seed.stream(n).last.value.toInt)

  val interestingStrings = Vector("", "a", "z", "\n", "0", "_", "\"", "\'", " ",
      "abcdefghijklmnopqrstuvwxyz")
  
  implicit def string: Arbitrary[String] = (seed, n) => interestingStrings.lift(n).getOrElse {
    val chars = seed.stream(n).last.stream(10).map(_()).map(_.toByte).filter { c => c > 31 && c < 128 }
    new String(chars.to[Array], "UTF-8")
  }

  private[probably] def spread(seed: Seed, total: Int, count: Int): List[Int] = {
    val sample = seed.stream(count).map(_.value.toDouble).map(math.abs(_)).to[List]
    sample.tails.foldLeft(List[Int]()) { case (acc, tail) => tail.headOption.fold(acc) { v =>
      (((v/(tail.sum))*(total - acc.sum)) + 0.5).toInt :: acc
    } }
  }
}

trait Arbitrary[T] { def apply(seed: Arbitrary.Seed, n: Int): T }

object Generate {
  def stream[T](implicit arbitrary: Arbitrary[T], seed: Arbitrary.Seed = Arbitrary.Seed(0L)): Stream[T] =
    Stream.from(0).map(arbitrary(seed, _))
  
  def find[T]
          (count: Int)
          (pred: T => Boolean)
          (implicit arbitrary: Arbitrary[T], seed: Arbitrary.Seed, reducer: Reducer[T])
          : Option[T] = {
    stream[T].take(count).find(!pred(_)).map { init =>
      def reduce(v: T): Option[T] = reducer(v).find(pred(_)).flatMap(reduce(_))

      reduce(init).getOrElse(init)
    }
  }
}

trait Reducer[T] { def apply(value: T): Stream[T] }

object Reducer {
  type Typeclass[T] = Reducer[T]
  implicit def gen[T]: Reducer[T] = macro Magnolia.gen[T]

  def combine[T](ctx: CaseClass[Reducer, T]): Reducer[T] = { value =>
    val streams = ctx.parameters.to[Vector].map { param =>
      param.dereference(value) +: param.typeclass(param.dereference(value))
    }
    
    Stream.from(1).map { (i: Int) => Arbitrary.spread(Arbitrary.Seed(value), i, ctx.parameters.length) }.map { xs =>
      println(xs)
      streams.zip(xs).map { case (stream, idx) => stream.lift(idx) }
    }.takeWhile(_.exists(_.isDefined)).map { elems =>
      ctx.rawConstruct(elems.zip(ctx.parameters).map { case (elem, param) => elem.getOrElse(param.dereference(value)) })
    }
  }

  def dispatch[T](ctx: SealedTrait[Reducer, T]): Reducer[T] =
    v => ctx.dispatch(v) { sub => sub.typeclass(sub.cast(v)) }

  implicit val int: Reducer[Int] = i => Stream(i - 1, i - 2)
  
  implicit val string: Reducer[String] =
    s => Stream(s.take(s.size/2), s.drop(s.size/2)) ++ s.to[Stream].indices.map(s.patch(_, "", 1))
  
  implicit val double: Reducer[Double] = d => Stream(d/2.0, d - d/4, d - d/8, d - d/16, d - d/32)
} 