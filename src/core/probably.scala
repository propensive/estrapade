/*

    Probably, version 0.1.0. Copyright 2017-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package probably

import gastronomy._
import magnolia._

import scala.util._
import scala.collection.immutable.ListMap
import scala.util.control.NonFatal

import language.dynamics
import language.implicitConversions

sealed trait Outcome(val passed: Boolean):
  def failed: Boolean = !passed
  def debug: String = ""

case class FailsAt(datapoint: Datapoint, count: Int) extends Outcome(false):
  override def debug: String = datapoint.map.map { case (k, v) => s"$k=$v" }.mkString(" ")

case object Passed extends Outcome(true)
case object Mixed extends Outcome(false)

sealed trait Datapoint(val map: Map[String, String])
case object Pass extends Datapoint(Map())

case class Fail(failMap: Map[String, String]) extends Datapoint(failMap)
case class Throws(exception: Throwable, throwMap: Map[String, String]) extends Datapoint(throwMap)
case class ThrowsInCheck(exception: Exception, throwMap: Map[String, String]) extends Datapoint(throwMap)

object Show:
  given Show[Int] = _.toString
  given Show[String] = identity(_)
  given Show[Boolean] = _.toString
  given Show[Long] = _.toString
  given Show[Byte] = _.toString
  given Show[Short] = _.toString
  given Show[Char] = _.toString

trait Show[T]:
  def show(value: T): String

object Showable:
  implicit def show[T: Show](value: T): Showable[T] = Showable[T](value, summon[Show[T]])

case class Showable[T](value: T, show: Show[T]):
  def apply(): String = show.show(value)

case class TestId private[probably](value: String)

class Runner(specifiedTests: Set[TestId] = Set()) extends Dynamic:

  final def runTest(testId: TestId): Boolean = specifiedTests.isEmpty || specifiedTests(testId)

  def applyDynamic[T](method: String)(name: String)(fn: => T): Test { type Type = T } =
    applyDynamicNamed[T]("")("" -> name)(fn)
  
  def applyDynamicNamed[T]
                       (method: String)
                       (name: (String, String), args: (String, Showable[_])*)
                       (fn: => T)
                       : Test { type Type = T } =
    new Test(name._2, args.toMap.map(_ -> _())):
      type Type = T
      def action(): T = fn

  def suite(name: String)(fn: Runner => Unit): Unit =
    val report = new Test(name, Map()) {
      type Type = Report

      def action(): Report =
        val runner = Runner()
        fn(runner)
        runner.report()

    }.check(_.results.forall(_.outcome.passed))

    if report.results.exists(_.outcome.passed) && report.results.exists(_.outcome.failed)
    then synchronized { results = results.updated(name, results(name).copy(outcome = Mixed)) }

    report.results.foreach { result => record(result.copy(indent = result.indent + 1)) }

  abstract class Test(val name: String, map: => Map[String, String]):
    type Type
    
    def id: TestId = TestId(name.digest[Sha256].encoded[Hex].take(6).toLowerCase)
    def action(): Type
    
    def assert(predicate: Type => Boolean): Unit =
      try if runTest(id) then check(predicate) catch case NonFatal(e) => ()
    
    def check(predicate: Type => Boolean): Type =
      val t0 = System.currentTimeMillis()
      val value = Try(action())
      val time = System.currentTimeMillis() - t0

      value match
        case Success(value) =>
          def handler: PartialFunction[Throwable, Datapoint] =
            case e: Exception => ThrowsInCheck(e, map)
          
          val datapoint: Datapoint = try if predicate(value) then Pass else Fail(map) catch handler
          record(this, time, datapoint)
          value

        case Failure(exception) =>
          record(this, time, Throws(exception, map))
          throw exception

  def report(): Report = Report(results.values.to(List))

  protected def record(test: Test, duration: Long, datapoint: Datapoint): Unit = synchronized {
    results = results.updated(test.name, results(test.name).append(test.name, duration, datapoint))
  }

  protected def record(summary: Summary) = synchronized { results = results.updated(summary.name, summary) }
  
  @volatile
  protected var results: Map[String, Summary] = ListMap().withDefault { name =>
    Summary(TestId(name.digest[Sha256].encoded[Hex].take(6).toLowerCase), name, 0, Int.MaxValue, 0L,
        Int.MinValue, Passed)
  }

case class Seed(value: Long):
  def apply(): Long = value
  
  def stream(count: Int): LazyList[Seed] =
    val rnd = java.util.Random(value)
    LazyList.continually(Seed(rnd.nextLong)).take(count)

object Arbitrary:
  def join[T](ctx: CaseClass[Arbitrary, T]): Arbitrary[T] = (seed, n) => ctx.rawConstruct {
    ctx.params.zip(spread(seed, n, ctx.params.size)).zip(seed.stream(ctx.params.size)).map {
      case ((param, i), s) => param.typeclass(s, i)
    } }
  
  val testInts = Vector(0, 1, -1, 2, -2, 42, Int.MaxValue, Int.MinValue, Int.MaxValue - 1, Int.MinValue + 1)

  given Arbitrary[Int] = (seed, n) => testInts.lift(n).getOrElse(seed.stream(n).last.value.toInt)

  val testStrings = Vector("", "a", "z", "\n", "0", "_", "\"", "\'", " ", "abcdefghijklmnopqrstuvwxyz")
  
  given Arbitrary[String] = (seed, n) => testStrings.lift(n).getOrElse {
    val chars = seed.stream(n).last.stream(10).map(_()).map(_.toByte).filter { c => c > 31 && c < 128 }
    String(chars.to(Array), "UTF-8")
  }

  private def spread(seed: Seed, total: Int, count: Int): List[Int] =
    val sample = seed.stream(count).map(_.value.toDouble).map(math.abs(_)).to(List)
    
    sample.tails.foldLeft(List[Int]()) { case (acc, tail) => tail.headOption.fold(acc) { v =>
      (((v/(tail.sum))*(total - acc.sum)) + 0.5).toInt :: acc
    } }

trait Arbitrary[T]:
  def apply(seed: Seed, n: Int): T

object Generate:
  def stream[T](using arbitrary: Arbitrary[T], seed: Seed = Seed(0L)): LazyList[T] =
    LazyList.from(0).map(arbitrary(seed, _))

case class Summary(id: TestId, name: String, count: Int, tmin: Long, ttot: Long, tmax: Long, outcome: Outcome,
                   indent: Int = 0):
  def avg: Double = ttot.toDouble/count/1000.0
  def min: Double = tmin.toDouble/1000.0
  def max: Double = tmax.toDouble/1000.0
  
  def append(test: String, duration: Long, datapoint: Datapoint): Summary =
    Summary(id, name, count + 1, tmin min duration, ttot + duration, tmax max duration, outcome match
      case FailsAt(dp, c) => FailsAt(dp, c)
      case Passed         => datapoint match
        case Pass           => Passed
        case other          => FailsAt(other, count + 1)
    )

case class Report(results: List[Summary])