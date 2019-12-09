package com.boopboopbeepboop

import com.boopboopbeepboop.Tztr._
import com.boopboopbeepboop.step.SourceStep._
import org.scalatest._

object SimpleResource {
  var seq = 0
  def nextId(): Int = { seq += 1; seq }
}

class SimpleResource() {
  val id = SimpleResource.nextId()
  var numTimesTornDown: Int = 0

  def teardown(): Unit = {
    numTimesTornDown += 1
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[SimpleResource]

  override def equals(other: Any): Boolean = other match {
    case that: SimpleResource =>
      (that canEqual this) &&
        id == that.id
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(id)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

class ResourceCleanupSpec extends FunSpec with Matchers {

  def simpleCleanup(s: SimpleResource) = s.teardown()


  val simpleRes1 = source(new SimpleResource()).named("simpleRes1").cleanup(simpleCleanup).cache
  val simpleRes2 = source(new SimpleResource()).named("simpleRes2").cleanup(simpleCleanup).cache
  val simpleRes3 = source(new SimpleResource()).named("simpleRes3").cleanup(simpleCleanup).cache

  var resources: Seq[SimpleResource] = Nil

  val joined = simpleRes1 join simpleRes2 join simpleRes3 map { case ((one, two), three) =>
    if (resources.isEmpty) {
      // grab a handle to them
      resources = Seq(one, two, three)
    }
    Seq(one, two, three)
  }


  describe("Testgen") {
    it("Caches items and tears them down once") {
      val ctx = newContext

      ctx += joined.assert() { res =>
        // no need to assert anything on the first pass
        res shouldEqual resources
      }

      ctx += joined.assert() { res =>
        // make sure we got the same resources
        res shouldEqual resources
      }

      val summary = ctx.resolve()
      summary.print()
      summary.numSuccess shouldEqual 2
      summary.numFailed shouldEqual 0

      resources.map { _.numTimesTornDown } shouldEqual Seq(1, 1, 1)
    }

    it("Caches some but tears down others repeatedly") {
      val tornDown = source(new SimpleResource()).named("simpleRes1").cleanup(simpleCleanup)
      val plain1 = source(new SimpleResource()).named("simpleRes2").cleanup(simpleCleanup)
      val plain2 = source(new SimpleResource()).named("simpleRes3").cleanup(simpleCleanup)

      val allTogether = (plain1 join plain2).cache join tornDown


      val ctx = newContext

      var resources1: Seq[SimpleResource] = Nil
      var resources2: Seq[SimpleResource] = Nil

      ctx += allTogether.assert("dummy assert 1") { case ((a, b), c) =>
        resources1 = Seq(a, b, c)
        "foo" shouldEqual "foo"
      }

      ctx += allTogether.assert("dummy assert 2") { case ((a, b), c) =>
        resources2 = Seq(a, b, c)
        "foo" shouldEqual "foo"
      }

      val summary = ctx.resolve()
      summary.print()
      summary.numSuccess shouldEqual 2
      summary.numFailed shouldEqual 0

      resources2(0).numTimesTornDown shouldEqual 1
      resources2(1).numTimesTornDown shouldEqual 1
      resources2(2).numTimesTornDown shouldEqual 1
      resources2(2) shouldNot equal(resources1(2))
    }
  }
}