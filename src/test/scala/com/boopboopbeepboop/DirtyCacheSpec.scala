package com.boopboopbeepboop

import com.boopboopbeepboop.Tztr._
import org.scalatest._

class DirtyCacheSpec extends FunSpec with Matchers {

  var i = -1
  def increment() = { i += 1; i }

  // increment by 1 each time we execute it
  val number = source(increment())
    .named("source inc on failure")
    .cache
    .dirtyOnFailure

  val dirtiesOnFailure =
    number.map { i =>
      // will throw exception the first time it runs
      if (i < 1) throw new RuntimeException
      i
    }.named("fail < 1")


  describe("Testgen") {
    it("runs tests") {
      val ctx = newContext

      ctx += dirtiesOnFailure.assert("fails i<1 RuntimeException") { i =>
        i shouldEqual 0 // we won't get to this ... exception will be thrown
      }

      ctx += dirtiesOnFailure.assert("fails failed assert") { i =>
        // now i is equal to 1 for this run ...
        i shouldEqual -1
      }

      ctx += dirtiesOnFailure.assert("passes") { i =>
        // now i is equal to 2, should be rebuilt twice
        i shouldEqual 2
      }

      ctx += dirtiesOnFailure.assert("only passes if caching working") { i =>
        // and it should continue to equal 2 for subsequent runs
        i shouldEqual 2
      }

      ctx.resolve() shouldEqual TestSummary(2, 2)
    }
  }
}