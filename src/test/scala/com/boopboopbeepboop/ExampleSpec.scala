package com.boopboopbeepboop

import com.boopboopbeepboop.Tztr._
import org.scalatest._

class ExampleSpec extends FunSpec with Matchers {

  val str1 = source { "foo" } named "foo source" cache
  val str2 = source { "bar" } named "bar source" cache

  val out = (str1 join str2).map { case (a, b) => a + b } named "concatenate strings" cache

  describe("Testgen") {
    it("runs tests") {
      val ctx = newContext

      ctx += str2.assert("simple assert") { _ shouldEqual "bar" }

      // In reality, you'd put all these asserts in one block. I'm separating them to test the flow

      val barAssert = out.assert("include bar")(_ should include ("bar"))
      val fooAssert = out.assert("include foo")(_ should include ("foo"))

      ctx += (barAssert join fooAssert join out).assert("equals foobar") {
        _._2 shouldEqual "foobar"
      }

      // example with multiple asserts

      ctx += out.assert("includes foo, and length 6") { result =>
        result should include("foo")
        result.length shouldEqual 6
      }

      ctx.resolve().print().throwOnFailure()
    }
  }
}