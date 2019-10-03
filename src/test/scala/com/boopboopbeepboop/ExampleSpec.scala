package com.boopboopbeepboop

import com.boopboopbeepboop.Tztr._
import org.scalatest._

class ExampleSpec extends FunSpec with Matchers {

  val str1 = source { "foo" } named "foo source" pure
  val str2 = source { "bar" } named "bar source" pure

  val out = (str1 join str2).map { case (a, b) => a + b } named "concatenate strings"

  describe("Testgen") {
    it("runs tests") {
      val ctx = newContext

      ctx += str2.assert(_ shouldEqual "bar")

      val barAssert = out.assert(_ should include ("bar")) named "include bar"
      val fooAssert = out.assert(_ should include ("foo")) named "include foo"

      ctx += (barAssert join fooAssert join out)
          .assert(_._2 shouldEqual "foobar") named "equals foobar"

      // example with multiple asserts
      ctx += out.assert { result =>
        result should include("foo")
        result.length shouldEqual 6
      }.named("includes foo, and length 6")

      println(ctx.trace())

      ctx.resolve()
    }
  }
}