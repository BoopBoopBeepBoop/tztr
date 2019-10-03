package com.boopboopbeepboop

import com.boopboopbeepboop.Tztr._
import org.scalatest._

class ExampleSpec extends FunSpec with Matchers {

  val str1 = pureSource {
    println("init foo")
    "foo"
  }
  val str2 = pureSource {
    println("init bar")
    "bar"
  }

  val out = pure {
    for {
      s1 <- str1
      s2 <- str2
    } yield {
      println("combine")
      s1 + s2
    }
  }

  describe("Testgen") {
    implicit val ctx: Context = newContext

    str2.assert(_ shouldEqual "bar")

    out.assert(_ should include ("bar"))
    out.assert(_ should include ("foo"))
    out.assert(_ shouldEqual "foobar")

    // example with multiple asserts
    out.assert { result =>
      result should include("foo")
      result.length shouldEqual 6
    }

    resolve
  }
}