package com.boopboopbeepboop

import com.boopboopbeepboop.Tztr.{Context, Test, TestPlan, TestSummary}
import com.boopboopbeepboop.step.{Assertion, CacheDecorator}

import scala.util.{Failure, Success}

class DefaultContext() extends Context {
  var assertions = Seq.empty[Assertion[_]]

  override def += (a: Assertion[_]): Unit = {
    // yeah it's an append. Revise when I do better data structures for this graph.
    assertions :+= a
  }

  override def resolve(): TestSummary = {
    val results = assertions.map { a =>
      val res = a.resolve()
      println(res)

      if (res.item.isLeft) {
        res.runSoFar.last.step.visit {
          case c: CacheDecorator[_, _] if c.dirtyOnAssertFailed =>
            c.dirty()
          case _ =>
        }
      }

      if (res.item.isRight) (1, 0) else (0, 1)
    }

    val (numSuccess, numFailure) = results.reduce((a, b) => (a._1 + b._1, a._2 + b._2))
    TestSummary(numSuccess, numFailure)
  }

  override def trace(): TestPlan = {
    val tests = assertions.flatMap(_.trace()).map(Test)
    TestPlan(tests)
  }
}