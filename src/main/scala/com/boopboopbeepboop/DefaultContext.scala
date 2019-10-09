package com.boopboopbeepboop

import com.boopboopbeepboop.Tztr.{Context, Test, TestPlan, TestSummary}
import com.boopboopbeepboop.step.{Assertion, CacheDecorator}
import com.boopboopbeepboop.util.Dag
import com.boopboopbeepboop.util.Dag.DagChange

class DefaultContext() extends Context {
  var assertions = Seq.empty[Assertion[_]]

  override def += (a: Assertion[_]): Unit = {
    // yeah it's an append. Revise when I do better data structures for this graph.
    assertions :+= a
  }

  override def resolve(): TestSummary = {
    def dirtyCaches(r: RunStep[_]): Boolean = r match {
      case RunStep(c: CacheDecorator[_, _], _, _) if c.dirtyOnAssertFailed => c.dirty(); true
      case _ => true
    }

    def internalCleanup[T](step: RunStep[T]): DagChange[RunStep[T]] = {
      step.value
        .map { v =>
          val cleanupRes = step.step.performCleanup(v)

          DagChange(
            newNode = if (cleanupRes.destroyed) step.copy(value = None) else step,
            continue = cleanupRes.shouldContinue
          )
        }
        .getOrElse(DagChange(step))
    }

    val results = assertions.map { a =>
      val res = a.resolve()
      println(res)

      Dag.visit(res.runDag) { thing =>
        println(s" ${thing.level}:${thing.step}")
        true
      }

      // if this is a failure, flag caches as dirty before we initiate a cleanup pass
      if (res.item.isLeft) {
        Dag.visit(res.runDag)(dirtyCaches)
      }

      // invoke cleanup on all steps, continuing if they return true
      val (newDag, _) = Dag.transform(res.runDag)(a => internalCleanup(a))
      res.copy(runDag = newDag)
    }


    // after running all tests for this context, dirty all caches and do final cleanup
    results.foldLeft(Set.empty[RunStep[_]]) { (seen, r) =>

      println("FINAL DIRTY")
      Dag.visit(r.runDag) {
        case RunStep(c: CacheDecorator[_, _], _, _) => c.dirty(); true
        case _ => true
      }

      println("FINAL CLEANUP")
      val (_, alreadySeen) = Dag.transform(r.runDag, seen)(a => internalCleanup(a))
      alreadySeen
    }

    val (numSuccess, numFailure) =
      results
        .map { r => r.item.fold(_ => (0, 1), _ => (1, 0)) }
        .reduce { (a, b) => (a._1 + b._1, a._2 + b._2) }

    TestSummary(numSuccess, numFailure)
  }

  override def trace(): TestPlan = {
    val tests = assertions.flatMap(_.trace()).map(Test)
    TestPlan(tests)
  }
}