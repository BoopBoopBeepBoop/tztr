package com.boopboopbeepboop

import com.boopboopbeepboop.step.{Assertion, CacheDecorator, SourceStep}

object Tztr {

  def newContext: Context = new DefaultContext()
  def source[A](t: => A) = new SourceStep[A](() => t)

  case class TestPlan(tests: Seq[Test]) {
    override def toString: String = s"TestPlan(\n  " + tests.map(_.toString).mkString("\n  ") + "\n)"
  }
  case class Test(seq: Seq[Step[_]]) {
    override def toString: String = s"${seq.map(_.toString).mkString(" -> ")}"
  }

  trait Context {
    def += (a: Assertion[_]): Unit
    def trace(): TestPlan
    def resolve(): TestSummary
  }

  case class TestSummary(numSuccess: Int, numFailed: Int, testDetailList: Seq[TestDetail])
  case class TestDetail(assertionNames: Seq[String], failure: Option[Throwable])

  implicit class SummaryExt(testSummary: TestSummary) {
    def print(): TestSummary = {
      println(s"Test Summary: { success: ${testSummary.numSuccess}, failures: ${testSummary.numFailed} }")
      testSummary.testDetailList.foreach { detail =>
        val passed = detail.failure.isEmpty
        detail.assertionNames.foreach { name =>
          println(s"  [${ if (passed) "PASS" else "FAIL" }] - $name")
        }
      }

      if (testSummary.numFailed > 0) {
        println()
        println("Failure Detail:")
        testSummary.testDetailList.filter(_.failure.isDefined).foreach { detail =>
          detail.assertionNames.foreach { name =>
            println(s"$name:")
            detail.failure.foreach(err => err.printStackTrace(System.out))
          }
        }
      }
      testSummary
    }

    def throwOnFailure(): TestSummary = {
      if(testSummary.numFailed > 0) throw new RuntimeException("There are failed tests!")
      testSummary
    }
  }

  trait Rename[S] {
    def rename(a: S, newName: String): S
  }

  implicit class RenameOps[S](a: S)(implicit ev: Rename[S]) {
    def named(newName: String): S = ev.rename(a, newName)
  }

  trait Cleanup[A, S[Q <: A] <: Step[Q]] {
    def cleanup(step: S[A], f: A => Unit): S[A]
  }

  implicit class CleanupOps[A, S[Q <: A] <: Step[Q]](step: S[A])(implicit ev: Cleanup[A, S]) {
    def cleanup(f: A => Unit): S[A] = ev.cleanup(step, f)
  }

  trait CanBeDirtied {
    // returns whether to continue
    def dirty(): Boolean
  }

  // It appears that the [Q] needs to be here to create the equivalence (in the compiler's eyes) between S[Q] and
  // Step[Q]. Using underbars doesn't work, and referencing A in the Step[] definition (which is what I tried initially)
  // causes A to resolve as Nothing
  implicit class CacheableOps[A, S[Q] <: Step[Q]](step: S[A]) {
    def cache: CacheDecorator[A, S[A]] = new CacheDecorator[A, S[A]](step)
  }
}
