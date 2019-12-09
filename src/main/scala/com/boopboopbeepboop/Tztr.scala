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

  case class TestSummary(numSuccess: Int, numFailed: Int)

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
