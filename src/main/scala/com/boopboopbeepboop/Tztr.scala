package com.boopboopbeepboop

import com.boopboopbeepboop.step.{Assertion, SourceStep}

import scala.util.{Failure, Success}

object Tztr {

  def newContext: Context = new DefaultContext()

  def source[A](t: => A) = new SourceStep[A](() => t)

  // tranformations of steps
  def pure[A](step: Step[A]): Step[A] = step.cache
  def name[A, B <: Step[A]](name: String)(step: B)(implicit ev: Rename[B]) = step.named(name)

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

  trait Rename[A] {
    def rename(a: A, newName: String): A
  }

  implicit class RenameOps[A](a: A)(implicit ev: Rename[A]) {
    def named(newName: String) = ev.rename(a, newName)
  }

  trait ProducesDirt[A] {
    def dirties(a: A, other: CanBeDirtied): A
  }

  trait CanBeDirtied {
    def dirty(): Unit
  }

  implicit class DirtyOps[A](a: A)(implicit ev: ProducesDirt[A]) {
    def dirties(other: CanBeDirtied) = ev.dirties(a, other)
  }
}
