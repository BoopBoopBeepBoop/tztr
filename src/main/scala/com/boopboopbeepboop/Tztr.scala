package com.boopboopbeepboop

import scala.util.{Failure, Success}

object Tztr {

  def newContext: Context = new DefaultContext()

  def source[A](t: => A) = new SourceStep[A](() => t)

  // tranformations of steps
  def pure[A](step: Step[A]): Step[A] = step.pure
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
    def resolve(): Unit
  }

  class DefaultContext() extends Context {
    var assertions = Seq.empty[Assertion[_]]

    override def += (a: Assertion[_]): Unit = {

      // yeah it's an append. Revise when I do better data structures for this graph.
      assertions ++= a.trace().flatten.collect { case q: Assertion[_] => q }
    }
    override def resolve(): Unit = {
      assertions.map(_.resolve()).foreach {
        case Success(_) => //
        case Failure(e) => throw new RuntimeException(e)
      }
    }

    override def trace(): TestPlan = {
      val tests = assertions.flatMap(_.trace()).map(Test)
      TestPlan(tests)
    }
  }

  trait Step[A] {
    def map[B](f: A => B): Transform[A, B] = new Transform[A, B](f, this)
    def flatMap[B](f: A => Step[B]): Transform[A, B] = new Transform[A, B](f(_).resolve(), this)

    def join[B](other: Step[B]): Join[A, B] = new Join[A, B](this, other)

    def pure: PureStepDecorator[A, Step[A]] = new PureStepDecorator(this)

    def resolve(): A
    def trace(): Seq[Seq[Step[_]]]

    def assert(toAssert: A => Unit): Assertion[A] = Assertion(this, toAssert)
  }

  trait Rename[A] {
    def rename(a: A, newName: String): A
  }

  implicit class RenameOps[A](a: A)(implicit ev: Rename[A]) {
    def named(newName: String) = ev.rename(a, newName)
  }
}
