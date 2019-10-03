package com.boopboopbeepboop

import scala.util.{Failure, Success, Try}

object Tztr {

  def newContext: Context = new DefaultContext()

  def source[A](t: => A) = new SourceStep[A](t)
  def pureSource[A](t: => A): Step[A] = new SourceStep[A](t).pure
  def pure[A](step: Step[A]): Step[A] = step.pure

  def resolve(implicit context: Context) = {
    context.resolve()
  }

  trait Context {
    def addAssertion(a: Assertion[_]): Unit
    def resolve(): Unit
  }
  class DefaultContext() extends Context {
    var assertions = Seq.empty[Assertion[_]]

    override def addAssertion(a: Assertion[_]): Unit = assertions :+= a // yeah it's an append. Sue me
    override def resolve(): Unit = {
      assertions.map(_.resolve()).foreach {
        case Success(_) => //
        case Failure(e) => throw new RuntimeException(e)
      }
    }
  }

  trait Step[A] {
    var downstream = Set.empty[Transform[A, _]]

    def map[B](f: A => B): Step[B] = {
      new Transform[A, B](f, this)
    }
    def flatMap[B](f: A => Step[B]): Step[B] = {
      new Transform[A, B](f(_).resolve(), this)
    }
    def pure: Step[A] = new PureStepDecorator(this)
    def resolve(): A
    def assert(toAssert: A => Unit)(implicit context: Context): Assertion[A] = {
      new Assertion(this, toAssert)
    }

    protected def register(t: Transform[A, _]) = downstream += t
  }

  class PureStepDecorator[A](wrapped: Step[A]) extends Step[A] {
    lazy val cached = wrapped.resolve()
    override def resolve(): A = cached
  }

  class SourceStep[A](
    t: => A
  ) extends Step[A] {

    def resolve() = t
  }

  class Transform[A, B](
    t: A => B,
    prev: Step[A]
  ) extends Step[B] {

    def resolve() = t(prev.resolve())
  }

  class Assertion[A](prev: Step[A], toAssert: A => Unit)(implicit context: Context) {
    context.addAssertion(this)

    def resolve() = {
      val resolved = prev.resolve()
      Try(toAssert(resolved))
    }
  }
}
