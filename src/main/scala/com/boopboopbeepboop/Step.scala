package com.boopboopbeepboop

import com.boopboopbeepboop.step.{Assertion, CacheDecorator, Join, Transform}
import com.boopboopbeepboop.util.Dag.{Continue, ShouldContinue}

case class CleanupResult(destroyed: Boolean, shouldContinue: ShouldContinue = Continue)

trait Step[A] {

  // ====== Common Properties ========
  protected def cleanupf: Option[A => Unit]

  // ====== Transformation Methods =======

  // TODO: move to implicit wrapper in the future?

  def map[B](f: A => B): Transform[A, B] = new Transform[A, B](f, this)
  def join[B](other: Step[B]): Join[A, B] = new Join[A, B](this, other)

  def assert(name: String = "")(toAssert: A => Unit): Assertion[A] = {
    Assertion(
      prev = this,
      toAssert = toAssert,
      name = if (name == "") None else Some(name)
    )
  }

  // ====== Internal Flow =======

  // TODO: make package private?

  def resolve(): Resolution[A]
  def trace(): Seq[Seq[Step[_]]]
  def visit(f: Step[_] => Unit): Unit
  def performCleanup(a: A): CleanupResult = {
    cleanupf.fold(CleanupResult(destroyed = false)) { c =>
      c(a)
      CleanupResult(destroyed = true)
    }
  }
}
