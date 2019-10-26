package com.boopboopbeepboop.step

import com.boopboopbeepboop.Step
import com.boopboopbeepboop.Tztr.Rename

case class Assertion[A](
  prev: Step[A],
  toAssert: A => Unit,
  name: Option[String] = None
) extends Step[A] {
  implicit val self: Step[A] = this

  def resolve() = {
    prev.resolve().map { resolved =>
      toAssert(resolved)
      resolved
    }
  }

  override def trace(): Seq[Seq[Step[_]]] = prev.trace().map(this +: _)
  override def toString = s"Assertion(id:$id)[${name.getOrElse("-")}]"

  override def visit(f: Step[_] => Unit): Unit = {
    f(this)
    prev.visit(f)
  }

  // no need to cleanup assertions
  override val cleanupf: Option[A => Unit] = None
}

object Assertion {
  implicit def assertionRename[A]: Rename[Assertion[A]] = {
    (a: Assertion[A], newName: String) => a.copy(name = Some(newName))
  }
}