package com.boopboopbeepboop.step

import com.boopboopbeepboop.Step
import com.boopboopbeepboop.Tztr.Rename

case class Assertion[A](
  prev: Step[A],
  toAssert: A => Unit,
  name: Option[String] = None
) extends Step[A] {
  implicit val self: Step[_] = this

  def resolve() = {
    prev.resolve().map { resolved =>
      toAssert(resolved)
      resolved
    }
  }

  override def trace(): Seq[Seq[Step[_]]] = prev.trace().map(this +: _)
  override def toString = s"Assertion[${name.getOrElse("-")}]"

  override def visit(f: Step[_] => Unit): Unit = {
    f(this)
    prev.visit(f)
  }
}

object Assertion {
  implicit def assertionRename[A]: Rename[Assertion[A]] = {
    (a: Assertion[A], newName: String) => a.copy(name = Some(newName))
  }
}