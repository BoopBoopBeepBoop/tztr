package com.boopboopbeepboop.step

import com.boopboopbeepboop.Step
import com.boopboopbeepboop.Tztr.Rename

case class Transform[A, B](
  t: A => B,
  prev: Step[A],
  name: Option[String] = None
) extends Step[B] {
  implicit val self: Step[_] = this

  def resolve() = prev.resolve().map(t)

  override def trace(): Seq[Seq[Step[_]]] = prev.trace().map(this +: _)
  override def visit(f: Step[_] => Unit): Unit = {
    f(this)
    prev.visit(f)
  }
  override def toString = s"Transform[${name.getOrElse("-")}]"
}

object Transform {
  implicit def transformRename[A, B]: Rename[Transform[A, B]] = {
    (a: Transform[A, B], newName: String) => a.copy(name = Some(newName))
  }
}