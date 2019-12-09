package com.boopboopbeepboop.step

import com.boopboopbeepboop.Step
import com.boopboopbeepboop.Tztr.{Cleanup, Rename}

case class Transform[A, B](
  protected val t: A => B,
  protected val prev: Step[A],
  protected val name: Option[String] = None,
  protected val cleanupf: Option[B => Unit] = None
) extends Step[B] {
  implicit val self: Step[B] = this

  def resolve() = prev.resolve().map(t)

  override def trace(): Seq[Seq[Step[_]]] = prev.trace().map(this +: _)
  override def visit(f: Step[_] => Unit): Unit = {
    f(this)
    prev.visit(f)
  }
  override def toString = s"Transform(id:$id)[${name.getOrElse("-")}]"
}

object Transform {
  implicit def transformRename[A, B]: Rename[Transform[A, B]] = {
    (a: Transform[A, B], newName: String) => a.copy(name = Some(newName))
  }

  implicit def transformCleanup[A, B]: Cleanup[B, ({ type T[B] = Transform[A, B]})#T] = {
    (a: Transform[A, B], f: B => Unit) => a.copy(cleanupf = Some(f))
  }
}