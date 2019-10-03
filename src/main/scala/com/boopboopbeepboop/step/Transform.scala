package com.boopboopbeepboop.step

import com.boopboopbeepboop.Tztr.{Rename, Step}

case class Transform[A, B](
  t: A => B,
  prev: Step[A],
  name: Option[String] = None
) extends Step[B] {

  def resolve() = t(prev.resolve())

  override def trace(): Seq[Seq[Step[_]]] = prev.trace().map(this +: _)
  override def toString = s"Transform[${name.getOrElse("-")}]"
}

object Transform {
  implicit def transformRename[A, B]: Rename[Transform[A, B]] = {
    (a: Transform[A, B], newName: String) => a.copy(name = Some(newName))
  }
}