package com.boopboopbeepboop

import com.boopboopbeepboop.Tztr.{Rename, Step}

case class Join[A1, A2](
  left: Step[A1],
  right: Step[A2],
  name: Option[String] = None
) extends Step[(A1, A2)] {

  override def resolve(): (A1, A2) = {
    (left.resolve(), right.resolve())
  }

  override def trace(): Seq[Seq[Step[_]]] = left.trace().map(this +: _) ++ right.trace().map(this +: _)

  override def toString: String = s"Join[${name.getOrElse("-")}]"
}

object Join {
  implicit def joinRename[A1, A2]: Rename[Join[A1, A2]] = {
    (a: Join[A1, A2], newName: String) => a.copy(name = Some(newName))
  }
}
