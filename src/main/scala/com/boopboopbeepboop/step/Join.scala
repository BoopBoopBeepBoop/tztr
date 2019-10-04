package com.boopboopbeepboop.step

import com.boopboopbeepboop.Step
import com.boopboopbeepboop.Tztr.Rename

case class Join[A1, A2](
  left: Step[A1],
  right: Step[A2],
  name: Option[String] = None
) extends Step[(A1, A2)] {
  implicit val self: Step[_] = this

  override def resolve() = {
    for {
      l <- left.resolve()
      r <- right.resolve()
    } yield (l, r)
  }

  override def trace(): Seq[Seq[Step[_]]] = left.trace().map(this +: _) ++ right.trace().map(this +: _)

  override def toString: String = s"Join[${name.getOrElse("-")}]"

  override def visit(f: Step[_] => Unit): Unit = {
    f(this)
    left.visit(f)
    right.visit(f)
  }
}

object Join {
  implicit def joinRename[A1, A2]: Rename[Join[A1, A2]] = {
    (a: Join[A1, A2], newName: String) => a.copy(name = Some(newName))
  }
}
