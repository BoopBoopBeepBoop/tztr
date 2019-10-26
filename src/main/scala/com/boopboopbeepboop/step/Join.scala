package com.boopboopbeepboop.step

import com.boopboopbeepboop.Step
import com.boopboopbeepboop.Tztr.{Cleanup, Rename}

case class Join[A1, A2](
  left: Step[A1],
  right: Step[A2],
  name: Option[String] = None,
  cleanupf: Option[((A1, A2)) => Unit] = None
) extends Step[(A1, A2)] {
  implicit val self: Step[(A1, A2)] = this

  override def resolve() = left.resolve() join right.resolve()

  override def trace(): Seq[Seq[Step[_]]] = left.trace().map(this +: _) ++ right.trace().map(this +: _)

  override def toString: String = s"Join(id:$id)[${name.getOrElse("-")}]"

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

//  implicit def joinCleanup[A1, A2]: Cleanup[(A1, A2), ({ type F[Z <: (A1, A2)] = Join[A1, A2]})#F] = {
//    new Cleanup[(A1, A2), ({ type F[Z <: (A1, A2)] = Join[A1, A2]})#F] {
//      override def cleanup(step: Join[A1, A2], f: ((A1, A2)) => Unit): Join[A1, A2] = step.copy(cleanupf = Some(f))
//    }
//  }
}
