package com.boopboopbeepboop.step

import com.boopboopbeepboop.Tztr.Rename
import com.boopboopbeepboop.{Resolution, Step}

case class SourceStep[A](
  t: () => A,
  name: Option[String] = None
) extends Step[A] {
  implicit val self: Step[_] = this

  def resolve() = Resolution(Right(None), Nil).map(_ => t())

  override def trace(): Seq[Seq[Step[_]]] = Seq(Seq(this))
  override def visit(f: Step[_] => Unit): Unit = f(this)
  override def toString = s"S[${name.getOrElse("-")}]"
}

object SourceStep {
  implicit def sourceRename[B]: Rename[SourceStep[B]] = {
    (a: SourceStep[B], newName: String) => a.copy(name = Some(newName))
  }
}