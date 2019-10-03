package com.boopboopbeepboop.step

import com.boopboopbeepboop.Tztr.{Rename, Step}

case class SourceStep[A](
  t: () => A,
  name: Option[String] = None
) extends Step[A] {

  def resolve() = t()
  override def trace(): Seq[Seq[Step[_]]] = Seq(Seq(this))
  override def toString = s"S[${name.getOrElse("-")}]"
}

object SourceStep {
  implicit def sourceRename[B]: Rename[SourceStep[B]] = {
    (a: SourceStep[B], newName: String) => a.copy(name = Some(newName))
  }
}