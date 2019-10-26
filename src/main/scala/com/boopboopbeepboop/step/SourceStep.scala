package com.boopboopbeepboop.step

import com.boopboopbeepboop.Tztr.{Cleanup, Rename}
import com.boopboopbeepboop.{Resolution, Step}

case class SourceStep[A](
  protected val t: () => A,
  protected val name: Option[String] = None,
  protected val cleanupf: Option[A => Unit] = None
) extends Step[A] {
  implicit val self: Step[A] = this

  def resolve() = Resolution(t)

  override def trace(): Seq[Seq[Step[_]]] = Seq(Seq(this))
  override def visit(f: Step[_] => Unit): Unit = f(this)
  override def toString = s"Source(id:$id)[${name.getOrElse("-")}]"
}

object SourceStep {
  implicit def sourceRename[B]: Rename[SourceStep[B]] = {
    (a: SourceStep[B], newName: String) => a.copy(name = Some(newName))
  }

  implicit def sourceCleanup[A]: Cleanup[A, SourceStep] = {
    (a: SourceStep[A], f: A => Unit) => a.copy(cleanupf = Some(f))
  }
}