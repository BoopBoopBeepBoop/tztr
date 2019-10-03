package com.boopboopbeepboop

import com.boopboopbeepboop.Tztr.{Rename, Step}

import scala.util.Try

case class Assertion[A](
  prev: Step[A],
  toAssert: A => Unit,
  name: Option[String] = None
) extends Step[Try[A]] {

  def resolve() = {
    val resolved = prev.resolve()
    Try {
      toAssert(resolved)
      resolved
    }
  }

  override def trace(): Seq[Seq[Step[_]]] = prev.trace().map(this +: _)
  override def toString = s"Assertion[${name.getOrElse("-")}]"
}

object Assertion {
  implicit def assertionRename[A]: Rename[Assertion[A]] = {
    (a: Assertion[A], newName: String) => a.copy(name = Some(newName))
  }
}