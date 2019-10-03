package com.boopboopbeepboop

import com.boopboopbeepboop.Tztr.{Rename, Step}

class PureStepDecorator[A, Wrapping <: Step[A]](val wrapped: Wrapping) extends Step[A] {
  lazy val cached = wrapped.resolve()
  override def resolve(): A = cached
  override def trace(): Seq[Seq[Step[_]]] = wrapped.trace().map { this +: _ }
  override def toString: String = s"PureCache"
}

object PureStepDecorator {
  implicit def pureRename[A, B <: Step[A]](implicit ev: Rename[B]): Rename[PureStepDecorator[A, B]] = {
    (a: PureStepDecorator[A, B], newName: String) => {
      new PureStepDecorator[A, B](ev.rename(a.wrapped, newName))
    }
  }
}