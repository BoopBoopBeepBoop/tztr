package com.boopboopbeepboop.step

import com.boopboopbeepboop.{Resolution, Step}
import com.boopboopbeepboop.Tztr.{CanBeDirtied, Rename}

case class CacheDecorator[A, Wrapping <: Step[A]](
  wrapped: Wrapping,
  dirtyOnAssertFailed: Boolean = false
) extends Step[A] with CanBeDirtied {
  implicit val self: Step[_] = this

  private var cached: Option[Resolution[A]] = None

  override def resolve(): Resolution[A] = {
    val result =
      cached
        .map(a => a.copy(runSoFar = Seq(a.runSoFar.last)))
        .getOrElse(wrapped.resolve())

    cached = Some(result)
    result
  }
  override def trace(): Seq[Seq[Step[_]]] = wrapped.trace().map { this +: _ }
  override def toString: String = s"Cache"

  def dirtyOnFailure: CacheDecorator[A, Wrapping] = {
    this.copy(dirtyOnAssertFailed = true)
  }

  override def dirty(): Unit = {
    // safe destruction here?

    cached.map { found =>
      // do something to tear it down safely
    }

    cached = None
  }

  override def visit(f: Step[_] => Unit): Unit = {
    f(this)
    wrapped.visit(f)
  }
}

object CacheDecorator {
  implicit def pureRename[A, B <: Step[A]](implicit ev: Rename[B]): Rename[CacheDecorator[A, B]] = {
    (a: CacheDecorator[A, B], newName: String) => {
      new CacheDecorator[A, B](ev.rename(a.wrapped, newName))
    }
  }
}