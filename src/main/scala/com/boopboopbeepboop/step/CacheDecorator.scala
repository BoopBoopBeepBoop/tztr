package com.boopboopbeepboop.step

import com.boopboopbeepboop.{CleanupResult, Resolution, Step}
import com.boopboopbeepboop.Tztr.{CanBeDirtied, Cleanup, Rename}
import com.boopboopbeepboop.util.Dag.Halt

case class CacheDecorator[A, Wrapping <: Step[A]](
  wrapped: Wrapping,
  dirtyOnAssertFailed: Boolean = false,
  protected val cleanupf: Option[A => Unit] = None
) extends Step[A] with CanBeDirtied {
  implicit val self: Step[A] = this

  private var cached: Option[Resolution[A]] = None

  override def resolve(): Resolution[A] = {
    val result =
      cached
        .map(a => a)
        .getOrElse(wrapped.resolve())
        .map(identity) // .map(identity) required to add this to the Dag chain

    cached = Some(result)
    result
  }
  override def trace(): Seq[Seq[Step[_]]] = wrapped.trace().map { this +: _ }
  override def toString: String = s"Cache"

  def dirtyOnFailure: CacheDecorator[A, Wrapping] = {
    this.copy(dirtyOnAssertFailed = true)
  }

  // this will just invalidate the cache. Cleanup and destruction occur by the performCleanup cycle
  override def dirty(): Boolean = {
    println("MARKED DIRTY")
    cached = None
    false
  }

  override def performCleanup(a: A): CleanupResult = {
    val defaultResult = CleanupResult(destroyed = false)
    val stopResult = CleanupResult(destroyed = false, Halt)

    println("ATTEMPT CLEANUP")
    def actuallyRun() = {
      cleanupf.fold(defaultResult) { c =>
        c(a)
        CleanupResult(destroyed = true)
      }
    }
    if (cached.isEmpty) actuallyRun() else stopResult
  }

  override def visit(f: Step[_] => Unit): Unit = {
    f(this)
    wrapped.visit(f)
  }
}

object CacheDecorator {
  implicit def cacheRename[A, B <: Step[A]](implicit ev: Rename[B]): Rename[CacheDecorator[A, B]] = {
    (a: CacheDecorator[A, B], newName: String) => {
      new CacheDecorator[A, B](ev.rename(a.wrapped, newName))
    }
  }

  implicit def cacheCleanup[A, Wrapping[Z] <: Step[Z]]: Cleanup[A, ({ type T[Q] = CacheDecorator[Q, Wrapping[Q]]})#T] = {
    (a: CacheDecorator[A, Wrapping[A]], f: A => Unit) => a.copy(cleanupf = Some(f))
  }
}