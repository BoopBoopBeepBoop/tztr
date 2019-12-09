package com.boopboopbeepboop.step

import com.boopboopbeepboop.Tztr.Rename
import com.boopboopbeepboop.{Resolution, Step}

import scala.concurrent.duration.FiniteDuration

case class WaitDecorator[A, Wrapping <: Step[A]](
  wrapped: Wrapping,
  check: A => Boolean,
  asLongAs: A => Boolean = (a: A) => true,
  interval: FiniteDuration,
  maxTime: FiniteDuration,
  protected val cleanupf: Option[A => Unit] = None
) extends Step[A] {
  implicit val self: Step[A] = this

  override def resolve(): Resolution[A] = {
    wrapped.resolve().map { inbound =>
      val startTime = System.currentTimeMillis()
      while (!check(inbound)) {

        val elapsed = System.currentTimeMillis() - startTime
        if (elapsed > maxTime.toMillis) {
          throw new RuntimeException(s"OUT OF TIME ($elapsed millis > $maxTime)")
        }
        if (asLongAs(inbound)) {
          throw new RuntimeException(s"asLongAs condition violated")
        }

        Thread.sleep(interval.toMillis)
        check(inbound)
      }
      inbound
    }
  }

  override def trace(): Seq[Seq[Step[_]]] = wrapped.trace().map { this +: _ }
  override def toString: String = s"Wait(id:$id)"

  override def visit(f: Step[_] => Unit): Unit = {
    f(this)
    wrapped.visit(f)
  }
}

object WaitDecorator {
  implicit def waitRename[A, B <: Step[A]](implicit ev: Rename[B]): Rename[WaitDecorator[A, B]] = {
    (a: WaitDecorator[A, B], newName: String) => {
      a.copy(wrapped = ev.rename(a.wrapped, newName))
    }
  }
}