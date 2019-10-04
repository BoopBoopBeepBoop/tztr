package com.boopboopbeepboop

import scala.util.Failure
import scala.util.control.NonFatal

case class RunStep(step: Step[_], level: Int)

case class Resolution[A](
  item: Either[(Failure[_], Step[_]), A],
  runSoFar: Seq[RunStep],
  level: Int = 0
) {
  def map[B](f: A => B)(implicit current: Step[_]): Resolution[B] = {
    val next = item.flatMap[(Failure[_], Step[_]), B] { i =>
      try {
        Right(f(i))
      } catch {
        case NonFatal(e) =>
          Left((Failure(e), current))
      }
    }
    Resolution(next, runSoFar :+ RunStep(current, level), level + 1)

  }

  def flatMap[B](f: A => Resolution[B])(implicit current: Step[_]): Resolution[B] = {
    val thing = map(f)

    thing.item.fold(
      failure => {
        Resolution(Left(failure), runSoFar ++ thing.runSoFar, level + 1)
      },
      {
        case Resolution(Left(failure), runList, otherLevel) =>
          Resolution(Left(failure), thing.runSoFar ++ runList, Math.max(level, otherLevel) + 1)
        case Resolution(Right(i), runList, otherLevel) =>
          Resolution(Right(i), thing.runSoFar ++ runList, Math.max(level, otherLevel) + 1)
      }
    )
  }

  override def toString: String = {
    s"  [${item.fold(_ => "FAIL", _ => "PASS")}] Test[${runSoFar.mkString(" -> ")}]" +
      item.fold( {
        case (e, step) =>
          s"\n      > Failed at $step with exception: ${e.exception}" // TODO: add exception trace
      },
        _ => ""
      )
  }
}