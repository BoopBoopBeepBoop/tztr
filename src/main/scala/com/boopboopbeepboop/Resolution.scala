package com.boopboopbeepboop

import com.boopboopbeepboop.util.Dag

import scala.util.Failure
import scala.util.control.NonFatal

case class RunStep[A](step: Step[A], level: Int, value: Option[A] = None) {
  override def toString: String = s"$step"
}

object Resolution {
  def apply[A](f: () => A)(implicit current: Step[A]): Resolution[A] = {
    val next = try {
      Right(f())
    } catch {
      case NonFatal(e) =>
        Left((Failure(e), current))
    }
    val nextStep = RunStep[A](current, 0, next.right.toOption)
    Resolution(next, Dag(nextStep, Nil), 1)
  }
}

case class Resolution[A](
  item: Either[(Failure[_], Step[_]), A],
  runDag: Dag[RunStep[_]],
  level: Int = 0
) {

  def map[B](f: A => B)(implicit current: Step[B]): Resolution[B] = {
    val next = item.flatMap[(Failure[_], Step[_]), B] { i =>
      try {
        Right(f(i))
      } catch {
        case NonFatal(e) =>
          Left((Failure(e), current))
      }
    }
    val nextStep = RunStep[B](current, level, next.right.toOption)
    Resolution(next, Dag(nextStep, Seq(runDag)), level + 1)

  }

  def join[B](b: Resolution[B])(implicit current: Step[(A, B)]): Resolution[(A, B)] = {
    def resolve(value: Either[(Failure[_], Step[_]), (A, B)], nextStep: RunStep[(A, B)]) = {
      Resolution(value, Dag(nextStep, Seq(runDag, b.runDag)), Math.max(level, b.level) + 1)
    }

    (item, b.item) match {
      case (Right(i), Right(j)) =>
        resolve(Right((i, j)), RunStep[(A, B)](current, level, Some((i, j))))

      case (Left(failure), _) => resolve(Left(failure), RunStep(current, level))
      case (_, Left(failure)) => resolve(Left(failure), RunStep(current, level))
    }
  }

  override def toString: String = {
    s"  [${item.fold(_ => "FAIL", _ => "PASS")}] Test[]" +
      item.fold( {
        case (e, step) =>
          s"\n      > Failed at $step with exception: ${e.exception}" // TODO: add exception trace
      },
        _ => ""
      )
  }
}