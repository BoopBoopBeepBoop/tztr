package com.boopboopbeepboop.util

// alright, so I don't know that it's acyclic, but I sure hope it is.
case class Dag[T](node: T, parents: Seq[Dag[T]] = Nil)

object Dag {
  object ShouldContinue {
    implicit def toBoolean(sc: ShouldContinue): Boolean = sc match {
      case Continue => true
      case Halt => false
    }
  }
  trait ShouldContinue
  case object Continue extends ShouldContinue
  case object Halt extends ShouldContinue
  case class DagChange[+T](newNode: T, continue: ShouldContinue = Continue)

  def visit[T](dag: Dag[T], visited: Set[T] = Set.empty[T])(f: T => Boolean): Set[T] = {
    val seenAlready = visited.contains(dag.node)

    if(!seenAlready && f(dag.node)) {
      dag.parents.foldLeft(visited + dag.node) { (v, parent) => visit(parent, v)(f) }
    } else visited + dag.node
  }

  def transform[T](dag: Dag[T], transformed: Set[T] = Set.empty[T])(f: T => DagChange[T]): (Dag[T], Set[T]) = {
    val seenAlready = transformed.contains(dag.node)

    val evaluated = f(dag.node)
    val newNode = evaluated.newNode

    if(!seenAlready) {
      if (evaluated.continue) {
        val (visited, newParents) = dag.parents
          .foldLeft((transformed, Seq.empty[Dag[T]])) { (v, parent) =>
            val (newDag, newTransformed) = transform(parent, v._1)(f)
            (newTransformed, v._2 :+ newDag)
          }

        (Dag(newNode, newParents), visited)
      } else {
        // if Halt, transform node, but do not proceed
        (Dag(newNode, dag.parents), transformed)
      }
    } else {
      (dag, transformed)
    }
  }
}