package com.boopboopbeepboop

import com.boopboopbeepboop.step.{Assertion, Join, CacheDecorator, Transform}

trait Step[A] {
  def map[B](f: A => B): Transform[A, B] = new Transform[A, B](f, this)
//  def flatMap[B](f: A => Step[B]): Transform[A, B] = new Transform[A, B](f(_).resolve(), this)

  def join[B](other: Step[B]): Join[A, B] = new Join[A, B](this, other)

  def cache: CacheDecorator[A, Step[A]] = new CacheDecorator(this)

  def resolve(): Resolution[A]
  def trace(): Seq[Seq[Step[_]]]
  def visit(f: Step[_] => Unit): Unit

  def assert(name: String = "")(toAssert: A => Unit): Assertion[A] = {
    Assertion(
      prev = this,
      toAssert = toAssert,
      name = if (name == "") None else Some(name)
    )
  }
}
