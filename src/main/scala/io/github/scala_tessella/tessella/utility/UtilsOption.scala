package io.github.scala_tessella.tessella
package utility

import scala.collection.immutable.SeqOps

/** Useful additional methods for collections of `Option` */
object UtilsOption:

  extension[A, CC[B] <: SeqOps[B, CC, CC[B]]] (options: CC[Option[A]])

    private def map2[B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
//      a.flatMap(x => b.map(y => f(x, y)))
      for
        x <- a
        c <- b.map(f(x, _))
      yield
        c

    private def emptied: CC[A] =
      options.flatten.take(0)

    /** Option of a sequence */
    def sequence: Option[CC[A]] =
      options.foldRight(Option(emptied))((a, b) => map2(a, b)(_ +: _))

  extension[A, B] (methods: List[B => Option[A]])

    /** Gets the result from the first defined method */
    def firstDefined(from: B): Option[A] =
      methods.iterator.map(_(from)).collectFirst { case Some(a) => a }

  /** A collection of `Option` of given type */
  type Options[A] = Vector[Option[A]]

  extension[A] (vector: Vector[A])

    /** Wraps all as `Option` */
    def toOptionsVector: Options[A] =
      vector.map(Option(_))

  extension[A] (options: Options[A])

    /** Checks sequentially if each element is equal to the corresponding where both are defined */
    def equalsWhereDefined(other: Options[A]): Boolean =
      options.sizeIs == other.size && options.lazyZip(other).forall({
        case (Some(elem1), Some(elem2)) => elem1 == elem2
        case _                          => true
    })
