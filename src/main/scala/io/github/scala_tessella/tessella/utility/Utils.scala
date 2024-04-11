package io.github.scala_tessella.tessella
package utility

import scala.annotation.tailrec

/** Useful additional methods for collections */
object Utils:

  extension [A, B](mapped: Map[A, B])

    /** Convert to a new `Map` where each key value pair is inverted
     *
     * @example {{{Map('A' -> 2, 'B' -> 1).invert // Map(2 -> 'A', 1 -> 'B')}}}
     */
    def invert: Map[B, A] =
      mapped.map((key, value) => value -> key)

    /** Convert to a new `Map` where each value is transformed according to a function
     *
     * @param f the function transforming each value
     * @example {{{Map('A' -> 2, 'B' -> 1).mapValues2(_ + 1) // Map('A' -> 3, 'B' -> 2)}}}
     */
    def mapValues2[T](f: B => T): Map[A, T] =
      mapped.view.mapValues(f).toMap

    /** Convert to a new `Map` where each key is transformed according to a function
     *
     * @param f the function transforming each key
     * @example {{{Map(1 -> 'A', 2 -> 'B').mapKeys(_ + 1) // Map(2 -> 'A', 3 -> 'B')}}}
     */
    def mapKeys[T](f: A => T): Map[T, B] =
      mapped.map((key, value) => f(key) -> value)

    /** Convert to a new inverted `Map` where each distinct value is mapped to its keys
     *
     * @return a `Map` mapping each values to its keys.
     * @note is the inverted function of [[Utils.mapByValue]]
     * @example {{{Map('A' -> 1, 'B' -> 1).groupByValues // Map(1 -> List('A', 'B'))}}}
     */
    def groupByValues: Map[B, Iterable[A]] =
      mapped.groupMap((_, value) => value)((key, _) => key)

  extension [A, B](grouped: Map[B, Iterable[A]])

    /** Convert to a new inverted `Map` where each grouped value is a key
     *
     * @return a `Map` mapping each element in the values to its key.
     * @note is the inverted function of [[Utils.groupByValues]]
     * @example {{{Map(1 -> List('A', 'B')).mapByValue // Map('A' -> 1, 'B' -> 1)}}}
     */
    def mapByValue: Map[A, B] =
      grouped.flatMap((key, value) => value.map(_ -> key))

  extension[A](seq: Seq[A])

    /** Convert the first two elements of the sequence into a tuple
     *
     * @throws IndexOutOfBoundsException if the sequence has less than two elements
     * @example {{{List(1, 2, 3).toCouple // (1, 2)}}}
     */
    def toCouple: (A, A) =
      (seq(0), seq(1))

    /** Convert to a `Map` where each distinct element is mapped to the number of its occurrences
     *
     * @example {{{List(1, 2, 2).groupBySize // Map(1 -> 1, 2 -> 2)}}}
     */
    def groupBySize: Map[A, Int] =
      seq.groupBy(identity).mapValues2(_.size)

    /** Convert to a `Map` where key is the element and value is a function applied to it
     *
     * @param f the function transforming each element
     * @return a `Map` mapping each element to its transformation.
     * @example {{{List(1, 2).toMap2(_ + 1) // Map(1 -> 2, 2 -> 3)}}}
     */
    def toMap2[T](f: A => T): Map[A, T] =
      seq.map(elem => elem -> f(elem)).toMap

    private def filterBySize(f: Int => Boolean): Iterable[A] =
      groupBySize.filter((_, size) => f(size)).keys

    /** Filter all elements occurring just once
     * 
     * @note it doesn't preserve the order
     * @return an 'Iterable' 
     * @example {{{List(1, 1, 2, 3, 2, 1, 4).filterUnique // Iterable(4, 3)}}}
     */
    def filterUnique: Iterable[A] =
      filterBySize(_ == 1)

    /** Filter all elements occurring more than once
     *
     * @note it doesn't preserve the order
     * @return an 'Iterable'
     * @example {{{List(1, 1, 2, 3, 2, 1, 4).filterNotUnique // Iterable(2, 1)}}}
     */
    def filterNotUnique: Iterable[A] =
      filterBySize(_ > 1)

  extension[A](list: List[A])

    /** Separate elements in disconnected groups
     *
     * @param areConnected the connection function
     * @return a 'List' of 'List'
     * @example {{{List(1, 2, 3, 5).groupConnected((x, y) => Math.abs(x - y) < 2) // List(List(5), List(1, 2, 3)}}}
     */
    def groupConnected(areConnected: (A, A) => Boolean): List[List[A]] =
      list.foldLeft(list.map(List(_)))((groups, elem) =>
        val (connected, unconnected) =
          groups.partition(_.exists(areConnected(_, elem)))
        connected.flatten :: unconnected
      )

  extension[A](iterable: Iterable[A])

    /** Tests whether each element compares with the corresponding element of another `Iterable`, according to a given function
     *
     * @param other the compared `Iterable`
     * @param f     the function comparing two elements
     * @return true if all element comparisons are true,
     *         otherwise false.
     * @example {{{List(1, 2).compareElems(List(2, 3))(_ < _) // true}}}
     */
    def compareElems(other: Iterable[A])(f: ((A, A)) => Boolean): Boolean =
      iterable.sizeCompare(other) == 0
        && iterable.lazyZip(other).forall((elem1, elem2) => f((elem1, elem2)))

  extension[A](sortedList: List[A])

    /** Filter out sequentially elements represented by others.
     *
     * @param f the function telling if an element may be represented by another
     */
    def originalsOnly(f: (A, A) => Boolean): List[A] =

      @tailrec
      def loop(seq: List[A], acc: List[A]): List[A] =
        seq match
          case Nil      => acc
          case h :: Nil => h :: acc
          case h :: t =>
            loop(
              t,
              t.find(f(h, _)) match
                case Some(_) => acc
                case None    => h :: acc
            )

      loop(sortedList, Nil)