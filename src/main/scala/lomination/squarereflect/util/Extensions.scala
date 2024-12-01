package lomination.squarereflect.util

import scala.collection.mutable.Builder
import scala.util.{Failure, Success, Try}
import lomination.squarereflect.Position

extension [A](seq: Seq[Try[A]])
  /** Transforms this sequence of tries of A into a try of sequence of A */
  def toTry: Try[Seq[A]] =
    seq
      .foldLeft[Try[Builder[A, Seq[A]]]](Success(Seq.newBuilder)) {
        case (Success(l), Success(a))   => Success(l.addOne(a))
        case (failure: Failure[_], _)   => failure
        case (_, Failure[A](exception)) => Failure[Builder[A, Seq[A]]](exception)
      }
      .map(_.result)

extension [A](grid: Seq[Seq[A]])
  def toPositionedSeq: Seq[(A, Position)] =
    grid.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.map { case (a, x) => (a, Position(x, y)) }
    }

extension [A](positionedSeq: Seq[(A, Position)])
  def toGrid: Seq[Seq[A]] =
    val maxIndex = positionedSeq.map { case (a, Position(x, y)) => x } reduce { math.max }
    positionedSeq
      .foldLeft[(Builder[Seq[A], Seq[Seq[A]]], Builder[A, Seq[A]])](Seq.newBuilder, Seq.newBuilder) {
        case ((grid, line), (a, Position(maxIndex, _))) => (grid addOne (line.addOne(a).result), Seq.newBuilder)
        case ((grid, line), (a, _))                     => (grid, line.addOne(a))
      }
      ._1
      .result
