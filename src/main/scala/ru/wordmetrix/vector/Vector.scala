package ru.wordmetrix.vector
import scala.collection.TraversableProxy
import scala.math.Ordering.StringOrdering
import scala.collection.Traversable

abstract class VectorFactory {
    def factory[F](list: List[(F, Double)])(
        implicit ord: Ordering[F]): Vector[F]

    def apply[F](list: List[(F, Double)])(implicit ord: Ordering[F]): Vector[F] = {
        val f = factory(
            list.groupBy(_._1).map({ case (x, y) => x -> y.map(_._2).sum }).toList.sortBy(_._1)
        )
        f
    }
    def apply[F](pairs: (F, Double)*)(implicit ord: Ordering[F]): Vector[F] = apply(pairs.toList)
}

object Vector extends VectorFactory {
    def factory[F](list: List[(F, Double)])(
        implicit ord: Ordering[F]): Vector[F] = new VectorList(list)
}

abstract trait Vector[F] extends Traversable[(F, Double)] {
    val self: Traversable[(F, Double)]

    def +(v: Vector[F]): Vector[F]

    def -(v: Vector[F]): Vector[F]

    def *(v: Vector[F]): Double

    def *(z: Double): Vector[F]

    def /(z: Double): Vector[F]

    val sqr: Double
    val norm: Double

    val normal: Vector[F]

    def clearRandomly(n: Int): Vector[F]

    def clearMinors(n: Int): Vector[F]
}

