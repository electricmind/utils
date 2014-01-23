package ru.wordmetrix.vector

import scala.collection.TraversableProxy
import scala.collection.immutable.HashMap
import Math.{ abs, sqrt }
import scala.collection.immutable.MapProxy

object VectorHASH extends VectorFactory {
    def factory[F](list: List[(F, Double)])(
        implicit ord: Ordering[F]): Vector[F] =
        new VectorHASH[F](
            HashMap[F, Double](list.map({ case (x, y) => x -> y }): _*)
        )
}

class VectorHASH[F](val self: HashMap[F, Double])(
    implicit accuracy: Double = 0.0001,
    ord: Ordering[F]) extends MapProxy[F, Double]
        with Serializable with Vector[F] {

    def +(v: Vector[F]): Vector[F] =
        new VectorHASH[F](HashMap[F, Double](
            (for {
                (key, value) <- v
                sv <- self.get(key)
                nv <- Option(sv + value)
                if abs(nv) > accuracy
            } yield {
                key -> nv
            }).toSeq: _*
        ))

    def -(v: Vector[F]): Vector[F] = new VectorHASH[F](HashMap[F, Double](
        (for {
            (key, value) <- v
            sv <- self.get(key)
            nv <- Option(sv - value)
            if abs(nv) > accuracy
        } yield {
            key -> nv
        }).toSeq: _*
    ))

    def *(v: Vector[F]): Double =
        (for {
            (key, value) <- v
            sv <- self.get(key)
        } yield {
            sv * value
        }).reduceOption(_ + _) getOrElse 0.0

    def *(z: Double): Vector[F] = new VectorHASH[F](HashMap[F, Double](
        (for {
            (key, value) <- self
            nv <- value * z match {
                case x if x > accuracy => Option(x)
                case _                 => None
            }
        } yield {
            key -> value * z
        }) toSeq: _*
    ))

    def /(z: Double): Vector[F] = this * (1 / z)

    lazy val sqr: Double = self.map(_._2).reduceOption(_ * _) getOrElse 0
    lazy val norm: Double = sqrt(sqr)
    val normal: Vector[F] = this / norm

    //TODO : clearRandomly
    def clearRandomly(n: Int): Vector[F] = this

    //TODO : clearMinors
    def clearMinors(n: Int): Vector[F] = this
}