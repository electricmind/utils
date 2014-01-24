package ru.wordmetrix.vector

import scala.collection.TraversableProxy
import scala.collection.immutable.HashMap
import Math.{ abs, sqrt }
import scala.collection.immutable.MapProxy

object VectorHASH extends VectorFactory {
    def factory[F](list: List[(F, Double)])(
        implicit ord: Ordering[F]): Vector[F] = {
        val m = list.foldLeft(HashMap[F, Double]()) {
            case (map, item) => map + item
        }
        val f = new VectorHASH[F](
            m
        )
        f
    }
}

class VectorHASH[F](val self: HashMap[F, Double])(
    implicit accuracy: Double = 0.0001,
    ord: Ordering[F]) extends MapProxy[F, Double]
        with Serializable with Vector[F] {

    def +(v: Vector[F]): Vector[F] = {
        new VectorHASH[F](

            (for {
                (key, value) <- v
                sv <- self.get(key) orElse Some(0.0)
                nv <- Option(sv + value)
                //if abs(nv) > accuracy
            } yield {
                key -> nv
            }).foldLeft(self) {
                case (map, item) if abs(item._2) > accuracy => map + item
                case (map, item)                            => map - item._1
            }
        )
    }
    def -(v: Vector[F]): Vector[F] = new VectorHASH[F](
        (for {
            (key, value) <- v
            sv <- self.get(key) orElse Some(0.0)
            nv <- Option(sv - value)
            //if abs(nv) > accuracy
        } yield {
            key -> nv
        }).foldLeft(self) {
            case (map, item) if abs(item._2) > accuracy => map + item
            case (map, item)                            => map - item._1
        }
    )

    def *(v: Vector[F]): Double =
        (for {
            (key, value) <- v
            sv <- self.get(key)
        } yield {
            sv * value
        }).reduceOption(_ + _) getOrElse 0.0

    def *(z: Double): Vector[F] = new VectorHASH[F](
        (for {
            (key, value) <- self
            nv <- value * z match {
                case x if abs(x) > accuracy => Option(x)
                case _                      => None
            }
        } yield {
            key -> value * z
        }).foldLeft(HashMap[F, Double]()) {
            case (map, item) => map + item
        }
    )

    def /(z: Double): Vector[F] = this * (1 / z)

    lazy val sqr: Double = {
        self.map({ case (x, y) => y * y }).reduceOption(_ + _) getOrElse 0
    }
    lazy val norm: Double = sqrt(sqr)
    lazy val normal: Vector[F] = this / norm

    //TODO : clearRandomly
    def clearRandomly(n: Int): VectorHASH[F] = this

    //TODO : clearMinors
    def clearMinors(n: Int): VectorHASH[F] = this

    def clear(accuracy: Double = accuracy) = new VectorHASH[F](
        (for {
            (key, value) <- self
            if abs(value) < accuracy
        } yield {
            key
        }).foldLeft(self) {
            case (map, item) => map - item
        }
    )
}