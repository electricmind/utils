package ru.wordmetrix.vector

import org.scalacheck.{ Arbitrary, Gen }

object CheckVectorList extends TestVector {
    val empty: Vector[Int] = VectorList.empty[Int]

    implicit def VS = Arbitrary[Vector[Int]](for {
        keys <- Gen.containerOf[List, Int](for {
            k <- Gen.choose(0, 10)
        } yield (k)) map (_.distinct.take(5).sortBy(x => x))
        values <- Gen.containerOf[List, Double](for {
            k1 <- Gen.choose(accuracy, 100)
            k2 <- Gen.choose(-100, -accuracy)
        } yield (k1 + k2))
    } yield {
        VectorList[Int](keys.zip(values))
    })
}
