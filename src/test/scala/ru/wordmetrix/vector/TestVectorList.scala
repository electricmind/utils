package ru.wordmetrix.vector

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class TestVectorList extends FlatSpec with Matchers {
    "A vector" should "be created" in {
        VectorList(("A", 1d))
    }

    "A vector" should "support addiniton" in {
        VectorList(("A", 1d)) + VectorList(("A", 1d)) should be(VectorList(("A", 2d)))
        VectorList(("B", 1d)) + VectorList(("A", 1d)) should be(VectorList(("A", 1d), ("B", 1d)))
        VectorList(("A", 1d)) + VectorList(("B", 1d)) should be(VectorList(("A", 1d), ("B", 1d)))
        VectorList(("A", 1d), ("C", 1d), ("D", 1d)) + VectorList(("B", 1d)) should be(VectorList(("A", 1d), ("B", 1d), ("C", 1d), ("D", 1d)))
        VectorList(("A", 1d)) + VectorList(("B", 1d), ("C", 1d), ("D", 1d)) should be(VectorList(("A", 1d), ("B", 1d), ("C", 1d), ("D", 1d)))
        VectorList(("E", 1d), ("F", 1d), ("D", 1d)) + VectorList(("B", 1d)) should be(VectorList(("E", 1d), ("F", 1d), ("D", 1d), ("B", 1d)))
        VectorList(("E", 1d)) + VectorList(("B", 1d), ("F", 1d), ("D", 1d)) should be(VectorList(("E", 1d), ("F", 1d), ("B", 1d), ("D", 1d)))
        VectorList(("A", 1d), ("C", 1d), ("D", 1d)) + VectorList(("A", -1d)) should be(VectorList(("C", 1d), ("D", 1d)))

    }
    "A vector" should "support substraction" in {
        VectorList(("A", 1d)) - VectorList(("A", 0.5d)) should be(VectorList(("A", 0.5d)))
        VectorList(("B", 1d)) - VectorList(("A", 1d)) should be(VectorList(("A", -1d), ("B", 1d)))
        VectorList(("A", 1d)) - VectorList(("B", 1d)) should be(VectorList(("A", 1d), ("B", -1d)))
        VectorList(("A", 1d), ("C", 1d), ("D", 1d)) - VectorList(("B", 1d)) should be(VectorList(("A", 1d), ("B", -1d), ("C", 1d), ("D", 1d)))
        VectorList(("A", 1d)) - VectorList(("B", 1d), ("C", 1d), ("D", 1d)) should be(VectorList(("A", 1d), ("B", -1d), ("C", -1d), ("D", -1d)))
        VectorList(("E", 1d), ("F", 1d), ("D", 1d)) - VectorList(("B", 1d)) should be(VectorList(("E", 1d), ("F", 1d), ("D", 1d), ("B", -1d)))
        VectorList(("E", 1d)) - VectorList(("B", 1d), ("F", 1d), ("D", 1d)) should be(VectorList(("E", 1d), ("F", -1d), ("B", -1d), ("D", -1d)))
        VectorList(("A", 1d), ("C", 1d), ("D", 1d)) - VectorList(("A", 1d)) should be(VectorList(("C", 1d), ("D", 1d)))
    }

    "A vector" should "support inner product" in {
        VectorList(("A", 1d)) * VectorList(("A", 0.5d)) should be(0.5)
        VectorList(("B", 1d)) * VectorList(("A", 1d)) should be(0d)
        VectorList(("A", 1d)) * VectorList(("B", 1d)) should be(0d)
        VectorList(("A", 1d), ("C", 1d), ("D", 1d)) * VectorList(("B", 1d)) should be(0d)
        VectorList(("A", 1d)) * VectorList(("B", 1d), ("C", 1d), ("D", 1d)) should be(0d)
        VectorList(("E", 1d), ("F", 1d), ("D", 1d)) * VectorList(("B", 1d)) should be(0d)
        VectorList(("A", 1d)) * VectorList(("B", 1d), ("F", 1d), ("D", 1d)) should be(0d)
        VectorList(("A", 1d), ("C", 1d), ("D", 1d)) * VectorList(("A", 1d)) should be(1d)
    }

    "A vector" should "has a norm" in {
        VectorList(("A", 1d), ("B", 1d)).normal should be(VectorList(("A", 0.7071067811865475), ("B", 0.7071067811865475)))
        VectorList(("A", 1d), ("B", 1d)).sqr should be(2)
        VectorList(("A", 1d), ("B", 1d)).norm should be(Math.sqrt(2))
    }

    "A vector" should "support multiplication" in {
        VectorList(("A", 1d), ("C", 1d), ("D", 1d)) * 2.0 should be(VectorList(("A", 2d), ("C", 2d), ("D", 2d)))
    }

    "A vector" should "support division" in {
        VectorList(("A", 1d), ("C", 1d), ("D", 1d)) / 0.5 should be(VectorList(("A", 2d), ("C", 2d), ("D", 2d)))
    }

    def isOrdered(l: Vector[String]) = l.map(_._1).toList match { case List() => true; case l => l.dropRight(1).zip(l.drop(1)).forall(x => x._1 < x._2) }

    "A clearRandom" should "cut off vector randomly" in {

        val vector = VectorList("A" -> 1d, "B" -> 1d, "C" -> 1d, "D" -> 1d, "E" -> 1d)
        for (i <- 1 to 5) {
            vector.clearRandomly(6).size should be(5)
            vector.clearRandomly(5).size should be(5)
            vector.clearRandomly(4).size should be(4)
            vector.clearRandomly(3).size should be(3)
            vector.clearRandomly(2).size should be(2)
            vector.clearRandomly(1).size should be(1)
            vector.clearRandomly(0).size should be(0)

            isOrdered(vector.clearRandomly(6)) should be(true)
            isOrdered(vector.clearRandomly(5)) should be(true)
            isOrdered(vector.clearRandomly(4)) should be(true)
            isOrdered(vector.clearRandomly(3)) should be(true)
            isOrdered(vector.clearRandomly(2)) should be(true)
            isOrdered(vector.clearRandomly(1)) should be(true)
            isOrdered(vector.clearRandomly(0)) should be(true)
        }
    }

    "A clearMinors" should "cut off minimum items of vector" in {

        val litters = Iterator.continually {
            val litters = Array[String]("F", "G", "H", "I")
            litters(scala.util.Random.nextInt(4))
        }

        val doubles = Iterator.continually {
            scala.util.Random.nextDouble
        }

        for (i <- 1 to 10) {
            val vector = (1 to 10).foldLeft(VectorList[String]("A" -> doubles.next, "B" -> doubles.next, "C" -> doubles.next, "D" -> doubles.next, "E" -> doubles.next))({
                case (v, x) => v + VectorList(litters.next -> doubles.next)

            })
            //            println(vector)
            vector.clearRandomly(5).clearMinors(6).size should be(5)
            vector.clearMinors(5).size should be(5)
            vector.clearMinors(4).size should be(4)
            vector.clearMinors(3).size should be(3)
            vector.clearMinors(2).size should be(2)
            vector.clearMinors(1).size should be(1)
            vector.clearMinors(0).size should be(0)

            isOrdered(vector.clearMinors(6)) should be(true)
            isOrdered(vector.clearMinors(5)) should be(true)
            isOrdered(vector.clearMinors(4)) should be(true)
            isOrdered(vector.clearMinors(3)) should be(true)
            isOrdered(vector.clearMinors(2)) should be(true)
            isOrdered(vector.clearMinors(1)) should be(true)
            isOrdered(vector.clearMinors(0)) should be(true)
        }
        val vector = VectorList("A" -> 2d, "B" -> 1.1d,
            "C" -> 4d, "D" -> -3d, "E" -> -1d)
        vector.clearMinors(5) should be(
            VectorList("A" -> 2d, "B" -> 1.1d, "C" -> 4d, "D" -> -3d, "E" -> -1d))
        vector.clearMinors(4) should be(
            VectorList("A" -> 2d, "B" -> 1.1d, "C" -> 4d, "D" -> -3d))
        vector.clearMinors(3) should be(
            VectorList("A" -> 2d, "C" -> 4d, "D" -> -3d))
        vector.clearMinors(2) should be(
            VectorList("C" -> 4d, "D" -> -3d))
        vector.clearMinors(1) should be(
            VectorList("C" -> 4d))
    }

}