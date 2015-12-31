package ru.wordmetrix.vector

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class TestVectorHASHHash extends FlatSpec with Matchers {

    implicit val accuracy: Double = 0.0001

    "A vector" should "be created" in {
        println("1")
        VectorHASH(("A", 1d))
        println("1")
    }

    "A vector" should "support addiniton" in {
        VectorHASH(("A", 1d)) + VectorHASH(("A", 1d)) should be(VectorHASH(("A", 2d)))
        VectorHASH(("B", 1d)) + VectorHASH(("A", 1d)) should be(VectorHASH(("A", 1d), ("B", 1d)))
        VectorHASH(("A", 1d)) + VectorHASH(("B", 1d)) should be(VectorHASH(("A", 1d), ("B", 1d)))
        VectorHASH(("A", 1d), ("C", 1d), ("D", 1d)) + VectorHASH(("B", 1d)) should be(VectorHASH(("A", 1d), ("B", 1d), ("C", 1d), ("D", 1d)))
        VectorHASH(("A", 1d)) + VectorHASH(("B", 1d), ("C", 1d), ("D", 1d)) should be(VectorHASH(("A", 1d), ("B", 1d), ("C", 1d), ("D", 1d)))
        VectorHASH(("E", 1d), ("F", 1d), ("D", 1d)) + VectorHASH(("B", 1d)) should be(VectorHASH(("E", 1d), ("F", 1d), ("D", 1d), ("B", 1d)))
        VectorHASH(("E", 1d)) + VectorHASH(("B", 1d), ("F", 1d), ("D", 1d)) should be(VectorHASH(("E", 1d), ("F", 1d), ("B", 1d), ("D", 1d)))
        VectorHASH(("A", 1d), ("C", 1d), ("D", 1d)) + VectorHASH(("A", -1d)) should be(VectorHASH(("C", 1d), ("D", 1d)))

    }
    "A vector" should "support substraction" in {
        VectorHASH(("A", 1d)) - VectorHASH(("A", 0.5d)) should be(VectorHASH(("A", 0.5d)))
        VectorHASH(("B", 1d)) - VectorHASH(("A", 1d)) should be(VectorHASH(("A", -1d), ("B", 1d)))
        VectorHASH(("A", 1d)) - VectorHASH(("B", 1d)) should be(VectorHASH(("A", 1d), ("B", -1d)))
        VectorHASH(("A", 1d), ("C", 1d), ("D", 1d)) - VectorHASH(("B", 1d)) should be(VectorHASH(("A", 1d), ("B", -1d), ("C", 1d), ("D", 1d)))
        VectorHASH(("A", 1d)) - VectorHASH(("B", 1d), ("C", 1d), ("D", 1d)) should be(VectorHASH(("A", 1d), ("B", -1d), ("C", -1d), ("D", -1d)))
        VectorHASH(("E", 1d), ("F", 1d), ("D", 1d)) - VectorHASH(("B", 1d)) should be(VectorHASH(("E", 1d), ("F", 1d), ("D", 1d), ("B", -1d)))
        VectorHASH(("E", 1d)) - VectorHASH(("B", 1d), ("F", 1d), ("D", 1d)) should be(VectorHASH(("E", 1d), ("F", -1d), ("B", -1d), ("D", -1d)))
        VectorHASH(("A", 1d), ("C", 1d), ("D", 1d)) - VectorHASH(("A", 1d)) should be(VectorHASH(("C", 1d), ("D", 1d)))
    }

    "A vector" should "support inner product" in {
        VectorHASH(("A", 1d)) * VectorHASH(("A", 0.5d)) should be(0.5)
        VectorHASH(("B", 1d)) * VectorHASH(("A", 1d)) should be(0d)
        VectorHASH(("A", 1d)) * VectorHASH(("B", 1d)) should be(0d)
        VectorHASH(("A", 1d), ("C", 1d), ("D", 1d)) * VectorHASH(("B", 1d)) should be(0d)
        VectorHASH(("A", 1d)) * VectorHASH(("B", 1d), ("C", 1d), ("D", 1d)) should be(0d)
        VectorHASH(("E", 1d), ("F", 1d), ("D", 1d)) * VectorHASH(("B", 1d)) should be(0d)
        VectorHASH(("A", 1d)) * VectorHASH(("B", 1d), ("F", 1d), ("D", 1d)) should be(0d)
        VectorHASH(("A", 1d), ("C", 1d), ("D", 1d)) * VectorHASH(("A", 1d)) should be(1d)
    }

    "A vector" should "has a norm" in {
        VectorHASH(("A", 1d), ("B", 1d)).normal should be(VectorHASH(("A", 0.7071067811865475), ("B", 0.7071067811865475)))
        VectorHASH(("A", 1d), ("B", 1d)).sqr should be(2)
        VectorHASH(("A", 1d), ("B", 1d)).norm should be(Math.sqrt(2))
    }

    "A vector" should "support multiplication" in {
        VectorHASH(("A", 1d), ("C", 1d), ("D", 1d)) * 2.0 should be(VectorHASH(("A", 2d), ("C", 2d), ("D", 2d)))
    }

    "A vector" should "support division" in {
        VectorHASH(("A", 1d), ("C", 1d), ("D", 1d)) / 0.5 should be(VectorHASH(("A", 2d), ("C", 2d), ("D", 2d)))
    }

    def isOrdered(l: Vector[String]) = l.map(_._1).toList match { case List() => true; case l => l.dropRight(1).zip(l.drop(1)).forall(x => x._1 < x._2) }

    "A clearRandom" should "cut off vector randomly" in {

        val vector = VectorHASH("A" -> 1d, "B" -> 1d, "C" -> 1d, "D" -> 1d, "E" -> 1d)
        for (i <- 1 to 5) {
            vector.clearRandomly(6).size should be(5)
            vector.clearRandomly(5).size should be(5)
            vector.clearRandomly(4).size should be(4)
            vector.clearRandomly(3).size should be(3)
            vector.clearRandomly(2).size should be(2)
            vector.clearRandomly(1).size should be(1)
            vector.clearRandomly(0).size should be(0)
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
            val vector = (1 to 10).foldLeft(VectorHASH[String]("A" -> doubles.next, "B" -> doubles.next, "C" -> doubles.next, "D" -> doubles.next, "E" -> doubles.next))({
                case (v, x) => v + VectorHASH(litters.next -> doubles.next)

            })
            //            println(vector)
            vector.clearRandomly(5).clearMinors(6).size should be(5)
            vector.clearMinors(5).size should be(5)
            vector.clearMinors(4).size should be(4)
            vector.clearMinors(3).size should be(3)
            vector.clearMinors(2).size should be(2)
            vector.clearMinors(1).size should be(1)
            vector.clearMinors(0).size should be(0)
        }
          
        val vector = VectorHASH("A" -> 2d, "B" -> 1.1d,
            "C" -> 4d, "D" -> -3d, "E" -> -1d)
        vector.clearMinors(5) should be(
            VectorHASH("A" -> 2d, "B" -> 1.1d, "C" -> 4d, "D" -> -3d, "E" -> -1d))
        vector.clearMinors(4) should be(
            VectorHASH("A" -> 2d, "B" -> 1.1d, "C" -> 4d, "D" -> -3d))
        vector.clearMinors(3) should be(
            VectorHASH("A" -> 2d, "C" -> 4d, "D" -> -3d))
        vector.clearMinors(2) should be(
            VectorHASH("C" -> 4d, "D" -> -3d))
        vector.clearMinors(1) should be(
            VectorHASH("C" -> 4d))
    }
}