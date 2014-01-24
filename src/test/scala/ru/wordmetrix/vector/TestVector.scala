package ru.wordmetrix.vector

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math._

object QuickCheckHeap extends Properties("VectorHASH") {

    implicit val VS = for {
        k1 <- Gen.choose(0, 10)
        k2 <- Gen.choose(0, 10)
        k3 <- Gen.choose(0, 10)
        k4 <- Gen.choose(0, 10)
        a1 <- Gen.choose(.0, .10)
        a2 <- Gen.choose(.0, .10)
        a3 <- Gen.choose(.0, .10)
        a4 <- Gen.choose(.0, .10)
    } yield VectorHASH[Int](k1 -> a1, k2 -> a2, k3 -> a3, k4 -> a4)

    val V2S = for {
        v1 <- VS
        v2 <- VS
    } yield (v1, v2)

    implicit val accuracy : Double =  0.001
    implicit class VectorCheck(v1: Vector[Int]) {
        def compare(v2: Vector[Int])(implicit accuracy: Double) =
            (v1 - v2).forall {
                case (_, x) => abs(x) < accuracy 
            }
    }
    
    property("compare") = forAll(V2S) {
        case (v1: VectorHASH[Int], v2: VectorHASH[Int]) =>
            (v1 == v1) ==> (v1 compare v2)
            (v1 != v2) ==> !((v1 compare v2)(0.0))
    }
    
    property("comutativity") = forAll(V2S) {
        case (v1: VectorHASH[Int], v2: VectorHASH[Int]) =>
            v1 + v2 == v2 + v1
    }

    property("identity of addition") = forAll(VS) {
        case (v1: VectorHASH[Int]) =>
            v1 + VectorHASH.empty[Int] == v1
    }

    property("inverse item of addition") = forAll(VS) {
        case (v1: VectorHASH[Int]) =>
            v1 - v1 == VectorHASH.empty[Int]
    }

    property("multiplication associativity") = forAll(for {
        v <- VS
        a <- Gen.choose(.0, .10)
        b <- Gen.choose(.0, .10)
    } yield (v, a, b)) {
        case (v1: VectorHASH[Int], a: Double, b: Double) =>
            v1 * 1 == v1
    }

    property("scala multiplication associativity") = forAll(for {
        v <- VS
        a <- Gen.choose(.0, .10)
        b <- Gen.choose(.0, .10)
    } yield (v, a, b)) {
        case (v: VectorHASH[Int], a: Double, b: Double) =>
            (v * a) * b compare v * (a * b)
    }

    property("scala multiplication identity") = forAll(for {
        v <- VS
        a <- Gen.choose(.0, .10)
    } yield (v, a)) {
        case (v1: VectorHASH[Int], a: Double) =>
            v1 * 1 == v1
    }

    property("distributivity with respect to addition") = forAll(for {
        (v1, v2) <- V2S
        a <- Gen.choose(.0, .10)
    } yield (v1, v2, a)) {
        case (v1: VectorHASH[Int], v2: VectorHASH[Int], a: Double) =>
            (v1 + v2) * a + "\n" + (v1 * a + v2 * a) |:
                ((v1 + v2) * a compare (v1 * a + v2 * a))
    }

    property("distributivity with respect to multiplication") = forAll(for {
        v1 <- VS
        a <- Gen.choose(.0, .10)
        b <- Gen.choose(.0, .10)
    } yield (v1, a, b)) {
        case (v: VectorHASH[Int], a: Double, b: Double) =>
            v * (a + b) compare v * a + v * b
    }
    
    
    
}

