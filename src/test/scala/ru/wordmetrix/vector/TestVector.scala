package ru.wordmetrix.vector

import org.scalacheck._

import Arbitrary._
import Gen._
import Prop._
import Math._
import org.scalautils._
import Tolerance._
import TripleEquals._

object QuickCheckHeap extends Properties("VectorHASH") {

    implicit val accuracy: Double = 0.001
    implicit val VS = Arbitrary[Vector[Int]](for {
        keys <- Gen.containerOf[List, Int](for {
            k <- Gen.choose(0, 10)
        } yield (k)) map (_.removeDuplicates.take(5).sortBy(x => x))
        values <- Gen.containerOf[List, Double](for {
            k1 <- Gen.choose(accuracy, 100)
            k2 <- Gen.choose(-100, -accuracy)
        } yield (k1 + k2))
    } yield {
        VectorHASH[Int](keys.zip(values))
    })

    implicit class VectorCheck(v1: Vector[Int]) {
        def compare(v2: Vector[Int])(implicit accuracy: Double) =
            (v1 - v2).forall {
                case (_, x) => abs(x) < accuracy
            }
    }
    implicit val vectorEq =
        new Equality[Vector[Int]] {
            def areEqual(v1: Vector[Int], v2: Any): Boolean =
                v2 match {
                    case v2: Vector[Int] => (v1 - v2).forall {
                        case (_, x) => x === 0.0 +- accuracy
                    }
                    case _ => false
                }
        }
    type A = Double
    val a = Arbitrary[A](Gen.choose(-10d, +10d))

    /*
     * Check technical function compare
     */
    property("compare") = forAll {
        (v1: Vector[Int], v2: Vector[Int]) =>
            {
                "Vector is equal itself" |: (v1 == v1) ==> (v1 compare v1)
            } && {
                "Vector is different from another" |:
                    (v1 != v2) ==> !((v1 compare v2)(0.0))
            } && {
                "Vector is equal itself +- a " |: (v1 == v1) ==> (v1 === v1)
            } && {
                "Vector is different from another +- a" |:
                    (v1 != v2) ==> !((v1 === v2))
            }
    }

    /*
     * Linear vector space axioms
     */

    property("comutativity") = forAll {
        (v1: Vector[Int], v2: Vector[Int]) =>
            v1 + v2 == v2 + v1
    }

    property("identity of addition") = forAll {
        (v1: Vector[Int]) =>
            v1 + VectorHASH.empty[Int] == v1
    }

    property("inverse item of addition") = forAll {
        (v1: Vector[Int]) =>
            v1 - v1 == VectorHASH.empty[Int]
    }

    property("multiplication associativity") = forAll {
        (v1: Vector[Int]) => v1 * 1 == v1
    }

    property("scala multiplication associativity") = forAll {
        (v: Vector[Int], a: A, b: A) =>
            (v * a) * b === v * (a * b)
    }

    property("scala multiplication identity") = forAll {
        (v: Vector[Int]) =>
            (v + "\n" + v * 1) |: v == v * 1
    }

    property("distributivity with respect to addition") =
        forAll(VS.arbitrary, VS.arbitrary, a.arbitrary) {
            case (v1: Vector[Int], v2: Vector[Int], a: A) =>
                ((v1 + v2) * a + "\n" + (v1 * a + v2 * a)) |:
                    ((v1 + v2) * a === (v1 * a + v2 * a))
        }

    property("distributivity with respect to multiplication") =
        forAll(VS.arbitrary, a.arbitrary, a.arbitrary) {
            case (v: Vector[Int], a: A, b: A) =>
                (v * (a + b) === v * a + v * b)
        }
    /*
     * Other properties
     */
    property("division and multiplication") =
        forAll(VS.arbitrary, a.arbitrary) {
            case (v: Vector[Int], a: A) =>
                s"${v / a} ${(v * (1 / a))}" |: (v / a === (v * (1 / a)))
        }

    property("addition and substraction") =
        forAll(VS.arbitrary, VS.arbitrary) {
            case (v1: Vector[Int], v2: Vector[Int]) =>
                ((v1 + v2 * (-1)) === (v1 - v2)) &&
                    ((v1 - v2) === (v2 - v1) * -1)
        }

    property("sqr") =
        forAll {
            (v: Vector[Int]) =>
                v.sqr == v * v &&
                    (v.sqr != 0) ==> (s"${(v + v).sqr / v.sqr} == 4" |:
                        ((v + v).sqr / v.sqr == 4))
        }

    property("norm") =
        forAll {
            (v: Vector[Int]) =>
                (v.sqr != 0) ==> {
                    (s"${(v + v).norm} / ${v.norm} == 2}" |:
                        (v + v).norm / v.norm == 2) &&
                        (s"${v.norm * v.norm} == v.sqr" |:
                            v.norm * v.norm === v.sqr +- 1)
                }
        }

    property("normal") =
        forAll {
            (v: Vector[Int]) =>
                (v.sqr != 0) ==> {
                    (v + v).normal == v.normal &&
                        v.normal * v.normal === 1.0 +- accuracy &&
                        v.normal.sqr === 1.0 +- accuracy
                    v / v.norm === v.normal

                }
        }

}

