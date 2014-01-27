package ru.wordmetrix.vector

import java.lang.Math.abs

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbDouble
import org.scalacheck.Gen
import org.scalacheck.Prop.{ forAll, propBoolean }
import org.scalacheck.Properties
import org.scalautils.Equality
import org.scalautils.Tolerance.convertNumericToPlusOrMinusWrapper
import org.scalautils.TripleEquals.convertToEqualizer

abstract class TestVector extends Properties("Vector") {

    implicit val accuracy: Double = 0.001
    implicit def VS: Arbitrary[Vector[Int]]
    val empty: Vector[Int]

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
            v1 + empty == v1
    }

    property("inverse item of addition") = forAll {
        (v1: Vector[Int]) =>
            v1 - v1 == empty
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
    /*
     * Clearing
     */

    property("clearRandom") = forAll(for {
        v <- VS.arbitrary
        n <- Gen.choose(0, v.size)
    } yield (v, n)) {
        case (v: Vector[Int], n: Int) =>
            s"Size is equal $n" |: v.clearRandomly(n).size == n &&
                (v - v.clearRandomly(n)).size == v.size - n
    }

    property("clearRandom") = forAll(for {
        v <- VS.arbitrary
        n <- Gen.choose(v.size, v.size * 2)
    } yield (v, n)) {
        case (v: Vector[Int], n: Int) =>
            s"Size is the same" |: v.clearRandomly(n).size == v.size && //
                (v - v.clearRandomly(n)).size == 0
    }

    property("clearRandom") = forAll(for {
        v <- VS.arbitrary
        n <- Gen.choose(-10, 0)
    } yield (v, n)) {
        case (v: Vector[Int], n: Int) =>
            (v.clearRandomly(n).size == 0)                
    }
    
    property("clearMinors") = forAll(for {
        v <- VS.arbitrary
        n <- Gen.choose(0, v.size)
    } yield (v, n)) {
        case (v: Vector[Int], n: Int) =>
            s"Size is equal $n" |: v.clearMinors(n).size == n &&
                (v - v.clearMinors(n)).size == v.size - n
    }

    property("clearMinors") = forAll(for {
        v <- VS.arbitrary
        n <- Gen.choose(v.size, v.size * 2)
    } yield (v, n)) {
        case (v: Vector[Int], n: Int) =>
            s"Size is the same" |: v.clearMinors(n).size == v.size && //
                (v - v.clearMinors(n)).size == 0
    }

    property("clearMinors") = forAll(for {
        v <- VS.arbitrary
        n <- Gen.choose(-10, 0)
    } yield (v, n)) {
        case (v: Vector[Int], n: Int) =>
            (v.clearMinors(n).size == 0)                
    }
    
    property("clearMinors") = forAll(for {
        v <- VS.arbitrary
        n <- Gen.choose(1,v.size-1)
    } yield (v.normal, n)) {
        case (v: Vector[Int], n: Int) =>
            
            val v1 =  v.clearMinors(n)
            (v1.map(x => abs(x._2)).min >= (v-v1).map(x=>abs(x._2)).max)
                            
    }


}

