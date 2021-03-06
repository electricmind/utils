package ru.wordmetrix.vector

import scala.collection.TraversableProxy

object VectorList extends VectorFactory {
  def factory[F](list: List[(F, Double)])(implicit accuracy: Double, ord: Ordering[F]): Vector[F] = new VectorList(list)
}

class VectorList[F](val self: List[(F, Double)])(
  implicit accuracy: Double,
  ord: Ordering[F])
  extends TraversableProxy[(F, Double)] with Serializable with Vector[F] {
  type Pair = (F, Double)
  type Pairs = List[Pair]

  //Vector[Int]()

  def this()(implicit accuracy: Double = 0.0001, ord: Ordering[F]) = this(List())

  def +(v: Vector[F]) = new VectorList(pairs(self, v.self.toList).map({
    case (f, (d0, d1)) => (f, d0 + d1)
  }).filter(filter)) //.sortBy(_._1))

  def -(v: Vector[F]) = new VectorList(pairs(self, v.self.toList).map({
    case (f, (d0, d1)) => (f, d0 - d1)
  }).filter(filter)) //.sortBy(_._1))

  def *(v: Vector[F]) = pairs(self, v.self.toList).map({
    case (f, (d0, d1)) => d0 * d1
  }).sum

  private def filter(pair: Pair) = Math.abs(pair._2) > accuracy

  private def pairs(ps1: Pairs, ps2: Pairs, outcome: List[(F, (Double, Double))] = List()): List[(F, (Double, Double))] = (ps1, ps2) match {

    case ((f1, d1) :: pst1, (f2, d2) :: pst2) => if (ord.gt(f1, f2)) {
      pairs(ps1, pst2, (f2, (0d, d2)) :: outcome)
    } else if (ord.lt(f1, f2)) {
      pairs(pst1, ps2, (f1, (d1, 0d)) :: outcome)
    } else {
      pairs(pst1, pst2, (f1, (d1, d2)) :: outcome)
    }

    case (pst, List()) => outcome.reverse ++ pst.map({ case (f, d: Double) => (f, (d, 0d)) })
    case (List(), pst) => outcome.reverse ++ pst.map({ case (f, d: Double) => (f, (0d, d)) })
  }

  def *(z: Double) = {
    //println(z)
    new VectorList(map({ case (x, y) => (x, y * z) }).toList)
  }

  def /(z: Double) = {
    new VectorList(map({ case (x, y) => (x, y / z) }).toList)
  }

  lazy val sqr = map(_._2).map(Math.pow(_, 2)).sum
  val norm = Math.pow(sqr, 0.5)

  //def
  lazy val normal = {
    this / norm
  }

  def clearRandomly(n: Int) = {
    val length = self.length

    def clear(rs: List[Int], vector: List[(F, Double)], n: Int,
              outcome: List[(F, Double)]): List[(F, Double)] =
      (rs, vector) match {
        case ((r :: rs), (k, v) :: vector) => if (r <= n) {
          clear(rs, vector, n + 1, outcome)
        } else {
          clear(r :: rs, vector, n + 1, (k, v) :: outcome)
        }
        case (rs, vector) => outcome.reverse.drop(rs.length) ++ vector
      }

    if (length > 0) new VectorList(
      clear((1 to (length - n)).map(
        x => scala.util.Random.nextInt(length)
      ).toList.sorted, self, 0, List())
    )
    else this
  }

  def clearMinors(n: Int) = if (n < self.length) Vector(
    self.sortBy(x => Math.abs(x._2)).takeRight(Math.max(0, n))
  )
  else this

  def clear(accuracy: Double): Vector[F] = ???
}
