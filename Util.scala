import scala.collection.mutable

object Util {
  def max[A](list: List[A], func: (A, A) => Int): A = {
    val last = list.last
    if (list.length == 1) {
      last
    } else {
      val trnList = list.init
      val maxEl = max(trnList, func)

      if (func(last, maxEl) > 0) last else maxEl
    }
  }

  def map[A, B, C](list: List[A], funcAB: A => B, funcBC: B => C): List[C] = {
    if (list.isEmpty) {
      return List()
    }
    val last = List(funcBC(funcAB(list.last)))
    if (list.length == 1) {
      last
    } else {
      val trnList = list.init

      map(trnList, funcAB, funcBC) ::: last
    }
  }

  @scala.annotation.tailrec
  def isSorted[A](list: List[A], func: (A, A) => Boolean): Boolean = {
    val last = list.last
    if (list.length < 2) {
      true
    } else {
      val trnList = list.init
      val beforeLast = trnList.last

      func(beforeLast, last) && isSorted(trnList, func)
    }
  }


  def probs(vs: Array[Double]): Array[Double] = {
    vs.map(value => vs.count(x => x == value) / vs.length.toDouble)
  }

  def entropy(vs: Array[Double]): Double = {
    (vs zip probs(vs)).distinct.map(e => -e._2 * (Math.log(e._2) / Math.log(2))).sum
  }

  def entropyWithoutX(vs: Array[Double], xIndex: Int): Double = {
    (vs zip probs(vs))
      .zipWithIndex
      .filter(pairWithIndex => pairWithIndex._2 != xIndex)
      .map(pairWithIndex => pairWithIndex._1)
      .distinct
      .map(e => -e._2 * (Math.log(e._2) / Math.log(2))).sum
  }

  def entropyDiff(vs: Array[Double], xIndex: Int) = {
    entropy(vs) - entropyWithoutX(vs, xIndex)
  }

  def mu(vs: Array[Double]): Double = {
    (vs zip probs(vs)).distinct.map(e => e._1 * e._2).sum
  }

  def variance(vs: Array[Double]): Double = {
    val curr_mu = mu(vs)
    (vs zip probs(vs)).distinct.map(e => e._2 * Math.pow(e._1 - curr_mu, 2)).sum
  }

  def sd(vs: Array[Double]): Double = {
    Math.pow(variance(vs), 0.5)
  }

  def zscore(vs: Array[Double], x: Double): Double = {
    (x - mu(vs)) / sd(vs)
  }

  def cov(xs: Array[Double], ys: Array[Double]): Double = {
    mu((xs zip ys).map(xy => xy._1 * xy._2)) - mu(xs) * mu(ys)
  }

  def pearson(xs: Array[Double], ys: Array[Double]): Double = {
    cov(xs, ys) / (sd(xs) * sd(ys))
  }

}

