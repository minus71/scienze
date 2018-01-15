package scienze

import scala.annotation.tailrec

case class FoundValue(depth: Int, num: Int)

object SplitSearch {

  def main(args: Array[String]): Unit = {

    val n = 2017

    val result = search(n, Seq(Seq(1, 2)))
    result.reverse.foreach(r => println(s"${r.depth + 1}:${r.num}"))

    val total = result.map(_.num).sum * 2

    println(s"fn($n) = $total")
  }

  def search(n: Int, space: Seq[Seq[Int]]): Seq[FoundValue] = {
    @tailrec
    def searchImpl(depth: Int, ss: Seq[Seq[Int]], results: Seq[FoundValue]): Seq[FoundValue] = {

      if (ss.isEmpty) return results
      else {

        val nextSeq = ss.map(next)
        val found = nextSeq
          .foldLeft(0) { (tot, xs) => tot + xs.count(_ == n) }

        val nextResults = if (found > 0) FoundValue(depth, found) +: results else results

        val nextSearchList = nextSeq.flatMap(splitSeq(n))

        return searchImpl(depth + 1, nextSearchList, nextResults)
      }
    }

    searchImpl(2, space, Seq())
  }

  val EMPTY_SEQ: Seq[Seq[Int]] = Seq(Seq())

  def splitSeq(sval: Int)(xs: Seq[Int]): Seq[Seq[Int]] = {
    val unfiltered = {
      xs.foldLeft(EMPTY_SEQ) {
        ((xss, x) =>
          if (x >= sval) Seq() +: xss
          else (x +: xss.head) +: xss.tail)
      }
    }
    (unfiltered filter {
      case a :: bb :: tail => true
      case _               => false
    } reverse) map (_.reverse)
  }

  def next(xs: Seq[Int]): Seq[Int] = {
    val ys = xs zip xs.drop(1)
    xs.head +: ys.flatMap { case (l, r) => List(l + r, r) }
  }

}


