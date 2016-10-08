package reductions

import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 10000
    val chars = new Array[Char](length)
    val threshold = 10
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def calculate(char: Char): Int = {
      char match {
        case '(' => 1
        case ')' => -1
        case _ => 0
      }
    }
    def loop(chars: Array[Char], acc: Int): Boolean =
      if (acc < 0) false
      else if (chars.isEmpty) acc == 0
      else loop(chars.tail, acc + calculate(chars.head))

    loop(chars, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    type Result = (Int, Int)

    def combineSegments(lc: Int, lo: Int, rc: Int, ro: Int): (Int, Int) = {
      val closed = lc + (if ((rc - lo) <= 0) 0 else rc - lo)
      val open = if ((lo - rc + ro) < 0) 0 else lo - rc + ro
      (closed, open)
    }

    def traverse(idx: Int, until: Int, closed: Int, open: Int): (Int, Int) = {
      if (idx == until)
        (closed, open)
      else chars(idx) match {
          case '(' => traverse(idx + 1, until, closed, open + 1)
          case ')' =>
            if (open > 0) traverse(idx + 1, until, closed, open - 1)
            else traverse(idx + 1, until, closed + 1, open)
          case _ => traverse(idx + 1, until, closed, open)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if(until - from <= threshold)
        traverse(from, until, 0, 0)
      else {
        val mid = from + (until - from) / 2
        val ((lc, lo), (rc, ro)) = parallel(
          reduce(from, mid),
          reduce(mid, until))
        combineSegments(lc, lo, rc, ro)
      }
    }
    reduce(0, chars.length) == (0, 0)
  }
}
