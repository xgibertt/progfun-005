package recfun
import common._

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r ) 1
    else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def balanceChunck(chars: List[Char], result: Int = 0): Int =
      if (chars.isEmpty || result < 0) result
      else chars.head match {
        case '(' => balanceChunck(chars.tail, result + 1)
        case ')' => balanceChunck(chars.tail, result - 1)
        case _ =>   balanceChunck(chars.tail, result)
      }

    balanceChunck(chars) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) +
      countChange(money, coins.tail)
  }
}