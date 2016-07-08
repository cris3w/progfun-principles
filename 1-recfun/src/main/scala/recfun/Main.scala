package recfun

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
  def pascal(c: Int, r: Int): Int = {
    if  (c == 0 || c == r || r == 0)  1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def balanceIter(chars: List[Char], acc: Int): Boolean = {
      if (chars.isEmpty) acc == 0
      else if (chars.head == '(') balanceIter(chars.tail, acc + 1)
      else if (chars.head == ')' && acc > 0) balanceIter(chars.tail, acc - 1)
      else if (chars.head == ')' && acc == 0) false
      else balanceIter(chars.tail, acc)
    }

    balanceIter(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def countChangeIter(money: Int, coins: List[Int], acc: Int): Int = {
      if (money < 0) acc
      else if (coins.isEmpty)
        if (money == 0) acc + 1 else acc
      else countChangeIter(money - coins.head, coins, acc) + countChangeIter(money, coins.tail, acc)
    }

    countChangeIter(money, coins, 0)
  }

}
