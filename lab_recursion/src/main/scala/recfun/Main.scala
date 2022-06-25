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
    if (c == 0 || c == r) 1
    else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def charCheck(chars: List[Char], openedBraces: Int): Boolean = {
      if (chars.isEmpty) {
        openedBraces == 0
      }
      else {
        def bracesCheck = {
          if
          (chars.head == '(')
            openedBraces + 1
          else if
          (chars.head == ')')
            openedBraces - 1
          else
            openedBraces
        }

        if (bracesCheck >= 0)
          charCheck(chars.tail, bracesCheck)
        else
          false
      }
    }

    charCheck(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if ((money < 0) || coins.isEmpty) 0
    else {
      if (money == 0) 1
      else {
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
      }
    }
  }

  /**
   * Exercise 4 (variant 19)
   */
  def funcSolution(x: Int, n: Int): Double = {
    @tailrec
    def pow(base: Double, exp: Int, acc: Double = 1): Double = {
      if (exp == 0 || base == 1) {
        acc
      } else if (exp < 0) {
        pow(1 / base, -exp - 1, acc / base)
      } else {
        pow(base, exp - 1, acc * base)
      }
    }

    if (x < n) {
      pow(x, n)
    }
    else if (x > n) {
      n
    }
    else {
      throw new IllegalArgumentException
    }
  }
}


