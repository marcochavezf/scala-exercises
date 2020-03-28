package recfun
import scala.annotation.tailrec


object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  // @tailrec
  def pascal(c: Int, r: Int): Int = 
    if (c > r || c < 0 || r < 0) {
      0
    } else if (c == 0 || r == 0) {
      1
    } else {
      pascal(c-1, r-1) + pascal(c, r-1)
    }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def countBalance(count: Int, chars: List[Char]): Boolean = 
      if (count < 0) {
        false
      } else if (chars.isEmpty) {
        count == 0
      } else {
        val delta = if (chars.head == '(') {
          1
        } else if (chars.head == ')') {
          -1
        } else {
          0
        }
        countBalance(count + delta, chars.tail)
      }

    countBalance(0, chars);
}

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty) {
      0
    } else if (money == 0) {
      1
    } else {
      countChange(money - coins.head, coins) + 
      countChange(money, coins.tail)
    }
  }
}
