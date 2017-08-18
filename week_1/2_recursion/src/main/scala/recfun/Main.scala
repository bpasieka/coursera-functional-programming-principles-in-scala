package recfun

object Main {
  def main(args: Array[String]): Unit = {

  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int =
      if (r < 0 || c < 0 || c > r) 0
      else if (c == 0) 1
      else pascal(c, r-1) + pascal(c-1, r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def checkBalance(chars: List[Char], stack: List[Char]): Boolean =
        if (chars.isEmpty) stack.isEmpty
        else if (chars.head.toString == "(")
          checkBalance(chars.tail, chars.head :: stack)
        else if (chars.head.toString == ")")
          stack.nonEmpty && stack.head.toString == "(" && checkBalance(chars.tail, stack.tail)
        else checkBalance(chars.tail, stack)

      checkBalance(chars, List[Char]())
    }
  
  /**
   * Exercise 3
   * I cheated and read how to solve this problem ^^
   */
    def countChange(money: Int, coins: List[Int]): Int =
      if (money == 0) 1
      else if (money > 0 && coins.nonEmpty)
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
      else 0
  }
