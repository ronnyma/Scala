package recfun

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
    if (c == r || c == 0) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def loop(chars: List[Char], stack: List[Char]): Boolean = {
      if (stack.isEmpty && chars.isEmpty) true else {
        if ((chars.head == '(' && chars.tail.isEmpty) || (chars.head == ')' && stack.isEmpty)) false else {
          if (chars.head == '(') loop(chars.tail, chars.head :: stack)
          else if (chars.head == ')') loop(chars.tail, stack.tail)
          else loop(chars.tail, stack)
        }
      }
    }
    loop(chars, Nil)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0) 1 else {
      if(coins.isEmpty || money < 0) 0 else {
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
      }
    }
  }
}
