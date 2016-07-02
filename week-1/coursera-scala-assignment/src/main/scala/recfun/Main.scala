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
  def pascal(c: Int, r: Int): Int =
    if(r - 1 < 0 || c - 1 < 0 || c + 1 > r) 1 else pascal(c - 1 , r - 1) + pascal(c , r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def lookForOpenBrasket(chars: List[Char]) : Boolean = {
      if(chars.isEmpty) true
      else if (chars.head == ')') false
      else if (chars.head == '(') lookForCloseBraskets(chars.tail, 1)
      else lookForOpenBrasket(chars.tail)
    }

    def lookForCloseBraskets(chars: List[Char], nbr : Int) : Boolean = {
      if(nbr == 0) lookForOpenBrasket(chars)
      else if(chars.isEmpty) false
      else if (chars.head == '(') lookForCloseBraskets(chars.tail, nbr + 1)
      else if (chars.head == ')') lookForCloseBraskets(chars.tail, nbr - 1)
      else lookForCloseBraskets(chars.tail, nbr)
    }

    lookForOpenBrasket(chars)
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money > 0 && coins.isEmpty) 0
    else if (money == 0 && coins.isEmpty) 1
    else if (money - coins.head < 0) countChange(money, coins.tail)
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
