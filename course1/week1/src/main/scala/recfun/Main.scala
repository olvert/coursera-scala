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

      def getRow(r: Int): List[Int] =

        r match {
          case 0 => List(1)
          case 1 => List(1, 1)
          case _ => recRow(r - 1, getRow(1))
        }

      def recRow(n: Int, l: List[Int]): List[Int] =

        n match {
          case 0 => l
          case _ => val le = 1 :: evolve(l) ::: List(1)
                                  recRow(n - 1, le)
        }

      def evolve(l: List[Int]): List[Int] =

        l match {
          case _ :: xs if xs.isEmpty => List()
          case x :: y :: ys =>  val z = x + y
                                val zs = y :: ys
                                z :: evolve(zs)
        }

      getRow(r)(c)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def balanceRec(bal: Int, chars: List[Char]): Boolean = {

        chars match {
          case _ if chars.isEmpty     =>  bal == 0
          case c :: cs if c == '('    =>  balanceRec(bal+1, cs)
          case c :: cs if c == ')'    =>  val correct = bal-1 >= 0
                                          correct && balanceRec(bal-1, cs)
          case _ :: cs                =>  balanceRec(bal, cs)
        }
      }

      balanceRec(0, chars )
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      if (money < 0 || coins.isEmpty) 0
      else if (money == 0)            1
      else  countChange(money - coins.head, coins) +
            countChange(money, coins.tail)
    }
  }
