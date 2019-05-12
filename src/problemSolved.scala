/*
# In number theory, an abundant number or excessive number is a number for which the sum of its proper divisors is
# greater than the number itself. The integer 12 is the first abundant number. Its proper divisors are 1, 2, 3, 4 and 6
# for a total of 16. The amount by which the sum exceeds the number is the abundance. The number 12 has an abundance of
# 4, for example. Implement the function calculate_sum() that returns the sum of all numbers that are NOT the sum of two
# abundant numbers, knowing that there isn't one over 28123. Note: The number 24 is not included in the sum as it's the
# result of 12+12.
# 4179871
*/

object problemSolved {
  
  import scala.collection.mutable.ListBuffer
  
  def abundant(x: Long): Boolean = {
    (for ( i <- 2 to x.toInt / 2 if x % i == 0 ) yield i).sum + 1 > x
  }
  def checkIfSum(lst: ListBuffer[Int], k: Int): Boolean = {
    var a = false
    for ( i <- lst.indices ) {
      if ( lst contains (k - i) ) a = true
    }
    a
  }
  var listAbundant = new ListBuffer[Int]
  for ( i <- 1 to 28123 if abundant(i) ) {
    listAbundant += i
  }
  def calculate_sum(): Int = {
    (for ( i <- 0 to 28123 if !checkIfSum(listAbundant, i) ) yield i).sum
  }
  def main(args: Array[String]): Unit = {
    
    println(calculate_sum())
  }
  
}
