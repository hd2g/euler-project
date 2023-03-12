def split(s: String): (String, String) = {
  val len = (s.length / 2).toInt
  (s.substring(0, len), s.substring(len + (if (s.length % 2 != 0) { 1 } else { 0 })))
}
assert(split("9119") == ("91", "19"))
assert(split("91019") == ("91", "19"))

def isPalindrome(n: Int): Boolean = {
  val (lhs, rhs) = split(n.toString)
  lhs == rhs.reverse
}
assert(isPalindrome(9119))
assert(isPalindrome(91019))
assert(!isPalindrome(9120))
assert(!isPalindrome(91020))

def solve(digit: Int): List[Int] = {
  val xs = (math.pow(10, digit - 1).toInt to math.pow(10, digit).toInt - 1)
  val rs = for (
    x <- xs;
    y <- xs;
    val n = x * y
    if isPalindrome(n)
  ) yield n
  rs.toList
}
assert(solve(2).last == 9009)
assert(solve(3).last == 580085)
