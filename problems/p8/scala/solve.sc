import scala.io.Source

val dat: String = Source.fromFile("../dat").getLines.mkString("")

def solve(s: String, digit: Int): List[Int] =
  if (s.size < digit) Nil
  else s.take(digit).map(_.asDigit).product :: solve(s.drop(1), digit)

assert(solve(dat, 4).max == 5832)
assert(solve(dat, 13).max == 2091059712)
