import scala.io.Source

val dat: String = Source.fromFile("../dat").getLines.mkString("")

def solve(s: String, digit: Int): List[Long] =
  if (s.size < digit) Nil
  else s.take(digit).map(_.asDigit.toLong).product :: solve(s.drop(1), digit)

assert(solve(dat, 4).max == 5832L)
assert(solve(dat, 13).max == 23514624000L)
