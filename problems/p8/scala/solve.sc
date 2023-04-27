import scala.io.Source

val dat: List[String] = Source.fromFile("../dat").getLines.toList

def solveHelper(line: String, digit: Int): List[Long] =
  if (line.size < digit) Nil
  else line.take(digit).map(_.asDigit.toLong).product :: solveHelper(line.drop(1), digit)

def solve(lines: List[String], digit: Int): List[Long] = lines.foldLeft(List[Long]())((acc, line) => acc ++ solveHelper(line, digit))

assert(solve(dat, 4).max == 5832L)
assert(solve(dat, 13).max == 5377010688L)
