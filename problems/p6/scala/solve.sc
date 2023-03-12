def solve(n: Int): Int = {
  val ns = (1 to n)
  math.pow(ns.sum, 2).toInt - ns.map(math.pow(_, 2).toInt).sum
}

assert(solve(10) == 2640)
assert(solve(100) == 25164150)
