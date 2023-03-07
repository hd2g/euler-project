def solve(n: Int): Int = (1 until n).view.filter(x => x % 3 == 0 || x % 5 == 0).sum

assert(solve(10) == 23)
assert(solve(1000) == 233168)
