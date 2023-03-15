def from(n: Long): Stream[Long] = n #:: from(n + 1)

def pythagoreans(m: Long): List[Vector[Long]] = (m - 1 to 1 by -1).view.map(n => Vector(m * m - n * n, 2 * m * n, m * m + n * n)).toList

def solve(summary: Long): List[Long] = from(2).flatMap(pythagoreans).find(_ match { case Vector(a, b, c) => a + b + c == summary }).get.toList

assert(solve(12).product == 60)
assert(solve(1000).product == 31875000)
