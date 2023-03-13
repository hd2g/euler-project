def isPythagorean(a: Int, b: Int, c: Int): Boolean = a * a + b * b == c * c

def solve(summary: Int): List[Int] = (1 to summary / 2)
  .combinations(3)
  .filter(_ match { case Vector(a, b, c) =>
    isPythagorean(a, b, c) && a + b + c == summary
  })
  .toList
  .lift(0)
  .fold(List[Int]())(_.toList)

// a = 3, b = 4, c = 5, abc = 60
assert(solve(12).product == 60)
assert(solve(1000).product == 31875000)
