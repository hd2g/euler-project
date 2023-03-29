def collatz(n: Long): List[Long] =
  n :: (if (n == 1) Nil
  else if (n % 2 == 0) {
    collatz(n / 2)
  }
  else {
    collatz(3 * n + 1)
  })

def countOfCollatz(n: Long): Int = {
  def go(col: Long, acc: Int): Int =
    if (col == 1) acc + 1
    else if (col % 2 == 0) go(col / 2, acc + 1)
    else go(3 * col + 1, acc + 1)

  go(n, 0)
}

def solve(limit: Long) = (1L to limit).view.maxBy(countOfCollatz)

assert(solve(1_000_000) == 837799L)
