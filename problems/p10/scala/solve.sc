def isPrime(n: Long): Boolean =
  if (n == 2 || n == 3) true
  else if (n % 2 == 0) false
  else (3 to math.sqrt(n).toInt).forall(n % _ != 0)

def from(n: Long, step: Long = 1): Stream[Long] = n #:: from(n + step, step)

lazy val primes: Stream[Long] = 2L #:: 3L #:: (5L #:: from(7L, 2L)).filter(n => !primes.takeWhile(p => p * p <= n).view.exists(p => n % p == 0))

def solve(below: Long): Long = primes.takeWhile(_ < below).sum

assert(solve(10) == 17L)
assert(solve(2000000) == 142913828922L)
