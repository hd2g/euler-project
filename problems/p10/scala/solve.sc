def isPrime(n: Long): Boolean =
  if (n == 2L || n == 3L) true
  else if (n % 2L == 0) false
  else !(3L to math.sqrt(n).toLong by 2L).exists(n % _ == 0)

def from(n: Long, s: Long): Stream[Long] = n #:: from(n + s, s)

lazy val primes: Stream[Long] = 2L #:: 3L #:: (5L #:: from(7L, 2L)).filter(n => !primes.takeWhile(prime => prime * prime <= n).exists(prime => n % prime == 0))

def solve(n: Long): Long = primes.takeWhile(_ < n).sum

assert(solve(10) == 17)
assert(solve(2000000) == 142913828922L)
