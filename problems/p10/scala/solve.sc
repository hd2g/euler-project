// def isPrime(n: Long): Boolean =
//   if (n == 2 || n == 3) true
//   else if (n % 2 == 0) false
//   else (3 to math.sqrt(n).toInt).forall(n % _ != 0)

// def from(n: Long, step: Long = 1): Stream[Long] = n #:: from(n + step, step)

// lazy val primes: Stream[Long] = 2L #:: 3L #:: (5L #:: from(7L, 2L)).filter(n => !primes.takeWhile(p => p * p <= n).view.exists(p => n % p == 0))

def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

def primesHelper: Stream[Long] = {
  def f(m: Long, s: Long, pss: Stream[Long]): Stream[Long] = pss match { case p #:: ps =>
    lazy val ns: Stream[Long] = for (
      x <- Stream.from(s + 6 to p * p - 2 by 6);
      y <- 0L #:: 4L #:: Stream.Empty
    ) yield x + y
    Stream.concat(ns.filter(n => gcd(m, n) == 1L), f(m * p, p * p, ps))
  }
  5L #:: f(1L, 7L, primesHelper)
}

lazy val primes: Stream[Long] = 2L #:: 3L #:: primesHelper

// def solve(below: Long): Long = primes.takeWhile(_ < below).sum

// assert(solve(10) == 17L)
// assert(solve(2000000) == 142913828922L)
