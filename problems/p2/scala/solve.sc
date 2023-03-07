lazy val fibs: Stream[Int] = 0 #:: fibs.scanLeft(1)(_+_)

def solve(n: Int): Int = fibs.view.takeWhile(_ <= n).filter(_ % 2 == 0).sum

assert(solve(4000000) == 4613732)
