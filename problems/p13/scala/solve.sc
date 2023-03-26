import scala.io.Source

lazy val dat: Array[Array[Int]] = Source.fromFile("../dat").getLines.map(_.split("").map(_.toInt).reverse.toArray).toArray

def add(xs: Array[Int], ys: Array[Int]): Array[Int] = {
  val size = math.max(xs.size, ys.size)
  val buf = new Array[Int](size + 1)

  val xss = if (xs.size < size) xs ++ new Array[Int](size - xs.size) else xs
  val yss = if (ys.size < size) ys ++ new Array[Int](size - ys.size) else ys

  val zss = xss.zip(yss).zipWithIndex.map { case ((x, y), i) => {
    val summary = x + y
    if (summary < 10) summary
    else {
      buf(i + 1) = 1
      summary - 10
    }
  }}

  if (buf.forall(_ == 0)) zss
  else add(zss, buf)
}

def solve(src: Array[Array[Int]]): Array[Int] = src.reduce(add).reverse.dropWhile(_ == 0)

assert(solve(dat).take(10).mkString("") == "5537376230")
