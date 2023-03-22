import scala.io.Source

type T = Array[Array[Int]]

lazy val dat: T = Source.fromFile("../dat").getLines.toArray.map(_.split(" ").map(_.toInt))

def height(src: T): Int = src.length
def width(src: T): Int = src(0).length

def within(under: Int, top: Int, value: Int): Int =
  if (value < under) under
  else if (top <= value) top
  else value

def takeAround(src: T, y: Int, x: Int, length: Int): List[Vector[Int]] = {
  val xs = x to within(0, width(src), x + length - 1)
  val ys = y to within(0, height(src), y + length - 1)

  val rights: Vector[Int] = src.lift(y).map(ar => xs.map(x => ar.lift(x)).collect { case Some(n) => n }).getOrElse(Vector()).toVector
  val unders: Vector[Int] = ys.map(y => src.lift(y).map(_.lift(x))).collect { case Some(n) => n.getOrElse(1) }.toVector
  val diagonals: Vector[Int] = ys.zip(xs).map { case (y, x) => src.lift(y).map(_.lift(x)) }.collect { case Some(n) => n.getOrElse(1) }.toVector
  List(rights, unders, diagonals)
}

def solveHelper(src: T, length: Int) = 
  (for (
    y <- 0 to height(src);
    x <- 0 to width(src)
  ) yield takeAround(src, y, x, length)).flatten

def solve(src: T, length: Int): Int = solveHelper(src, length).map(_.product).max

val length = 4
assert(solve(dat, length) == 51267216)
