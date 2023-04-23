type Axis = (Int, Int)

def nextAxis(from: Axis, to: Axis, dy: Int, dx: Int): Option[Axis] = {
  val (y, x) = from
  val (h, w) = to
  val yy = y + dy
  val xx = x + dx
  if (yy <= h && xx <= w) Some((yy, xx))
  else None
}

def route(from: Axis, to: Axis, ds: List[Axis]): List[Axis] = ???

def routes(from: Axis, to: Axis): List[Vector[Axis]] = ???

def solve(hw: Axis): List[Vector[Axis]] = ???

def tests() = {
  solve((2,2)).size == 6
}
