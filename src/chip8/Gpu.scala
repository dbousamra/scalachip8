package chip8

class Gpu {

  class Screen(x: Int, y: Int) {
    val s = Array.ofDim[Int](x, y)
    def apply(x: Int) = s(x)
    def update(x: Int, y: Int, value: Int) { s(x)(y) = value }
  }

  val width = 65
  val height = 33	
  val screen = new Screen(width, height)

  def reset() = {
    for {
      rows <- screen.s.indices
      cols <- screen.s(rows).indices
    } screen.s(rows)(cols) = 0
  }
}