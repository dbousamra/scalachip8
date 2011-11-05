package chip8
import java.io._
class Memory(romFilename: String) {

  val mem = loadRom(romFilename)

  def loadRom(filename: String): Array[Int] = {
    val is = new FileInputStream(filename)
    Array.concat(Array.fill(0x200)(0), Stream.continually(is.read).takeWhile(-1 !=).toArray)
  }

}