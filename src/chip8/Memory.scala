package chip8
import java.io._
class Memory(romFilename: String) {
  
  val mem = loadRom(romFilename)
  
  def loadRom(filename: String) : List[Int] = {
    val is = new FileInputStream(filename)
    List.fill(0x200)(0) ::: Stream.continually(is.read).takeWhile(-1 !=).toList
  }
  
  

}