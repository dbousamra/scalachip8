package chip8

object Main {
  
  def main(args: Array[String]): Unit = {
    val romFilename = "roms/PONG"
    val runner: Cpu = new Cpu(romFilename, DEBUG_MODE = true)
    runner.reset
    runner.emulate
  }
  
}