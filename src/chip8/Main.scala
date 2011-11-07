package chip8

import processing.core._
import java.awt.Dimension

object Main {

  private var emulator: Screen = _

  def main(args: Array[String]): Unit = {
	val mod = 10
    emulator = new Screen
    val frame = new javax.swing.JFrame("scalachip8")
    frame.setPreferredSize(new Dimension(mod * 64, mod * 34))
    frame.getContentPane().add(emulator)
    emulator.init
    frame.pack
    frame.setVisible(true)
  }

}

class Screen extends PApplet {
  val romFilename = "roms/chip8/MERLIN"
  val runner: Cpu = new Cpu(romFilename, DEBUG_MODE = true)
  val mod = 10

  override def setup() = {
    runner.reset
    frameRate(800)
    delay(0)
    size(mod*64, mod*32)
    background(0)
    smooth()
    noStroke()
    fill(255)
  }

  override def draw() = {
    stroke(0)
    background(0)
    
    def drawLines(cpu: Cpu) = {
      for (x <- 0 until 64) {
        for (y <- 0 until 32) {
          if (cpu.gpu.screen(x)(y) == 1) {
            stroke(255, 255, 255)
            rect(x*mod, y*mod, mod, mod)
          }
        }
      }
    }
    drawLines(runner)
    runner.emulate
  }

}