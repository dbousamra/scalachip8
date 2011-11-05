package chip8
import scala.collection.immutable.Stack
import java.io.File

class Cpu(romFilename: String, DEBUG_MODE: Boolean) {

  val memory: Memory = new Memory(romFilename)
  val opcodes: Opcodes = new Opcodes
  val gpu: Gpu = new Gpu
  val registers = new Array[Register](16)
  val registerI = new Register
  val stack = new Stack[Int]
  var pc = 0x200

  def emulate = while (true) { opcodes.decode(nextOpcode) }

  def reset {
    registerI := 0
    registers.map(r => r := 0)
    memory.loadRom(romFilename)
    gpu.reset
  }

  def nextOpcode: Int = {
    var add = memory.mem(pc)
    add <<= 8
    add |= memory.mem(pc + 1)
    pc += 2
    add
  }

}