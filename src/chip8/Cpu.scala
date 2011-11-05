package chip8
import scala.collection.immutable.Stack
import java.io.File
import scala.collection.mutable.Stack

class Cpu(romFilename: String, DEBUG_MODE: Boolean) {

  val memory: Memory = new Memory(romFilename)
  val opcodes: Opcodes = new Opcodes(this)
  val gpu: Gpu = new Gpu
  var registers = List[Register]()
  val registerI = new Register
  val stack = new scala.collection.mutable.Stack[Int]
  var pc = 0x200

  def emulate = while (true) { 
    
    val nextOpc = nextOpcode
    println(pc.toHexString + " " + nextOpc.toHexString)
    opcodes.decode(nextOpc) 
    }

  def reset {
    registerI := 0
    pc = 0x200
    registers = List.fill[Register](16)(new Register(0))
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