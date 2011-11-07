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
  var timer = 0
  val screenModifier = 8

  def emulate = {
    import Register._
    val nextOpc = nextOpcode
    //println(pc.toHexString + " " + nextOpc.toHexString + " Registers: " + " RegI: " + registerI.value.toHexString + " " + debug)
    opcodes.decode(nextOpc)
  }

  def debug = "      " + registers(0).value.toHexString + " " +
    registers(1).value.toHexString + " " +
    registers(2).value.toHexString + " " +
    registers(3).value.toHexString + " " +
    registers(4).value.toHexString + " " +
    registers(5).value.toHexString + " " +
    registers(6).value.toHexString + " " +
    registers(7).value.toHexString + " " +
    registers(8).value.toHexString + " " +
    registers(9).value.toHexString + " " +
    registers(10).value.toHexString + " " +
    registers(11).value.toHexString + " " +
    registers(12).value.toHexString + " " +
    registers(13).value.toHexString + " " +
    registers(14).value.toHexString + " " +
    registers(15).value.toHexString

  def reset {
    registerI := 0
    pc = 0x200
    registers = List.fill[Register](16)(new Register(0))
    memory.loadRom(romFilename)
    timer = 0
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