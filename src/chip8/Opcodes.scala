package chip8
import scala.util.Random

class Opcodes(cpu: Cpu) {
  import cpu._
  import Register._
  def decode(opcode: Int) = opcode & 0xF000 match {
    case 0x0000 => opcode & 0xF match {
      case 0x0000 => clearScreen(opcode)
      case 0x000E => opcode00EE(opcode)
    }
    case 0x1000 => opcode1NNN(opcode)
    case 0x2000 => opcode2NNN(opcode)
    case 0x3000 => opcode3XNN(opcode)
    case 0x4000 => opcode4XNN(opcode)
    case 0x5000 => opcode5XY0(opcode)
    case 0x6000 => opcode6XNN(opcode)
    case 0x7000 => opcode7XNN(opcode)
    case 0x8000 => opcode & 0xF match {
      case 0x0 => opcode8XY0(opcode)
      case 0x1 => opcode8XY1(opcode)
      case 0x2 => opcode8XY2(opcode)
      case 0x3 => opcode8XY3(opcode)
      case 0x4 => opcode8XY4(opcode)
      case 0x5 => opcode8XY5(opcode)
      case 0x6 => opcode8XY6(opcode)
      case 0x7 => opcode8XY7(opcode)
      case 0xE => opcode8XYE(opcode)
    }

    case 0x9000 => opcode9XY0(opcode)
    case 0xA000 => opcodeANNN(opcode)
    case 0xB000 => opcodeBNNN(opcode)
    case 0xC000 => opcodeCXNN(opcode)
    case 0xD000 => opcodeDXYN(opcode)
    case 0xE000 => opcode & 0xF match {
      case 0xE => opcodeEX9E(opcode)
      case 0x1 => opcodeEXA1(opcode)
    }
    case 0xF000 => opcode & 0xFF match {
      case 0x07 => opcodeFX07(opcode)
      case 0x0A => opcodeFX0A(opcode)
      case 0x15 => opcodeFX15(opcode)
      case 0x18 => opcodeFX18(opcode)
      case 0x1E => opcodeFX1E(opcode)
      case 0x29 => opcodeFX29(opcode)
      case 0x33 => opcodeFX33(opcode)
      case 0x55 => opcodeFX55(opcode)
      case 0x65 => opcodeFX65(opcode)
    }
    case _ => None
  }

  def clearScreen(opcode: Int) = {
    cpu.gpu.reset
  }

  def opcode00EE(opcode: Int) =
    {
      cpu.pc = cpu.stack.first
      cpu.stack.pop
    }

  def opcode1NNN(opcode: Int) =
    {
      cpu.pc = opcode & 0x0FFF
    }

  def opcode2NNN(opcode: Int) =
    {
      cpu.stack.push(cpu.pc)
      cpu.pc = opcode & 0x0FFF
    }

  def opcode3XNN(opcode: Int) =
    {
      if (cpu.registers((opcode & 0x0F00) >> 8).value == (opcode & 0x00FF))
        cpu.pc += 2
    }

  def opcode4XNN(opcode: Int) =
    {
      if (cpu.registers((opcode & 0x0F00) >> 8).value != (opcode & 0X00FF))
        cpu.pc += 2
    }

  def opcode5XY0(opcode: Int) =
    {
      if (cpu.registers((opcode & 0x0F00) >> 8).value == cpu.registers((opcode & 0x00F0) >> 4).value)
        cpu.pc += 2
    }

  def opcode6XNN(opcode: Int) = cpu.registers((opcode & 0x0F00) >> 8).value = (opcode & 0x00FF)

  def opcode7XNN(opcode: Int) = cpu.registers((opcode & 0x0F00) >> 8) += (opcode & 0x00FF)

  def opcode8XY0(opcode: Int) = cpu.registers((opcode & 0x0F00) >> 8).value = cpu.registers((opcode & 0x00F0) >> 4).value

  def opcode8XY1(opcode: Int) =
    {
      val regx = (opcode & 0x0F00) >> 8
      cpu.registers(regx).value = cpu.registers(regx).value | cpu.registers((opcode & 0x00F0) >> 4).value
    }

  def opcode8XY2(opcode: Int) =
    {
      val regx = (opcode & 0x0F00) >> 8
      cpu.registers(regx).value = cpu.registers(regx).value & cpu.registers((opcode & 0x00F0) >> 4).value
    }

  def opcode8XY3(opcode: Int) =
    {
      val regx = (opcode & 0x0F00) >> 8
      val regy = (opcode & 0x00F0) >> 4
      cpu.registers(regx).value = cpu.registers(regx).value ^ cpu.registers(regy).value
    }

  def opcode8XY4(opcode: Int) =
    {
      cpu.registers(0xF).value = 0
      val regx = (opcode & 0x0F00) >> 8
      val regy = (opcode & 0x00F0) >> 4

      val value = cpu.registers(regx).value + cpu.registers(regy).value

      if (value > 255)
        cpu.registers(0xF).value = 1

      cpu.registers(regx).value = cpu.registers(regx).value + cpu.registers(regy).value
    }

  def opcode8XY5(opcode: Int) =
    {
      cpu.registers(0xF).value = 1

      var regx = opcode & 0x0F00
      regx >>= 8
      var regy = opcode & 0x00F0
      regy >>= 4

      if (cpu.registers(regx).value < cpu.registers(regy).value)
        cpu.registers(0xF).value = 0

      cpu.registers(regx).value = cpu.registers(regx).value - cpu.registers(regy).value
    }

  def opcode8XY6(opcode: Int) =
    {
      cpu.registers(0xF).value = cpu.registers((opcode & 0x0F00) >> 8).value & 0x1
      cpu.registers((opcode & 0x0F00) >> 8).value >>= 1
    }

  def opcode8XY7(opcode: Int) =
    {
      cpu.registers(0xF).value = 1

      var regx = opcode & 0x0F00
      regx >>= 8
      var regy = opcode & 0x00F0
      regy >>= 4

      if (cpu.registers(regy).value < cpu.registers(regx).value) {
        cpu.registers(0xF).value = 0
      }

      cpu.registers(regx).value = cpu.registers(regy).value - cpu.registers(regx).value
    }

  def opcode8XYE(opcode: Int) =
    {
      var regx = opcode & 0x0F00
      regx >>= 8

      cpu.registers(0xF).value
      cpu.registers(0xF).value = cpu.registers(regx).value >> 7
      cpu.registers(regx).value <<= 1

    }

  def opcode9XY0(opcode: Int) =
    {
      var regx = opcode & 0x0F00
      regx >>= 8
      var regy = opcode & 0x00F0
      regy >>= 4

      if (cpu.registers(regx).value != cpu.registers(regy).value)
        cpu.pc += 2
    }

  def opcodeANNN(opcode: Int) = cpu.registerI.value = (opcode & 0x0FFF)

  def opcodeBNNN(opcode: Int) =
    {
      cpu.pc = cpu.registers(0).value + opcode & 0x0FFF
      cpu.registers(0).value
    }

  def opcodeCXNN(opcode: Int) =
    {
      val r = new Random
      cpu.registers((opcode & 0x0F00) >> 8).value = r.nextInt() & (opcode & 0x00FF)
    }

  def opcodeDXYN(opcode: Int) = {
    val height = opcode & 0x000F
    val coordx = cpu.registers((opcode & 0x0F00) >> 8).value
    val coordy = cpu.registers((opcode & 0x00F0) >> 4).value
    cpu.registers(0xF).value = 0
    for (yline <- 0 until height) {
      val data = cpu.memory.mem(cpu.registerI + yline)
      var xpixelinv = 8
      val xpixel = 0
      for (xpixel <- 0 until 8) {
        xpixelinv -= 1
        val mask = 1 << xpixelinv
        if ((data & mask) != 0) {
          val x = coordx + xpixel
          val y = coordy + yline
          if (cpu.gpu.screen(x & 64)(y & 32) == 1) {
            cpu.registers(0xF).value = 1
          }
          cpu.gpu.screen(x)(y) ^= 1
        }
      }
    }
  }

  def opcodeEX9E(opcode: Int) =
    {
      var regx = opcode & 0x0F00
      regx >>= 8
      //	int key = cpu.registers(regx).value 
      //
      //	if (m_KeyState[key] == 1)
      cpu.pc += 2
    }

  def opcodeEXA1(opcode: Int) =
    {
      val regx = (opcode & 0x0F00) >> 8
      //val key = cpu.registers(regx).value 
      val key = 1

      //if (m_KeyState[key] == 0)
      cpu.pc += 2
    }

  def opcodeFX07(opcode: Int) = cpu.registers((opcode & 0x0F00) >> 8).value = cpu.timer

  def opcodeFX0A(opcode: Int) =
    {
      var regx = (opcode & 0x0F00) >> 8
      //
      //	int keypressed = GetKeyPressed( ) 
      //
      //	if (keypressed == -1)
      //	{
      //		cpu.pc -= 2 
      //	}
      //	else
      //	{
      //		//cpu.registers(regx).value = keypressed 
      //	}
    }

  def opcodeFX15(opcode: Int) = cpu.timer = cpu.registers((opcode & 0x0F00) >> 8).value

  def opcodeFX18(opcode: Int) =
    {
      var regx = (opcode & 0x0F00) >> 8

      //m_SoundTimer = cpu.registers(regx).value 
    }

  def opcodeFX1E(opcode: Int) = cpu.registerI.value += cpu.registers((opcode & 0x0F00) >> 8).value

  def opcodeFX29(opcode: Int) = cpu.registerI.value = cpu.registers((opcode & 0x0F00) >> 8).value * 5

  def opcodeFX33(opcode: Int) =
    {
      val value = cpu.registers((opcode & 0x0F00) >> 8).value
      val hundreds = value / 100
      val tens = (value / 10) % 10
      val units = value % 10

      cpu.memory.mem(cpu.registerI.value) = hundreds
      cpu.memory.mem(cpu.registerI.value + 1) = tens
      cpu.memory.mem(cpu.registerI.value + 2) = units
    }

  def opcodeFX55(opcode: Int) =
    {
      var regx = (opcode & 0x0F00) >> 8
      for (i <- 0 to regx) {
        cpu.memory.mem(cpu.registerI.value + i) = cpu.registers(i).value
      }
      cpu.registerI.value = cpu.registerI.value + regx
    }

  def opcodeFX65(opcode: Int) =
    {
      val regx = opcode & 0x0F00 >> 8
      for (i <- 0 to regx) {
        cpu.registers(i).value = cpu.memory.mem(cpu.registerI.value + i)
      }
      cpu.registerI.value = cpu.registerI.value + regx + 1
    }
}