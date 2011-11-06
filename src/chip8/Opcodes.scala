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

  /// return from subroutine
  def opcode00EE(opcode: Int) =
    {
      cpu.pc = cpu.stack.last;
      cpu.stack.pop
    }

  // jump to address NNN
  def opcode1NNN(opcode: Int) =
    {
      cpu.pc = opcode & 0x0FFF;
    }

  // call subroutine NNN
  def opcode2NNN(opcode: Int) =
    {
      cpu.stack.push(cpu.pc);
      cpu.pc = opcode & 0x0FFF;
    }

  // skip next instruction if VX == NN
  def opcode3XNN(opcode: Int) =
    {
      val nn = opcode & 0x00FF;
      var regx = opcode & 0x0F00;
      regx >>= 8;

      if (cpu.registers(regx).value == nn)
        cpu.pc += 2;
    }

  // skip next instruction if VX != NN
  def opcode4XNN(opcode: Int) =
    {
      val nn = opcode & 0x00FF;
      var regx = opcode & 0x0F00;
      regx >>= 8;

      if (cpu.registers(regx).value != nn)
        cpu.pc += 2;
    }

  // skip next instruction if VX == VY
  def opcode5XY0(opcode: Int) =
    {
      var regx = opcode & 0x0F00;
      regx >>= 8;
      var regy = opcode & 0x00F0;
      regy >>= 4;

      if (cpu.registers(regx).value == cpu.registers(regy).value)
        cpu.pc += 2;
    }

  // sets VX to nn
  def opcode6XNN(opcode: Int) =
    {
      val nn = opcode & 0x00FF;
      var regx = opcode & 0x0F00;
      regx >>= 8;
      cpu.registers(regx).value = nn;
    }

  // adds NN to vx. carry not affected
  def opcode7XNN(opcode: Int) =
    {
      val nn = opcode & 0x00FF;
      var regx = opcode & 0x0F00;
      regx >>= 8;
      cpu.registers(regx) += nn;
    }

  // set vx to vy
  def opcode8XY0(opcode: Int) =
    {
      var regx = opcode & 0x0F00;
      regx >>= 8;
      var regy = opcode & 0x00F0;
      regy >>= 4;

      cpu.registers(regx).value = cpu.registers(regy).value;
    }

  // VX = VX | VY
  def opcode8XY1(opcode: Int) =
    {
      var regx = opcode & 0x0F00;
      regx >>= 8;
      var regy = opcode & 0x00F0;
      regy >>= 4;

      cpu.registers(regx).value = cpu.registers(regx).value | cpu.registers(regy).value;
    }

  // VX = VX & VY
  def opcode8XY2(opcode: Int) =
    {
      var regx = opcode & 0x0F00;
      regx >>= 8;
      var regy = opcode & 0x00F0;
      regy >>= 4;

      cpu.registers(regx).value = cpu.registers(regx).value & cpu.registers(regy).value;
    }

  // VX = VX xor VY
  def opcode8XY3(opcode: Int) =
    {
      var regx = opcode & 0x0F00;
      regx >>= 8;
      var regy = opcode & 0x00F0;
      regy >>= 4;
      cpu.registers(regx).value = cpu.registers(regx).value ^ cpu.registers(regy).value;
    }

  // add vy to vx. set carry to 1 if overflow otherwise 0
  def opcode8XY4(opcode: Int) =
    {
      cpu.registers(0xF).value = 0;
      var regx = opcode & 0x0F00;
      regx >>= 8;
      var regy = opcode & 0x00F0;
      regy >>= 4;

      val value = cpu.registers(regx).value + cpu.registers(regy).value;

      if (value > 255)
        cpu.registers(0xF).value = 1;

      cpu.registers(regx).value = cpu.registers(regx).value + cpu.registers(regy).value;
    }

  // sub vy from vx. set carry to 1 if no borrow otherwise 0
  def opcode8XY5(opcode: Int) =
    {
      cpu.registers(0xF).value = 1;

      var regx = opcode & 0x0F00;
      regx >>= 8;
      var regy = opcode & 0x00F0;
      regy >>= 4;

      if (cpu.registers(regx).value < cpu.registers(regy).value)
        cpu.registers(0xF).value = 0;

      cpu.registers(regx).value = cpu.registers(regx).value - cpu.registers(regy).value;
    }

  // Shifts VX right by one. VF is set to the value of the least significant bit of VX before the shift.
  def opcode8XY6(opcode: Int) =
    {
      var regx = opcode & 0x0F00;
      regx >>= 8;

      cpu.registers(0xF).value = cpu.registers(regx).value & 0x1;
      cpu.registers(regx).value >>= 1;
    }

  // Sets VX to VY minus VX. VF is set to 0 when there's a borrow, and 1 when there isn't.
  def opcode8XY7(opcode: Int) =
    {
      cpu.registers(0xF).value = 1;

      var regx = opcode & 0x0F00;
      regx >>= 8;
      var regy = opcode & 0x00F0;
      regy >>= 4;

      if (cpu.registers(regy).value < cpu.registers(regx).value) {
        cpu.registers(0xF).value = 0;
      }

      cpu.registers(regx).value = cpu.registers(regy).value - cpu.registers(regx).value;
    }

  // Shifts VX left by one. VF is set to the value of the most significant bit of VX before the shift
  def opcode8XYE(opcode: Int) =
    {
      var regx = opcode & 0x0F00;
      regx >>= 8;

      cpu.registers(0xF).value
      cpu.registers(0xF).value = cpu.registers(regx).value >> 7;
      cpu.registers(regx).value <<= 1;

    }

  // skip next instruction if VX != VY
  def opcode9XY0(opcode: Int) =
    {
      var regx = opcode & 0x0F00;
      regx >>= 8;
      var regy = opcode & 0x00F0;
      regy >>= 4;

      if (cpu.registers(regx).value != cpu.registers(regy).value)
        cpu.pc += 2;
    }

  // set I to nnn
  def opcodeANNN(opcode: Int) =
    {

      cpu.registerI.value = (opcode & 0x0FFF);
    }

  // jump to address NNN + V0
  def opcodeBNNN(opcode: Int) =
    {
      val nnn = opcode & 0x0FFF;
      cpu.pc = cpu.registers(0).value + nnn;
      cpu.registers(0).value
    }

  // set vx to rand + NN
  def opcodeCXNN(opcode: Int) =
    {
      val nn = opcode & 0x00FF;
      var regx = opcode & 0x0F00;
      regx >>= 8;
      val r = new Random;
      cpu.registers(regx).value = r.nextInt() & nn;
    }

  def opcodeDXYN(opcode: Int) = {

    var regx = opcode & 0x0F00;
    regx = regx >> 8;
    var regy = opcode & 0x00F0;
    regy = regy >> 4;

    val height = opcode & 0x000F

    val coordx = cpu.registers(regx).value;
    val coordy = cpu.registers(regy).value;

    cpu.registers(0xF).value = 0

    // loop for the amount of vertical lines needed to draw

    for (yline <- 0 until height) {
      val data = cpu.memory.mem(cpu.registerI + yline)
      var xpixelinv = 8;
      val xpixel = 0;
      for (xpixel <- 0 until 8) {
        xpixelinv -= 1
        val mask = 1 << xpixelinv;
        if ((data & mask) != 0) {
          val x = coordx + xpixel;
          val y = coordy + yline;
          if (cpu.gpu.screen(x)(y) == 1) {
            cpu.registers(0xF).value = 1
          }
          cpu.gpu.screen(x)(y) ^= 1;
        }
      }
    }
  }

  //  def opcodeDXYN(opcode: Int) =
  //    {
  //      var regx = opcode & 0x0F00;
  //      regx = regx >> 8;
  //      var regy = opcode & 0x00F0;
  //      regy = regy >> 4;
  //
  //      val height = opcode & 0x000F
  //      val coordx = cpu.registers(regx)
  //      val coordy = cpu.registers(regy);
  //
  //      cpu.registers(0xF).value = 0
  //      for (yline <- 0 until height) {
  //        val data = cpu.memory.mem(cpu.registerI.value + yline)
  //        var xpixelinv = 8;
  //        for (xpixel <- 0 until 8) {
  //          xpixelinv -= 1
  //
  //          val mask = 1 << xpixelinv;
  //          if ((data & mask) != 0) {
  //            val x = coordx + xpixel;
  //            val y = coordy + yline;
  //            if (cpu.gpu.screen(x)(y) == 1)
  //              cpu.registers(0xF).value = 1
  //            cpu.gpu.screen(x)(y) ^= 1;
  //          }
  //        }
  //      }
  //    }

  //	Skips the next instruction if the key stored in VX is pressed.
  def opcodeEX9E(opcode: Int) =
    {
      println("IN EX9E")
      var regx = opcode & 0x0F00;
      regx >>= 8;
      //	int key = cpu.registers(regx).value ;
      //
      //	if (m_KeyState[key] == 1)
      cpu.pc += 2;
    }

  // Skips the next instruction if the key stored in VX isn't pressed.
  def opcodeEXA1(opcode: Int) =
    {
      println("IN EXA1")
      var regx = opcode & 0x0F00;
      regx >>= 8;
      //val key = cpu.registers(regx).value ;
      val key = 1

      //if (m_KeyState[key] == 0)
      cpu.pc += 2;
    }

  // Sets VX to the value of the delay timer.
  def opcodeFX07(opcode: Int) =
    {
      var regx = opcode & 0x0F00;
      regx >>= 8;

      cpu.registers(regx).value = cpu.timer;
    }

  // A key press is awaited, and then stored in VX.
  def opcodeFX0A(opcode: Int) =
    {
      println("IN FX0A")
      var regx = opcode & 0x0F00;
      regx >>= 8;
      //
      //	int keypressed = GetKeyPressed( ) ;
      //
      //	if (keypressed == -1)
      //	{
      //		cpu.pc -= 2 ;
      //	}
      //	else
      //	{
      //		//cpu.registers(regx).value = keypressed ;
      //	}
    }

  // delay to vx
  def opcodeFX15(opcode: Int) =
    {
      var regx = opcode & 0x0F00;
      regx >>= 8;

      cpu.timer = cpu.registers(regx).value;
    }

  // sound to vx
  def opcodeFX18(opcode: Int) =
    {
      var regx = opcode & 0x0F00;
      regx >>= 8;

      //m_SoundTimer = cpu.registers(regx).value ;
    }

  // adds vx to I
  def opcodeFX1E(opcode: Int) =
    {
      var regx = opcode & 0x0F00;
      regx >>= 8;

      cpu.registerI.value += cpu.registers(regx).value;
    }

  //Sets I to the location of the sprite for the character in VX. Characters 0-F (in hexadecimal) are represented by a 4x5 font.
  def opcodeFX29(opcode: Int) =
    {
      var regx = opcode & 0x0F00;
      regx >>= 8;
      cpu.registerI.value = cpu.registers(regx).value * 5;
    }

  //Stores the Binary-coded decimal representation of VX at the addresses I, I plus 1, and I plus 2.
  def opcodeFX33(opcode: Int) =
    {
      var regx = opcode & 0x0F00;
      regx >>= 8;

      val value = cpu.registers(regx).value;

      val hundreds = value / 100;
      val tens = (value / 10) % 10;
      val units = value % 10;

      cpu.memory.mem(cpu.registerI.value) = hundreds
      cpu.memory.mem(cpu.registerI.value + 1) = tens;
      cpu.memory.mem(cpu.registerI.value + 2) = units;
    }

  // Stores V0 to VX in memory starting at address I.
  def opcodeFX55(opcode: Int) =
    {
      var regx = opcode & 0x0F00;
      regx >>= 8;
      for (i <- 0 to regx) {
        cpu.memory.mem(cpu.registerI.value + i) = cpu.registers(i).value
      }
      cpu.registerI.value = cpu.registerI.value + regx + 1
    }

  //Fills V0 to VX with values from memory starting at address I.
  def opcodeFX65(opcode: Int) =
    {
      var regx = opcode & 0x0F00;
      regx >>= 8;

      for (i <- 0 to regx) {
        cpu.registers(i).value = cpu.memory.mem(cpu.registerI.value + i)
      }
      cpu.registerI.value = cpu.registerI.value + regx + 1
    }
}