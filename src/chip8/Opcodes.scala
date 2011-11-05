package chip8

class Opcodes(cpu: Cpu) {
  import cpu._

  def decode(opcode: Int) = opcode & 0xF000 match {
    case 0x0000 => opcode & 0xF match {
      case 0x0000 => None
      case 0x000E => None
    }
    case 0x1000 => None
    case 0x2000 => None
    case 0x3000 => None
    case 0x5000 => None
    case 0x6000 => None
    case 0x7000 => None
    case 0x8000 => opcode & 0xF match {
      case 0x0 => None
      case 0x1 => None
      case 0x2 => None
      case 0x3 => None
      case 0x4 => None
      case 0x5 => None
      case 0x6 => None
      case 0x7 => None
      case 0xE => None
    }
    
    case 0x9000 => None
    case 0xA000 => None
    case 0xB000 => None
    case 0xC000 => None
    case 0xE000 => opcode & 0xF match {
      case 0xE => None
      case 0x1 => None
    }
    case 0xF000 => opcode & 0xFF match {
      case 0x07 => None
      case 0x0A => None
      case 0x15 => None
      case 0x18 => None
      case 0x1E => None
      case 0x29 => None
      case 0x33 => None
      case 0x55 => None
      case 0x65 => None
    }
    case _ => None
  }
  
  
/// return from subroutine
def opcode00EE( )
{
	m_ProgramCounter = m_Stack.back( ) ;
	m_Stack.pop_back( ) ;
}



// jump to address NNN
def opcode1NNN(opcode: Int) =
{
	m_ProgramCounter = opcode & 0x0FFF ;
}



// call subroutine NNN
def opcode2NNN(opcode: Int) =
{
	m_Stack.push_back(m_ProgramCounter) ;
	m_ProgramCounter = opcode & 0x0FFF ;
}



// skip next instruction if VX == NN
def opcode3XNN(opcode: Int) =
{
	int nn = opcode & 0x00FF ;
	int regx = opcode & 0x0F00 ;
	regx >>= 8 ;

	if (m_Registers[regx] == nn)
		m_ProgramCounter += 2 ;
}



// skip next instruction if VX != NN
def opcode4XNN(opcode: Int) =
{
	int nn = opcode & 0x00FF ;
	int regx = opcode & 0x0F00 ;
	regx >>= 8 ;

	if (m_Registers[regx] != nn)
		m_ProgramCounter += 2 ;
}



// skip next instruction if VX == VY
def opcode5XY0(opcode: Int) =
{
	int regx = opcode & 0x0F00 ;
	regx >>= 8 ;
	int regy = opcode & 0x00F0 ;
	regy >>= 4 ;

	if (m_Registers[regx] == m_Registers[regy])
		m_ProgramCounter += 2 ;
}



// sets VX to nn
def opcode6XNN(opcode: Int) =
{
	int nn = opcode & 0x00FF ;
	int regx = opcode & 0x0F00 ;
	regx >>= 8 ;

	m_Registers[regx] = nn;
}



// adds NN to vx. carry not affected
def opcode7XNN(opcode: Int) =
{
	int nn = opcode & 0x00FF ;
	int regx = opcode & 0x0F00 ;
	regx >>= 8 ;

	m_Registers[regx] += nn;
}



// set vx to vy
def opcode8XY0(opcode: Int) =
{
	int regx = opcode & 0x0F00 ;
	regx >>= 8 ;
	int regy = opcode & 0x00F0 ;
	regy >>= 4 ;

	m_Registers[regx] = m_Registers[regy] ;
}



// VX = VX | VY
def opcode8XY1(opcode: Int) =
{
	int regx = opcode & 0x0F00 ;
	regx >>= 8 ;
	int regy = opcode & 0x00F0 ;
	regy >>= 4 ;

	m_Registers[regx] = m_Registers[regx] | m_Registers[regy] ;
}



// VX = VX & VY
def opcode8XY2(opcode: Int) =
{
	int regx = opcode & 0x0F00 ;
	regx >>= 8 ;
	int regy = opcode & 0x00F0 ;
	regy >>= 4 ;

	m_Registers[regx] = m_Registers[regx] & m_Registers[regy] ;
}



// VX = VX xor VY
def opcode8XY3(opcode: Int) =
{
	int regx = opcode & 0x0F00 ;
	regx >>= 8 ;
	int regy = opcode & 0x00F0 ;
	regy >>= 4 ;

	m_Registers[regx] = m_Registers[regx] ^ m_Registers[regy] ;
}



// add vy to vx. set carry to 1 if overflow otherwise 0
def opcode8XY4(opcode: Int) =
{
	m_Registers[0xF] = 0 ;
	int regx = opcode & 0x0F00 ;
	regx >>= 8 ;
	int regy = opcode & 0x00F0 ;
	regy >>= 4 ;

	int value = m_Registers[regx] + m_Registers[regy] ;

	if (value > 255)
		m_Registers[0xF] = 1 ;

	m_Registers[regx] = m_Registers[regx] + m_Registers[regy] ;
}



// sub vy from vx. set carry to 1 if no borrow otherwise 0
def opcode8XY5(opcode: Int) =
{
	m_Registers[0xF] = 1 ;

	int regx = opcode & 0x0F00 ;
	regx >>= 8 ;
	int regy = opcode & 0x00F0 ;
	regy >>= 4 ;

	if (m_Registers[regx] < m_Registers[regy])
		m_Registers[0xF] = 0 ;

	m_Registers[regx] = m_Registers[regx] - m_Registers[regy] ;
}



// Shifts VX right by one. VF is set to the value of the least significant bit of VX before the shift.
def opcode8XY6(opcode: Int) =
{
	int regx = opcode & 0x0F00 ;
	regx >>= 8 ;

	m_Registers[0xF] = m_Registers[regx] & 0x1 ;
	m_Registers[regx] >>= 1 ;
}



// Sets VX to VY minus VX. VF is set to 0 when there's a borrow, and 1 when there isn't.
def opcode8XY7(opcode: Int) =
{
	m_Registers[0xF] = 1 ;

	int regx = opcode & 0x0F00 ;
	regx >>= 8 ;
	int regy = opcode & 0x00F0 ;
	regy >>= 4 ;

	if (m_Registers[regy] < m_Registers[regx])
		m_Registers[0xF] = 0 ;

	m_Registers[regx] = m_Registers[regy] - m_Registers[regx] ;
}



// Shifts VX left by one. VF is set to the value of the most significant bit of VX before the shift
def opcode8XYE(opcode: Int) =
{
	int regx = opcode & 0x0F00 ;
	regx >>= 8 ;

	m_Registers[0xF] = m_Registers[regx] >> 7 ;
	m_Registers[regx] <<= 1 ;

}


// skip next instruction if VX != VY
def opcode9XY0(opcode: Int) =
{
	int regx = opcode & 0x0F00 ;
	regx >>= 8 ;
	int regy = opcode & 0x00F0 ;
	regy >>= 4 ;

	if (m_Registers[regx] != m_Registers[regy])
		m_ProgramCounter += 2 ;
}



// set I to nnn
def opcodeANNN(opcode: Int) =
{
	m_AddressI = opcode & 0x0FFF ;
}



// jump to address NNN + V0
def opcodeBNNN(opcode: Int) =
{
	int nnn = opcode & 0x0FFF ;
	m_ProgramCounter = m_Registers[0] + nnn ;
}



// set vx to rand + NN
def opcodeCXNN(opcode: Int) =
{
	int nn = opcode & 0x00FF ;
	int regx = opcode & 0x0F00 ;
	regx >>= 8 ;

	m_Registers[regx] = rand() & nn ;
}



// Draws a sprite at coordinate (VX, VY) that has a width of 8 pixels and a height of N pixels.
// As described above, VF is set to 1 if any screen pixels are flipped from set to unset when the sprite is drawn,
// and to 0 if that doesn't happen
def opcodeDXYN(opcode: Int) =
{
	const int SCALE = 10 ;
	int regx = opcode & 0x0F00 ;
	regx = regx >> 8 ;
	int regy = opcode & 0x00F0 ;
	regy = regy >> 4 ;

	int coordx = m_Registers[regx] * SCALE;
	int coordy = m_Registers[regy] * SCALE ;
	int height = opcode & 0x000F ;

	m_Registers[0xf] = 0 ;

	for (int yline = 0; yline < height; yline++)
	{
		// this is the data of the sprite stored at m_GameMemory[m_AddressI]
		// the data is stored as a line of bytes so each line is indexed by m_AddressI + yline
		BYTE data = (m_GameMemory[m_AddressI+yline]);

		// for each of the 8 pixels in the line
		int xpixel = 0 ;
		int xpixelinv = 7 ;
		for(xpixel = 0; xpixel < 8; xpixel++, xpixelinv--)
		{
			
			// is ths pixel set to 1? If so then the code needs to toggle its state
			int mask = 1 << xpixelinv ;
			if (data & mask)
			{
				int x = (xpixel*SCALE) + coordx ;
				int y = coordy + (yline*SCALE) ;

				int colour = 0 ;

				// a collision has been detected
				if (m_ScreenData[y][x][0] == 0)
				{
					colour = 255 ;
					m_Registers[15]=1;
				}

				// colour the pixel
				for (int i = 0; i < SCALE; i++)
				{
					for (int j = 0; j < SCALE; j++)
					{
						m_ScreenData[y+i][x+j][0] = colour ;
						m_ScreenData[y+i][x+j][1] = colour ;
						m_ScreenData[y+i][x+j][2] = colour ;
					}
				}

			}
		}
	}
}



//	Skips the next instruction if the key stored in VX is pressed.
def opcodeEX9E(opcode: Int) =
{
	int regx = opcode & 0x0F00 ;
	regx >>= 8 ;
	int key = m_Registers[regx] ;

	if (m_KeyState[key] == 1)
		m_ProgramCounter+=2 ;
}



// Skips the next instruction if the key stored in VX isn't pressed.
def opcodeEXA1(opcode: Int) =
{
	int regx = opcode & 0x0F00 ;
	regx >>= 8 ;
	int key = m_Registers[regx] ;

	if (m_KeyState[key] == 0)
		m_ProgramCounter+=2 ;
}



// Sets VX to the value of the delay timer.
def opcodeFX07(opcode: Int) =
{
	int regx = opcode & 0x0F00 ;
	regx >>= 8 ;

	m_Registers[regx] = m_DelayTimer ;
}



// A key press is awaited, and then stored in VX.
def opcodeFX0A(opcode: Int) =
{
	int regx = opcode & 0x0F00 ;
	regx >>= 8 ;

	int keypressed = GetKeyPressed( ) ;

	if (keypressed == -1)
	{
		m_ProgramCounter -= 2 ;
	}
	else
	{
		m_Registers[regx] = keypressed ;
	}
}



// delay to vx
def opcodeFX15(opcode: Int) =
{
	int regx = opcode & 0x0F00 ;
	regx >>= 8 ;

	m_DelayTimer = m_Registers[regx] ;
}



// sound to vx
def opcodeFX18(opcode: Int) =
{
	int regx = opcode & 0x0F00 ;
	regx >>= 8 ;

	m_SoundTimer = m_Registers[regx] ;
}



// adds vx to I
def opcodeFX1E(opcode: Int) =
{
	int regx = opcode & 0x0F00 ;
	regx >>= 8 ;

	m_AddressI += m_Registers[regx] ;
}



//Sets I to the location of the sprite for the character in VX. Characters 0-F (in hexadecimal) are represented by a 4x5 font.
def opcodeFX29(opcode: Int) =
{
	int regx = opcode & 0x0F00 ;
	regx >>= 8 ;
	m_AddressI = m_Registers[regx]*5;
}



//Stores the Binary-coded decimal representation of VX at the addresses I, I plus 1, and I plus 2.
def opcodeFX33(opcode: Int) =
{
	int regx = opcode & 0x0F00 ;
	regx >>= 8 ;

	int value = m_Registers[regx] ;

	int hundreds = value / 100 ;
	int tens = (value / 10) % 10 ;
	int units = value % 10 ;

	m_GameMemory[m_AddressI] = hundreds ;
	m_GameMemory[m_AddressI+1] = tens ;
	m_GameMemory[m_AddressI+2] = units ;
}



// Stores V0 to VX in memory starting at address I.
def opcodeFX55(opcode: Int) =
{
	int regx = opcode & 0x0F00 ;
	regx >>= 8 ;

	for (int i = 0 ; i <= regx; i++)
	{
		m_GameMemory[m_AddressI+i] = m_Registers[i] ;
	}

	m_AddressI= m_AddressI+regx+1 ;
}



//Fills V0 to VX with values from memory starting at address I.
def opcodeFX65(opcode: Int) =
{
	int regx = opcode & 0x0F00 ;
	regx >>= 8 ;

	for (int i = 0 ; i <= regx; i++)
	{
		m_Registers[i] = m_GameMemory[m_AddressI+i]  ;
	}

	m_AddressI= m_AddressI+regx+1 ;
}

  
}