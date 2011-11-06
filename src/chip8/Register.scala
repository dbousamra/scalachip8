package chip8

class Register(private var _value:Int = 0) {
  def value = _value
  def value_=(i: Int) = _value = i
  def hex : String = value.toHexString
  def +(i:Int) = new Register(value + i)
  def :=(i:Int) = value = i
  def +=(i:Int) = value = (value + i) & 255
  def -=(i:Int) = value -= (value - i) & 255
  def ==(i:Int) = value == i
  def <<(i:Int) = value << (value << i) & 255
  def >>(i:Int) = value >> (value >> i) & 255
  def &=(i:Int) = value &= (value & i) & 255
  def |=(i:Int) = value |= (value | i) & 255
  def ^=(i:Int) = value ^= (value - i) & 255
}

object Register {
	implicit def reg2int(r:Register):Int = r.value 
}