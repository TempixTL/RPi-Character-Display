import com.pi4j.io.gpio._

val Command = false
val Data = true

val DATA_INST_Pin = RaspiPin.GPIO_00 //Data/Instructions
val MOSI_Pin = RaspiPin.GPIO_12 //Master out slave in
val SCK_Pin = RaspiPin.GPIO_14 //Serial clock
val SS_Pin = RaspiPin.GPIO_07 //Slave select

//Initialize GPIO and pins
val gpio = GpioFactory.getInstance()
val regSelect = gpio.provisionDigitalOutputPin(DATA_INST_Pin, PinState.LOW)
val mosi = gpio.provisionDigitalOutputPin(MOSI_Pin, PinState.LOW)
val clock = gpio.provisionDigitalOutputPin(SCK_Pin, PinState.HIGH)
val slaveSelect = gpio.provisionDigitalOutputPin(SS_Pin, PinState.HIGH)

val display = new Display(regSelect, mosi, clock, slaveSelect)
display.setup()

//Drawing fun!

//Smiley face
display.writeCGRAM(0, 1, 0x0A)
display.writeCGRAM(0, 4, 0x11)
display.writeCGRAM(0, 5, 0x0E)

//Star
display.writeCGRAM(1, 1, 0x04)
display.writeCGRAM(1, 2, 0x1F)
display.writeCGRAM(1, 3, 0x0E)
display.writeCGRAM(1, 4, 0x0E)
display.writeCGRAM(1, 5, 0x1B)

//Pokeball
display.writeCGRAM(2, 3, 0x01)
display.writeCGRAM(2, 4, 0x06)
display.writeCGRAM(2, 5, 0x08)
display.writeCGRAM(2, 6, 0x09)
display.writeCGRAM(2, 7, 0x1F)

display.writeCGRAM(3, 3, 0x10)
display.writeCGRAM(3, 4, 0x0C)
display.writeCGRAM(3, 5, 0x02)
display.writeCGRAM(3, 6, 0x12)
display.writeCGRAM(3, 7, 0x1F)

display.writeCGRAM(4, 0, 0x1F)
display.writeCGRAM(4, 1, 0x09)
display.writeCGRAM(4, 2, 0x08)
display.writeCGRAM(4, 3, 0x06)
display.writeCGRAM(4, 4, 0x01)

display.writeCGRAM(5, 0, 0x1F)
display.writeCGRAM(5, 1, 0x12)
display.writeCGRAM(5, 2, 0x02)
display.writeCGRAM(5, 3, 0x0C)
display.writeCGRAM(5, 4, 0x10)

//Hi smiley
display.writeCGRAM(6, 0, 0x15)
display.writeCGRAM(6, 1, 0x1D)
display.writeCGRAM(6, 2, 0x15)
display.writeCGRAM(6, 4, 0x0A)
display.writeCGRAM(6, 6, 0x11)
display.writeCGRAM(6, 7, 0x0E)

//Apple logo
display.writeCGRAM(7, 1, 0x02)
display.writeCGRAM(7, 2, 0x04)
display.writeCGRAM(7, 3, 0x1B)
display.writeCGRAM(7, 4, 0x1E)
display.writeCGRAM(7, 5, 0x1F)
display.writeCGRAM(7, 6, 0x0E)
display.returnHome()

display.writeString("Custom charset:")
display.positionCursor(0, 1)
for(character <- 0 to 7) {
  display.writeData(character)
  display.writeData(' ')
}

while (true) {
  for (_ <- 0 until 64) {
    display.shiftDisplayLeft()
    Thread.sleep(100)
  }
  Thread.sleep(3000)
}

gpio.shutdown()

class Display(val regSelect: GpioPinDigitalOutput, val mosi: GpioPinDigitalOutput, val clock: GpioPinDigitalOutput, val slaveSelect: GpioPinDigitalOutput) {

  def writeString(str: String) = {
    str.foreach(c => writeData(c.toInt))
  }

  def delayedWriteString(str: String, delay: Int = 300) = {
    str.foreach(c => {
      writeData(c.toInt)
      Thread.sleep(delay)
    })
  }

  def positionCursor(x: Int, y: Int) {
    //Usage: positionCursor(7, 1)
    //  This would put the cursor at the 4th character position from the left on the bottom line

    //Sending binary command 1000 0000
    //Command signature      1??? ????
    //  ? = Position of cursor
    //Last seven digits correspond to a number from 0x0 to 0x7F (000 0000 to 111 1111)
    //0x0 to 0x3F (000 0000 to 011 1111) are x positions at y = 0
    // 0x40 to 0x7F (100 0000 to 111 1111) are x positions at y = 1
    writeCommand(0x80 + x + (if(y >= 1) 0x40 else 0))
  }

  //Setup using given parameters
  //  fontNumber: id of font to initialize to (0-3)
  //  incrementEntry: true = increment cursor after a character is written to screen, false = dont
  //  scrollEntry: (only valid if incrementEntry = true) true = scroll entire display left at each character input, false = scroll entire display right
  //  displayMode: true = graphic mode, false = character mode
  def setup(fontNumber: Int = 0, incrementEntry: Boolean = true, scrollEntry: Boolean = false, displayMode: Boolean = false) = {
    emulatePowerOn(fontNumber, incrementEntry, scrollEntry, displayMode)
    initialize(fontNumber, incrementEntry, scrollEntry)
    clearCGRAM()
    returnHome()
  }

  private def initialize(fontNumber: Int, incrementEntry: Boolean, scrollEntry: Boolean) {
    //Display controller says power stabilization = 500ms
    Thread.sleep(500)

    setFunction(fontNumber)

    setCharacterMode()

    setDisplayProperties()

    clearDisplay()

    returnHome()

    setEntryMode(incrementEntry, scrollEntry)

    clearDisplay()

  }

  //Mirror power on sequence, resets mode back to character if changed to graphic
  private def emulatePowerOn(fontNumber: Int, incrementEntry: Boolean, scrollEntry: Boolean, displayMode: Boolean) {
    //1. Clear display
    clearDisplay()

    //2. Function set
    setFunction(fontNumber)

    //3. Power off
    setDisplayMode(displayMode, false)

    //4. Display off
    setDisplayProperties(false, false, false)

    //5. Entry mode set
    setEntryMode(incrementEntry, scrollEntry)

    //6. Move cursor right, power back on
    shiftCursorRight()
    setDisplayMode(displayMode)
  }

  //MARK: Low-level display instructions

  //Tells display that one bit has been sent
  private def cycleClock() = {
    clock.low()
    clock.high()
  }

  //Data = true, Command = false
  //dataRegister: true = use data register, false = use instruction register
  private def write(dataOrCommand: Boolean, data: Int) = {
    //Converts integer into an array of booleans representing an 8-bit binary number to send to display
    //Function ignores bits in positions > 7 (255 max in decimal), and doesn't account for signed ints (therefore can't use scala.Byte)
    def intToBinaryArray(data: Int): Array[Boolean] = {
      val numberOfBits = 8
      Array.tabulate(numberOfBits)(i => math.pow(2, (numberOfBits-1-i)).toInt & data).map(_ != 0)
    }

    //Set SS low, display is active low
    slaveSelect.low()

    //If sending data, set mosi high, else if sending command set mosi low
    mosi.setState(dataOrCommand)
    cycleClock()

    //Writing to display, so mosi is set low
    mosi.low()
    cycleClock()

    //Send each bit of the command
    println("Sending " + (if(dataOrCommand) "data" else "command") + ": 0x" + data.toHexString.toUpperCase)
    for (bit <- intToBinaryArray(data)) {
      if(bit) mosi.high() else mosi.low()
      cycleClock()
    }

    //Communication is over, set SS high again
    slaveSelect.high()
  }

  def writeData(data: Int, ramOperation: Boolean = false) = write(true, data)
  def writeCommand(commandData: Int) = write(false, commandData)

  //character: int 0-7 specifying which CGRAM character to write
  //row: int 0-7 specifying which row in character to write
  //data: binary number representing pixels in row to write
  def writeCGRAM(character: Int, row: Int, data: Int) = {
    writeCommand(0x40 + (character << 3) + row)
    writeData(data)
  }

  def clearCGRAM() = for(character <- 0 to 7; row <- 0 to 7) { writeCGRAM(character, row, 0x00) }

  def clearDisplay() {
    //Sending binary command 0000 0001
    //Command signature:     0000 0001
    writeCommand(0x01)

    //Display takes 6ms to clear
    Thread.sleep(6)
  }

  //Moves display back to origin if it has been shifted, moves cursor back to (0, 0)
  def returnHome() {
    //Sending binary command 0000 0010
    //Command signature:     0000 0010
    writeCommand(0x02)
  }

  //Determines behavior of screen as text is written to it
  private def setEntryMode(increment: Boolean, shift: Boolean) {
    //Sending binary command 0000 0100
    //Command signature      0000 01IS
    //  I = Increment/or decrement text as it's written on screen
    //  S = Shift(scroll) data on line as it's written
    writeCommand(0x04 + (if(increment) 0x02 else 0) + (if(shift) 0x01 else 0))
  }

  //Turns on/off display and optionally shows cursor
  private def setDisplayProperties(on: Boolean = true, showCursor: Boolean = false, cursorBlink: Boolean = false) = {
    //Sending binary command 0000 1100
    //Command signature:     0000 1DCB
    //  D = Display On
    //  C = Cursor On
    //  B = Cursor Blink
    writeCommand(0x08 + (if(on) 0x04 else 0) + (if(showCursor) 0x02 else 0) + (if(cursorBlink) 0x01 else 0))
  }

  def showCursor() = setDisplayProperties(true, true)
  def hideCursor() = setDisplayProperties(true, false)

  def enableBlinkingCursor() = setDisplayProperties(true, true, true)
  def disableBlinkingCursor() = setDisplayProperties(true, true, false)

  //Graphic mode = true, character mode = false
  private def setDisplayMode(graphicOrCharacter: Boolean, on: Boolean = true) = {
    //Sending binary command 0001 0111
    //Command signature:     0001 GP11
    //  G = Mode: 1=graphic, 0=character
    //  P = Power: 1=0n, 0=off
    writeCommand(0x13 + (if(graphicOrCharacter) 0x08 else 0) + (if(on) 0x04 else 0))
  }

  private def setCharacterMode() = setDisplayMode(false)
  private def setGraphicMode() = setDisplayMode(true)

  //Shifts display or the cursor to the right or left 1 space
  //Shift display = true, move cursor = false; move right = true, move left = false
  private def shiftDisplayOrCursor(displayOrCursor: Boolean = false, rightOrLeft: Boolean = false) = {
    //Sending binary command 0001 0000
    //Command signature:     0001 DR00
    //  D = Display or Cursor: 1=shift display, 0=move cursor
    //  R = Right or left: 1=right, 0=left
    writeCommand(0x10 + (if(displayOrCursor) 0x08 else 0) + (if(rightOrLeft) 0x04 else 0))
  }

  def shiftDisplayRight() = shiftDisplayOrCursor(true, true)
  def shiftDisplayLeft() = shiftDisplayOrCursor(true, false)

  def shiftCursorRight() = shiftDisplayOrCursor(false, true)
  def shiftCursorLeft() = shiftDisplayOrCursor(false, false)

  //Sets the number of lines the display uses, the font, and the font table
  def setFunction(fontToSet: Int, twoLineDisplay: Boolean = true, bigFont: Boolean = false) = {
    //Function set
    //Sending binary command 0011 1000
    //Command signature:     0011 NFFT
    //  N = lines: N=1 is 2 lines
    //  F = Font: 0 = 5x8, 1 = 5x10
    //  FT = Font Table:
    //     FT=00 is English/Japanese ~"standard" for character LCDs
    //     FT=01 is Western European I fractions, circle-c some arrows
    //     FT=10 is English/Russian
    //     FT=11 is Western European II, arrows, Greek letters
    val fontNumber = if(fontToSet > 3 || fontToSet < 0) 0 else fontToSet
    writeCommand(0x30 + (if(twoLineDisplay) 0x08 else 0) + (if(bigFont) 0x04 else 0) + fontNumber)
  }
}
