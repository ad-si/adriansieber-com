import AVFoundation  // Necessary for activating the flash

// Binary codes as described in the Scout SDK.
// Distinguishes between direct commands, which are executed immediately
// and scripting commands, which are only executed once the run command
// is triggered.

let directSpinCw05s  = 0b0000000  //  0 - Spin motor 0.5 seconds clockwise
let directSpinAcw05s = 0b0000001  //  1 - Spin motor 0.5 seconds anticlockwise

let directBeep1      = 0b0000100  //  4
let directBeep2      = 0b0000101  //  5
let directBeep3      = 0b0000110  //  6
let directBeep4      = 0b0000111  //  7
let directBeep5      = 0b0001000  //  8

let directStop       = 0b0001111  // 10

// All motor scripting commands play a short jingle before activation
// This can unfortunately not be prevented
let scriptCw05s      = 0b0010000  // 16 - Spin motor 0.5 seconds clockwise
let scriptCw1s       = 0b0010001  // 17 -      "     1               "
let scriptCw2s       = 0b0010010  // 18 -      "     2               "
let scriptCw5s       = 0b0010011  // 19 -      "     5               "

let scriptAcw05s     = 0b0010100  // 20 - Spin motor 0.5 seconds anticlockwise
let scriptAcw1s      = 0b0010101  // 21 -      "     1               "
let scriptAcw2s      = 0b0010110  // 22 -      "     2               "
let scriptAcw5s      = 0b0010111  // 23 -      "     5               "

let scriptBeep1      = 0b0011000  // 24
let scriptBeep2      = 0b0011001  // 25
let scriptBeep3      = 0b0011010  // 26
let scriptBeep4      = 0b0011011  // 27
let scriptBeep5      = 0b0011100  // 28

// Pause run until light is shined on sensor
let scriptWaitLight  = 0b0011101  // 29

// Spin clockwise in bursts until light is shined on sensor
// then spin anticlockwise until light is out of range
// Continues with script after 30 seconds of spinning clockwise.
let scriptSeekLight  = 0b0011110  // 30

// Beeps a pattern and waits for someone flashing that same pattern back.
// If correct or after 3 failed attempts, it continues with the program.
let scriptCode       = 0b0011111  // 31

// Stays still in dark, shortly spins clockwise if light is flashed.
// Continues with script after 30 seconds of standing still.
// Makes continuous clockwise spinning possible by regularly flashing.
let scriptKeepAlive  = 0b0100000  // 32

let directRun        = 0b0100001  // 33 - Same as pressing the run button
let directDelete     = 0b0100010  // 34 - Delete all previously stored commands

// Not sure what those commands are for. Let me know if you have any insights!
let scriptNext       = 0b1000110  // 70
let scriptReset      = 0b1000111  // 71

// Maximum number of commands which can be stored before execution.
// Plays a deep beep when storage is full.
let maxNumCmds = 15


func sleepMs (_ milliseconds: Int) {
  usleep(useconds_t(milliseconds * 1000))
}


func torchOn() {
  guard let device = AVCaptureDevice.default(for: AVMediaType.video)
    else { return }
  guard device.hasTorch
    else { return }

  do {
    try device.lockForConfiguration()
    try device.setTorchModeOn(level: 1.0)
  }
  catch { print(error) }

  device.unlockForConfiguration()
}


func torchOff() {
  guard let device = AVCaptureDevice.default(for: AVMediaType.video)
    else { return }
  guard device.hasTorch
    else { return }

  do {
    try device.lockForConfiguration()
  }
  catch {
    print(error)
  }

  device.torchMode = AVCaptureDevice.TorchMode.off
  device.unlockForConfiguration()
}


func vllInit() {
  torchOn()
  sleepMs(400)
}


func vllStart() {
  torchOff()
  sleepMs(20)
}


func vll0() {
  torchOn()
  sleepMs(40)
  torchOff()
  sleepMs(20)
}


func vll1() {
  torchOn()
  sleepMs(20)
  torchOff()
  sleepMs(40)
}


func vllEnd() {
  torchOn()
  sleepMs(20)
  torchOff()
  sleepMs(60)
  torchOn()
  sleepMs(120)
  torchOff()
}


func runCmd (_ cmd: Int) {
  vllInit()
  vllStart()
  runBoolList(binaryFlagToBoolList(cmd))
  vllEnd()
}


func getChecksum (_ n: Int) -> Int {
  let shift2 = n >> 2
  let shift4 = n >> 4

  return 7 - ((n + shift2 + shift4) & 7)
}


func binaryFlagToBoolList (_ number: Int) -> [Bool] {
  // Put checksum before number (e.g. 111 + 0000000)
  let fullNum = (getChecksum(number) << 7 ) + number

  return (0...9)
    .map {fullNum & (1 << $0) != 0}
    .reversed()
}


func runBoolList (_ boolList: [Bool]) {
  // 10 digit binary code with 3 digit checksum and 7 digit payload
  if boolList.count != 10 {
    print("Error: Binary code must consist of 10 digits")
  }
  else {
    for bool in boolList {
      if bool {
        vll1()
      }
      else {
        vll0()
      }
    }
  }
}


runCmd(directDelete)

runCmd(scriptCw5s)
runCmd(scriptWaitLight)
runCmd(scriptAcw5s)
runCmd(scriptBeep5)

runCmd(directRun)
