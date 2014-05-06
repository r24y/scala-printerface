package io.muller.printerface

import jssc._

object Main {
  def main(args:Array[String]):Unit = {
    val port = new SerialPort("/dev/ttyACM0")
    port.openPort()
    port.setEventsMask(SerialPort.MASK_RXCHAR)
    
    Thread.sleep(1000)
    port.addEventListener(new SerialPortEventListener {
      def serialEvent(ev:SerialPortEvent) = {
        print(port.readString())
      }
    })
    val printer = new SerialPrinter(port)

    printer.send(Command.Home)
    printer.disconnect()
    println("\nDone")
  }
}

class SerialPrinter (port:SerialPort) {
  def this(portName:String,baud:Int) = this({
    val port = new SerialPort(portName)
    port.setParams(baud,8,1,0)
    port
  })
  def connect():Unit = port.openPort()
  def disconnect():Unit = port.closePort()
  def send(c:Command) = {
    val str = c.toGCode
    val cmd = s"$str\n"
    println(s"Sending $str")
    port.writeBytes(cmd.getBytes)
  }
}

trait Command {
  def toGCode:String
}

object Command {
  def Home = new HomeCommand
}

class HomeCommand extends Command{
  override def toGCode = "G28"
}

case class Move(p:Map[Char,Double],t:String) extends Command{
  def this(p:Map[Char,Double]) = this(p, "G1")
  override def toGCode = {
    val x = p get 'x' match {
      case Some(n) => s" X$n"
      case None => ""
    }
    val y = p get 'y' match {
      case Some(n) => s" Y$n"
      case None => ""
    }
    val z = p get 'z' match {
      case Some(n) => s" Z$n"
      case None => ""
    }
    s"$t $x$y$z"
  }
  def x = p get 'x' match {
    case Some(n) => n
    case None => Double.NaN
  }
  def y = p get 'y' match {
    case Some(n) => n
    case None => Double.NaN
  }
  def z = p get 'z' match {
    case Some(n) => n
    case None => Double.NaN
  }
}

object RapidMove{
  def apply(p:Map[Char,Double]) = new Move(p, "G0")
}

case class EStop extends Command {
  override def toGCode = "M112"
}

class GetPosition extends Command {
  override def toGCode = "M114"
}
