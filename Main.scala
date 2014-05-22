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
  def Move(x:Double,y:Double,z:Double) = new Move(x,y,z)
  def EStop = new EStop
  def apply(cmd:String) = new Command {
    override def toGCode = cmd
  }
}

class HomeCommand extends Command{
  override def toGCode = "G28"
}

case class Move(p:Map[Char,Double],t:String) extends Command{
  def this(p:Map[Char,Double]) = this(p, "G1")
  def this(x:Double, y:Double, z:Double, t:String) = this(Map(
    'x' -> x,
    'y' -> y,
    'z' -> z
  ), t)
  def this(x:Double, y:Double, z:Double) = this(x,y,z,"G1")
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

class SetDelta(p:Map[Char, Double]) extends Command {
  val t = "M665"
  override def toGCode = {
    // Delta arm length
    val l = p get 'L' match {
      case Some(n) => s" L$n"
      case None => ""
    }
    // Delta radius
    val r = p get 'R' match {
      case Some(n) => s" R$n"
      case None => ""
    }
    // Segments per second
    val s = p get 'S' match {
      case Some(n) => s" S$n"
      case None => ""
    }
    s"$t $l$r$s"
  }
}
