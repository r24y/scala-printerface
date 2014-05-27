package io.muller.printerface

import jssc._

object Main {
  def main(args:Array[String]):Unit = {
    val printer = new SerialPrinter("/dev/ttyACM0",115200)
    printer.on((ev:PrinterEvent[Any]) => ev.name match {
        case "data" => print(ev.data.asInstanceOf[String])
        case _ =>
      }
    )
    printer.connect()

    printer.send(Command.Home)
    Thread.sleep(5000)
    printer.disconnect()
    println("\nDone")
  }
}

class SerialPrinter (port:SerialPort,baud:Int) {

  var listeners: List[PrinterEvent[Any] => Unit] = Nil

  def this(portName:String,baud:Int) = this(new SerialPort(portName),baud)

  def on(l:(PrinterEvent[Any] => Unit)) = {
    listeners ::= l
  }

  def emit(ev:PrinterEvent[Any]) = listeners foreach (l => l(ev))

  def connect():Unit = {
    port.openPort()
    port.setParams(baud,8,1,0)
    port.setEventsMask(SerialPort.MASK_RXCHAR)
    port.addEventListener(new SerialPortEventListener {
      def serialEvent(ev:SerialPortEvent) = {
        emit(new PrinterEvent("data", port.readString()))
      }
    })
    emit(new PrinterEvent("connected",true))
  }
  def disconnect():Unit = port.closePort()
  def send(c:Command) = {
    val str = c.toGCode
    val cmd = s"$str\n"
    println(s"Sending $str")
    port.writeBytes(cmd.getBytes)
  }

}

case class PrinterEvent[T](name:String, data:T)

trait Command {
  def toGCode:String
}

object Command {
  lazy val Home = new HomeCommand
  def Move(x:Double,y:Double,z:Double) = new Move(x,y,z)
  lazy val EStop = new EStop
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

class EStop extends Command {
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
