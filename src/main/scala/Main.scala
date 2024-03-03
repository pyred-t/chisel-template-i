import matcal._
import chisel3._
import chisel3.util._
import chisel3.testers._

object Main {
    def main(args: Array[String]): Unit = {
        println(getVerilogString(new PE(8, 16, 16, Dataflow.WS)))
    }
}
