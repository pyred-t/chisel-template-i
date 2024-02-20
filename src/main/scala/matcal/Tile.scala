package matcal

import chisel3._
import chisel3.util._

class Tile(inputWidth: Int, outputWidth: Int, accWidth: Int, df: Dataflow.Value,
            val rows: Int, val columns: Int) extends Module {
    
    val io = IO(new Bundle {
        val in_a = Input(Vec(rows, SInt(inputWidth.W)))
        val in_b = Input(Vec(columns, SInt(outputWidth.W)))
        val in_d = Input(Vec(columns, SInt(outputWidth.W)))
        val out_a = Output(Vec(rows, SInt(inputWidth.W)))
        val out_c = Output(Vec(columns, SInt(outputWidth.W)))
        val out_b = Output(Vec(columns, SInt(outputWidth.W)))
        
        val in_control = Input(Vec(columns, new PEControl(accWidth)))
        val out_control = Output(Vec(columns, new PEControl(accWidth)))
        val in_valid = Input(Vec(columns, Bool()))
        val out_valid = Output(Vec(columns, Bool()))
    })

    // PE 2d array
    val tile = Seq.fill(rows, columns)(Module(new PE(inputWidth, outputWidth, accWidth, df)))
    val tileT = tile.transpose

    // connect 'a' horizontally from left Input to right Output
    for (r <- 0 until rows) {
        tile(r).foldLeft(io.in_a(r)){case (in_a, pe) => 
            pe.io.in_a := in_a
            pe.io.out_a     // pass to next in_a
        }

        io.out_a(r) := tile(r)(columns-1).io.out_a
    }

    // connect 'b', 'd', 'control', 'valid' vertically from top Input to bottom Output
    for (c <- 0 until columns) {
        tileT(c).foldLeft((io.in_b(c), io.in_d(c), io.in_control(c), io.in_valid(c))){
            case ((b, d, ctrl, valid), pe) => {
                pe.io.in_b := b
                pe.io.in_d := d
                pe.io.in_control := ctrl
                pe.io.in_valid := valid
                (pe.io.out_b, pe.io.out_c, pe.io.out_control, pe.io.out_valid)
            }
        }

        io.out_b(c) := tileT(c)(rows-1).io.out_b
        io.out_c(c) := tileT(c)(rows-1).io.out_c
        io.out_control(c) := tileT(c)(rows-1).io.out_control
        io.out_valid(c) := tileT(c)(rows-1).io.out_valid
    }

}