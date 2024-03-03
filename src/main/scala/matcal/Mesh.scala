package matcal

import chisel3._
import chisel3.util._

class Mesh(inputWidth: Int, outputWidth: Int, accWidth: Int, df: Dataflow.Value,
            val tileRows: Int, val tileColumns: Int, tile_latency: Int,
            val meshRows: Int, val meshColumns: Int, output_delay: Int) extends Module {
    
    val io = IO(new Bundle {
        val in_a = Input(Vec(meshRows, Vec(tileRows, SInt(inputWidth.W))))
        val in_b = Input(Vec(meshColumns, Vec(tileColumns, SInt(outputWidth.W))))
        val in_d = Input(Vec(meshColumns, Vec(tileColumns, SInt(outputWidth.W))))
        val out_b = Output(Vec(meshColumns, Vec(tileColumns, SInt(outputWidth.W))))
        val out_c = Output(Vec(meshColumns, Vec(tileColumns, SInt(outputWidth.W))))

        val in_control = Input(Vec(meshColumns, Vec(tileColumns, new PEControl(accWidth))))
        val out_control = Output(Vec(meshColumns, Vec(tileColumns, new PEControl(accWidth))))
        val in_valid = Input(Vec(meshColumns, Vec(tileColumns, Bool())))
        val out_valid = Output(Vec(meshColumns, Vec(tileColumns, Bool())))
    })

    def pipe[T <: Data](valid: Bool, t: T, latency: Int): T = {
        chisel3.withReset(false.B) {Pipe(valid, t, latency).bits}
    }

    val mesh = Seq.fill(meshRows, meshColumns)(Module(new Tile(inputWidth, outputWidth, accWidth, df, tileRows, tileColumns)))
    val meshT = mesh.transpose


    // connect tile 'a'
    for (r <- 0 until meshRows) {
        mesh(r).foldLeft(io.in_a(r)){
            case (in_a, tile) => // (leftsum, this_element) => next_leftsum
                tile.io.in_a := ShiftRegister(in_a, tile_latency+1)
                tile.io.out_a
        }
    }

    // connect tile 'b', 'd', 'control', 'valid'
    for (c <- 0 until meshColumns) {
        meshT(c).foldLeft((io.in_valid(c), io.in_b(c), io.in_d(c), io.in_control(c))){
            case ((valid, b, d, ctrl), tile) => {
                tile.io.in_b := pipe(valid.head, b, tile_latency+1)
                tile.io.in_d := pipe(valid.head, d, tile_latency+1)
                tile.io.in_control := pipe(valid.head, ctrl, tile_latency+1)
                tile.io.in_valid := ShiftRegister(valid, tile_latency+1)
                (tile.io.out_valid, tile.io.out_b, tile.io.out_c, tile.io.out_control)
            }
        }
    }

    // capature out_vec and out_control_vec
    for(c <- 0 until meshColumns) {
        io.out_b(c) := ShiftRegister(meshT(c).last.io.out_b, output_delay)
        io.out_c(c) := ShiftRegister(meshT(c).last.io.out_c, output_delay)
        io.out_valid(c) := ShiftRegister(meshT(c).last.io.out_valid, output_delay)
        io.out_control(c) := ShiftRegister(meshT(c).last.io.out_control, output_delay)
    }
}
