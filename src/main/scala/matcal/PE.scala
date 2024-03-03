package matcal

import chisel3._
import chisel3.util._


class PEControl(accWidth: Int) extends Bundle {
    val propagate = Bool()
}


class PE(inputWidth: Int, outputWidth: Int, accWidth: Int, df: Dataflow.Value)extends Module{
    val io = IO(new Bundle{
        val in_a = Input(SInt(inputWidth.W))
        val in_b = Input(SInt(outputWidth.W))
        val in_d = Input(SInt(outputWidth.W))
        val out_a = Output(SInt(inputWidth.W))   // forward a
        val out_b = Output(SInt(outputWidth.W))  
        val out_c = Output(SInt(outputWidth.W))

        val in_control = Input(new PEControl(accWidth))
        val out_control = Output(new PEControl(accWidth))   // forward control
        val in_valid = Input(Bool())    // not implemented yet
        val out_valid = Output(Bool())  // forward valid
    })
    
    val cWidth = if (df == Dataflow.WS) inputWidth else accWidth
    // dual register
    val c1 = RegInit(0.S(cWidth.W))
    val c2 = RegInit(0.S(cWidth.W))
    
    // signal
    val prop = io.in_control.propagate

    // forward
    io.out_a := io.in_a
    io.out_valid := io.in_valid
    io.out_control := io.in_control

    // compute
    when(io.in_valid){
    when((df == Dataflow.WS).B){
        // c1 as held weight (update when propgate), 
        // c2 as flush weight (use at the same time propgating),
        // in_d => c1/c2 => out_c;
        // in_b/out_b as partial sum
        when(prop) {
            // 对c1的读取必须在时钟周期结束前
            io.out_c := c1
            io.out_b := io.in_b + io.in_a * c2.asTypeOf(SInt(inputWidth.W)) // mac
            c1 := io.in_d
        }.otherwise{
            io.out_c := c2
            io.out_b := io.in_b + io.in_a * c1.asTypeOf(SInt(inputWidth.W))
            c2 := io.in_d
        }
    }.otherwise{
        // c1 and c2 are used to accumulate alternately (prop==1: c2, prop==0: c1);
        // Output one last result and update it initial bias: in_d => c1/c2 => out_c,
        // while the other is accumulating: c2/c1 => c2/c1;
        // b as weight to be forwarded
        when(prop) {
            io.out_c := c1
            io.out_b := io.in_b
            c2 := c2 + io.in_a * io.in_b.asTypeOf(SInt(inputWidth.W))
            c1 := io.in_d
        }.otherwise{
            io.out_c := c2
            io.out_b := io.in_b
            c1 := c1 + io.in_a * io.in_b.asTypeOf(SInt(inputWidth.W))
            c2 := io.in_d
        }
    }
    printf(p" $name, INPUT=${io.in_a},  ${io.in_b}, OUT_C=${io.out_c} REG=$c1, $c2 \n")

    }.otherwise{
        // output 在任何情况下都需要赋值
        c1 := c1
        c2 := c2
        io.out_b := DontCare
        io.out_c := DontCare
    }

}