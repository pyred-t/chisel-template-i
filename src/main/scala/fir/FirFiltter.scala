package fir

import chisel3._


class FirFiltter(bitwidth : Int, coeffs : Seq[Int]) extends Module {
    val io = IO(new Bundle {
         val in = Input(UInt(bitwidth.W))
         val out =  Output(UInt((bitwidth*2+coeffs.length-1).W))
    })

    // 寄存器组
    val delays = Seq.fill(coeffs.length)(Wire(UInt(bitwidth.W))).scan(io.in)(
        (prev : UInt, next : UInt)=>{
            next := RegNext(prev)
            next
        }
    )
    
    // 部分乘积
    val mults = delays.zip(coeffs).map{case (delay: UInt, coeff : Int) => delay * coeff.U}
    
    // 总和
    val result = mults.reduce(_+&_)

    io.out := result
}