package fir

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec


class FirSpec extends AnyFreeSpec with ChiselScalatestTester {
    "Fir should calculate proper sum" in {
        test(new FirFiltter(8, Seq.fill(4)(1) )) { c =>
            c.io.in.poke(1.U)
            c.io.out.expect(1.U)  // 1, 0, 0, 0
            c.clock.step(1)
            c.io.in.poke(4.U)
            c.io.out.expect(5.U)  // 4, 1, 0, 0
            c.clock.step(1)
            c.io.in.poke(3.U)
            c.io.out.expect(8.U)  // 3, 4, 1, 0
            c.clock.step(1)
            c.io.in.poke(2.U)
            c.io.out.expect(10.U)  // 2, 3, 4, 1
            c.clock.step(1)
            c.io.in.poke(7.U)
            c.io.out.expect(16.U)  // 7, 2, 3, 4
            c.clock.step(1)
            c.io.in.poke(0.U)
            c.io.out.expect(12.U)  // 0, 7, 2, 3
        }
    }
}
