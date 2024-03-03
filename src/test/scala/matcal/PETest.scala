package matcal

import chisel3._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.GivenWhenThen


class PETest extends AnyFlatSpec with ChiselScalatestTester with GivenWhenThen {
    val inwid = 8; val outwid = 16; val accwid = 16;
    
    "PE" should "calculate proper sum in WS" in {
        test(new PE(inwid, outwid, accwid, Dataflow.WS)) {c => {
            Given("initial weight 1")
            // peCtrl1.propagate := true.B
            c.io.in_valid.poke(true.B)
            c.io.in_control.poke((new PEControl(accwid)).Lit(_.propagate -> true.B))
            c.io.in_d.poke(1.S) // update c1
            c.clock.step()

            Then("start calculating with initial weight")
            c.io.in_valid.poke(true.B)
            c.io.in_control.poke((new PEControl(accwid)).Lit(_.propagate -> false.B))
            c.io.in_a.poke(2.S)
            c.io.in_b.poke(1.S)
            c.io.in_d.poke(1.S) // update c2
            c.clock.step()
            c.io.out_a.expect(2.S)
            c.io.out_b.expect(3.S)

            When("prop signal is high, weight is changed to 2")
            c.io.in_valid.poke(true.B)
            c.io.in_control.poke((new PEControl(accwid)).Lit(_.propagate -> true.B))
            c.io.in_a.poke(3.S)
            c.io.in_b.poke(0.S)
            c.io.in_d.poke(2.S) // update c1
            c.clock.step()
            c.io.out_a.expect(3.S)
            // c.io.out_c.expect(1.S) // last c1?
            c.io.out_b.expect(3.S)
            
            c.io.in_valid.poke(true.B)
            c.io.in_control.poke((new PEControl(accwid)).Lit(_.propagate -> false.B))
            c.io.in_a.poke(3.S)
            c.io.in_b.poke(1.S)
            c.clock.step()
            c.io.out_a.expect(3.S)
            // c.io.out_c.expect(1.S)  // last c2
            c.io.out_b.expect(7.S)
        }}
    }

    it should "calculate proper sum in OS" in {
        test(new PE(inwid, outwid, accwid, Dataflow.OS)) {c => {
            val a1 = Seq(1, 2, 3, 4)
            val b1 = Seq(3, 0, 2, 0)
            val result1 = a1.zip(b1).map{case (x, y) => x*y}.reduce(_+_)
            When("prop is high")
            for (i <- 0 until 4) {
                c.io.in_valid.poke(true.B)
                c.io.in_control.poke((new PEControl(accwid)).Lit(_.propagate -> true.B))
                c.io.in_a.poke(a1(i))
                c.io.in_b.poke(b1(i))
                c.io.in_d.poke(100.S)
                c.clock.step()              // calculate c2, in_d => c1
                c.io.out_b.expect(b1(i))
            }
            c.io.in_valid.poke(true.B)
            c.io.in_d.poke(200.S)
            c.io.in_control.poke((new PEControl(accwid)).Lit(_.propagate -> false.B))
            c.io.out_c.expect(result1.S)       // out c2, read result before clock end
            c.clock.step()
            c.io.out_c.expect(200.S)            // after one clock, in_d => c2
            When("prop is low and initial partial sum is 200")
            for (i <- 0 until 4) {
                c.io.in_valid.poke(true.B)
                c.io.in_control.poke((new PEControl(accwid)).Lit(_.propagate -> false.B))
                c.io.in_a.poke(a1(i))
                c.io.in_b.poke(b1(i))
                c.clock.step()      // calculate c1, in_d => c2
                c.io.out_b.expect(b1(i))
            }
            c.io.in_valid.poke(true.B)
            c.io.in_d.poke(120.S)
            c.io.in_control.poke((new PEControl(accwid)).Lit(_.propagate -> true.B))
            c.io.out_c.expect((result1+100).S)  // out c1 before clock end
            c.clock.step()
            c.io.out_c.expect(120.S)        // after one clock, in_d => c1
        }
    }}
    
}
