package matcal

import chisel3._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.GivenWhenThen
import scala.util.Random


class MeshTest extends AnyFlatSpec with ChiselScalatestTester with GivenWhenThen {
    val inwid = 8; val outwid = 16; val accwid = 16;
    val mRows = 3; val mCols = 2;

    def getElem(vec : Seq[Int], idx: Int) : Int = {
        var elm = 0
        if(idx < vec.size && idx >= 0) {
            elm = vec(idx)
        }
        elm
    }

    "Mesh" should "does correct matrix computation in OS" in {
        test(new Mesh(inwid, outwid, accwid, Dataflow.OS, 1, 1, 0, mRows, mCols, 0)){c => {
            val inmat = Seq.tabulate(3, 3) { (i, j) => Random.nextInt(10)}
            val kmat = Seq.tabulate(3, 2) { (i, j) => Random.nextInt(10)}
            println(inmat, kmat)
            val kmatT = kmat.transpose
            val resmat = new Array[Int](3*2)

            for (i <- 0 until 3) {
                for(j <-0 until 2) {
                    resmat(i*2 + j) = inmat(i).zip(kmatT(j)).map{case (x,y) => x*y}.reduce(_+_)
                }
            }

            for(i <- 0 until (3+2)) {
                val invec = inmat.zipWithIndex.map{case (row, rowid) => getElem(row, i - rowid).S}
                val kvec = kmatT.zipWithIndex.map{case (col, colid) => getElem(col, i-colid).S}
                println(i, "A ", invec, "B ", kvec)

                c.io.in_a.zipWithIndex.foreach{case (x, idx) => x.foreach{y=>y.poke(invec(idx))}}
                c.io.in_b.zipWithIndex.foreach{case (x, idx) => x.foreach{y=>y.poke(kvec(idx))}}
                c.io.in_control.foreach{x=>x.foreach{y=>y.poke((new PEControl(accwid)).Lit(_.propagate -> true.B))}}
                c.io.in_valid.foreach{x=>x.foreach{y=>y.poke(true.B)}}
                c.clock.step()
            }

            // c.io.in_control.foreach{x=>x.foreach{y=>y.propagate.poke(true.B)}}
            c.io.in_control.foreach{x=>x.foreach{y=>y.poke((new PEControl(accwid)).Lit(_.propagate -> false.B))}}
            c.io.in_valid.foreach{x=>x.foreach{y=>y.poke(true.B)}}
            
            // 3 个周期后 propagate==false 到达 Mesh 列末端
            c.clock.step()              // 1
            println(c.io.out_c.peek())
            c.clock.step()              // 2
            println(c.io.out_c.peek())
            c.clock.step()
            println(c.io.out_c.peek())  // 3 resmat(2, 0)  resmat(2, 1)
            c.clock.step()
            println(c.io.out_c.peek())  // resmat(1, 0)  resmat(1, 1)
            c.clock.step()
            println(c.io.out_c.peek())  // resmat(0, 0)  resmat(0, 1)

            println(resmat.map{_.toString}.mkString(","))
        }}
    }

    
}
