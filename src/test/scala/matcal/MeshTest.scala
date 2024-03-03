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
                
                c.io.in_a.foreach{x=>x.zipWithIndex.foreach{case(y, idx)=> y.poke(invec(idx))}} 
                c.io.in_b.foreach{x=>x.zipWithIndex.foreach{case(y, idx)=> y.poke(kvec(idx))}}
                c.io.in_control.foreach{x=>x.foreach{y=>y.poke((new PEControl(accwid)).Lit(_.propagate -> true.B))}}
                c.io.in_valid.foreach{x=>x.foreach{y=>y.poke(true.B)}}
                c.clock.step()
            }

            val res1 = c.io.out_c.map{_.map{_.peek()}}
            println(res1)

            // c.io.in_control.foreach{x=>x.foreach{y=>y.propagate.poke(true.B)}}
            c.io.in_control.foreach{x=>x.foreach{y=>y.poke((new PEControl(accwid)).Lit(_.propagate -> false.B))}}
            c.io.in_valid.foreach{x=>x.foreach{y=>y.poke(true.B)}}

            val res2 = c.io.out_c.map{_.map{_.peek()}}
            println(res2)

            c.clock.step()

            val res3 = c.io.out_c.map{_.map{_.peek()}}
            println(res3)

            println(resmat.map{_.toString}.mkString(","))
        }}
    }

    
}
