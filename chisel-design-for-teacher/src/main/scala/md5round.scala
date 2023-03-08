import chisel3._
import chisel3.util._
class md5round extends Module{
  val io = IO(new Bundle{
    val a = Input(UInt(32.W))
    val b = Input(UInt(32.W))
    val c = Input(UInt(32.W))
    val d = Input(UInt(32.W))
    val m = Input(UInt(32.W))
    val s = Input(UInt(5.W))
    val t = Input(UInt(32.W))
    val r = Input(UInt(2.W))
    val next_a = Output(UInt(32.W))
  })
  val add_res = Wire(UInt(32.W))
  val rot_res = Wire(Vec(2, UInt(32.W)))
  add_res := 0.U
  rot_res := Seq(add_res << io.s, add_res >> (32.U - io.s))
  io.next_a := io.b + rot_res.reduce(_|_)
  switch(io.r) {
    is(0.U) {
      add_res := io.a + F(io.b, io.c, io.d) + io.m + io.t
    }
    is(1.U) {
      add_res := io.a + G(io.b, io.c, io.d) + io.m + io.t
    }
    is(2.U) {
      add_res := io.a + H(io.b, io.c, io.d) + io.m + io.t
    }
    is(3.U) {
      add_res := io.a + I(io.b, io.c, io.d) + io.m + io.t
    }
  }
  def F(x: UInt, y: UInt, z: UInt): UInt = {
    (x & y) | ((~x).asUInt & z)
  }
  def G(x: UInt, y: UInt, z: UInt): UInt = {
    (x & z) | (y & (~z).asUInt)
  }
  def H(x: UInt, y: UInt, z: UInt): UInt = {
    x ^ y ^ z
  }
  def I(x: UInt, y: UInt, z: UInt): UInt = {
    y ^ (x | (~z).asUInt)
  }
}
