import chisel3._
import chisel3.util._

import scala.language.postfixOps

class md5 extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(128.W))
    val in_valid = Input(Bool())

    val out = Output(UInt(128.W))
    val out_valid = Output(Bool())
    val ready = Output(Bool())
  })
  io.out := 0.U
  io.out_valid := false.B
  io.ready := false.B
  val idle :: r0 :: r1 :: r2 :: r3 :: finished :: turn_arnd :: Nil = Enum(7)
  val A = RegInit(defs.A)
  val B = RegInit(defs.B)
  val C = RegInit(defs.C)
  val D = RegInit(defs.D)
  val AA = RegInit(0.U(32.W))
  val BB = RegInit(0.U(32.W))
  val CC = RegInit(0.U(32.W))
  val DD = RegInit(0.U(32.W))
  val next_A = Wire(UInt(32.W))
  val next_B = Wire(UInt(32.W))
  val next_C = Wire(UInt(32.W))
  val next_D = Wire(UInt(32.W))
  val phase = RegInit(0.U(4.W))
  val state = RegInit(1.U(8.W))
  val next_state = Wire(UInt(8.W))
  val msg = RegInit(0.U(512.W))
  val r = Module(new md5round)
  val out_r = RegInit(false.B)

  val cya = Wire(UInt(32.W))
  val cyb = Wire(UInt(32.W))
  val cyc = Wire(UInt(32.W))
  val cyd = Wire(UInt(32.W))

  // inital wires
  next_A := A
  next_B := B
  next_C := C
  next_D := D
  next_state := state
  r.io.a := 0.U
  r.io.b := 0.U
  r.io.c := 0.U
  r.io.d := 0.U
  r.io.m := 0.U
  r.io.s := 0.U
  r.io.t := 0.U
  r.io.r := 0.U
  cya := 0.U
  cyb := 0.U
  cyc := 0.U
  cyd := 0.U

  out_r := state(finished)
  io.out_valid := out_r
  io.ready := state(idle)
  io.out := A ## B ## C ## D

  // update regs
  state := next_state

  when(next_state(idle)) {
    AA := 0.U
    BB := 0.U
    CC := 0.U
    DD := 0.U
  }.elsewhen(next_state(r0) && state(idle)) {
    AA := A
    BB := B
    CC := C
    DD := D
  }

  when(next_state(idle)) {
    A := defs.A
    B := defs.B
    C := defs.C
    D := defs.D
  }.otherwise {
    A := next_A
    B := next_B
    C := next_C
    D := next_D
  }

  when(next_state(r0) && state(idle)) {
    phase := 0.U
  } .otherwise {
    phase := phase + 1.U
  }

  when(next_state(idle)) {
    msg := 0.U
  }.elsewhen(next_state(r0) && state(idle)) {
    msg := Cat(io.in, io.in, io.in, io.in)
  }

// combine logic
  when(state(idle)) {
    printf(p"waiting idle\n")
    when(io.in_valid) {
      printf(p"starting round0\n")
      next_state := UIntToOH(r0)
    }
  }

  switch(phase(1, 0)) {
    is(0.U) {
      cya := A
      cyb := B
      cyc := C
      cyd := D
    }
    is(1.U) {
      cya := D
      cyb := A
      cyc := B
      cyd := C
    }
    is(2.U) {
      cya := C
      cyb := D
      cyc := A
      cyd := B
    }
    is(3.U) {
      cya := B
      cyb := C
      cyc := D
      cyd := A
    }
  }

  when(state(r0)) {
    printf(p"round0\n")
    r.io.r := 0.U
    switch(phase(1, 0)) {
      is(0.U) {
        next_A := r.io.next_a
      }
      is(1.U) {
        next_D := r.io.next_a
      }
      is(2.U) {
        next_C := r.io.next_a
      }
      is(3.U) {
        next_B := r.io.next_a
      }
    }
    r.io.a := cya
    r.io.b := cyb
    r.io.c := cyc
    r.io.d := cyd
    for(i <- 0 until 16)
      when(phase === i.U) {
        r.io.m := msg(i * 32 + 31, i * 32)
        r.io.s := defs.S(i)
        r.io.t := defs.K(i)
      }

    when(phase === "b1111".U) {
      next_state := UIntToOH(r1)
    }
  }

  when(state(r1)) {
    printf(p"round1\n")
    r.io.r := 1.U
    switch(phase(1, 0)) {
      is(0.U) {
        next_A := r.io.next_a
      }
      is(1.U) {
        next_D := r.io.next_a
      }
      is(2.U) {
        next_C := r.io.next_a
      }
      is(3.U) {
        next_B := r.io.next_a
      }
    }
    r.io.a := cya
    r.io.b := cyb
    r.io.c := cyc
    r.io.d := cyd
    for(i <- 0 until 16)
      when(phase === i.U) {
        val j = (i >> 1) | ((i << 3) & 0xf)
        r.io.m := msg(j * 32 + 31, j * 32)
        r.io.s := defs.S(16 + i)
        r.io.t := defs.K(16 + i)
      }

    when(phase === "b1111".U) {
      next_state := UIntToOH(r2)
    }
  }

  when(state(r2)) {
    printf(p"round2\n")
    r.io.r := 2.U
    switch(phase(1, 0)) {
      is(0.U) {
        next_A := r.io.next_a
      }
      is(1.U) {
        next_D := r.io.next_a
      }
      is(2.U) {
        next_C := r.io.next_a
      }
      is(3.U) {
        next_B := r.io.next_a
      }
    }
    r.io.a := cya
    r.io.b := cyb
    r.io.c := cyc
    r.io.d := cyd
    for(i <- 0 until 16)
      when(phase === i.U) {
        val j = (i >> 2) | ((i << 2) & 0xf)
        r.io.m := msg(j * 32 + 31, j * 32)
        r.io.s := defs.S(32 + i)
        r.io.t := defs.K(32 + i)
      }

    when(phase === "b1111".U) {
      next_state := UIntToOH(r3)
    }
  }

  when(state(r3)) {
    printf(p"round3\n")
    r.io.r := 3.U
    switch(phase(1, 0)) {
      is(0.U) {
        next_A := r.io.next_a
      }
      is(1.U) {
        next_D := r.io.next_a
      }
      is(2.U) {
        next_C := r.io.next_a
      }
      is(3.U) {
        next_B := r.io.next_a
      }
    }
    r.io.a := cya
    r.io.b := cyb
    r.io.c := cyc
    r.io.d := cyd
    for(i <- 0 until 16)
      when(phase === i.U) {
        val j = (i >> 3) | ((i << 1) & 0xf)
        r.io.m := msg(j * 32 + 31, j * 32)
        r.io.s := defs.S(48 + i)
        r.io.t := defs.K(48 + i)
      }

    when(phase === "b1111".U) {
      next_state := UIntToOH(finished)
    }
  }

  when(state(finished)) {
    printf(p"finished\n")
    next_A := AA + A
    next_B := BB + B
    next_C := CC + C
    next_D := DD + D
    next_state := UIntToOH(turn_arnd)
  }

  when(state(turn_arnd)) {
    printf(p"turn_arnd\n")
    next_state := UIntToOH(idle)
  }
}
