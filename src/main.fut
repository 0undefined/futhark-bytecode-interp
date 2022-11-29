open import "interp_registers"

type dtype = f64

module ir = InterpRegisterListF64 f32 { def numregs = 8 : i64 }

entry main (_: i64) : f64 =
  0f64
