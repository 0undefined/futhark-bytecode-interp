open import "interp_registers"

type dtype = f64

module ir = InterpRegisterListF64 { def numregs = 2 : i64 }

entry main (_: i64) : f64 =
  0f64
