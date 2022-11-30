open import "interp_registers"

module ir_2  = interp_list_memory f64 { def numregs =  2 : i64 }
module ir_4  = interp_list_memory f64 { def numregs =  4 : i64 }
module ir_8  = interp_list_memory f64 { def numregs =  8 : i64 }
module ir_16 = interp_list_memory f64 { def numregs = 16 : i64 }
module ir_32 = interp_list_memory f64 { def numregs = 32 : i64 }

module it_4 =  interp_tuple_memory f64

entry main (min: f64) (max: f64) (n: i64) : [n]f64 =
  -- (max - min) / n
  let interval = (/) ((-) max min) (f64.i64 (n-1)) in

  -- map (λm.(m * interval + min)) ιn
  let input = map ((<-<) ((+) min) ((>->) (f64.i64) ((*) interval))) (iota n) in

  -- let input = iota n |> map (f64.i64) |> map ()
  let prog = \initial_v -> ir_2.([
    #cnst initial_v,
    #store 1,
    #cnst  0.5f64,
    #mul   1
  ]) in
  map (\f -> ir_2.(eval (init 0) (prog f)))
      input
  |> map ((flip (ir_2.get)) 0)
