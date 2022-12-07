open import "interp_registers"

module ir_2  = interp_list_memory f64 { def numregs =  2 : i64 }
module ir_4  = interp_list_memory f64 { def numregs =  4 : i64 }
module ir_8  = interp_list_memory f64 { def numregs =  8 : i64 }
module ir_16 = interp_list_memory f64 { def numregs = 16 : i64 }
module ir_32 = interp_list_memory f64 { def numregs = 32 : i64 }

module it_4 =  interp_tuple_memory f64

-- ==
-- input { 0f64 100f64    100i64 }
-- input { 0f64 100f64    150i64 }
-- input { 0f64 100f64    200i64 }
-- input { 0f64 100f64    250i64 }
-- input { 0f64 100f64    300i64 }
-- input { 0f64 100f64    350i64 }
-- input { 0f64 100f64    400i64 }
-- input { 0f64 100f64    450i64 }
-- input { 0f64 100f64    500i64 }
-- input { 0f64 100f64   1000i64 }
-- input { 0f64 100f64   1250i64 }
-- input { 0f64 100f64   1500i64 }
-- input { 0f64 100f64   1750i64 }
-- input { 0f64 100f64   2000i64 }
-- input { 0f64 100f64   2500i64 }
-- input { 0f64 100f64   5000i64 }
-- input { 0f64 100f64   7500i64 }
-- input { 0f64 100f64  10000i64 }
-- input { 0f64 100f64  25000i64 }
-- input { 0f64 100f64  50000i64 }
-- input { 0f64 100f64  75000i64 }
-- input { 0f64 100f64 100000i64 }
-- input { 0f64 100f64 125000i64 }
-- input { 0f64 100f64 150000i64 }
-- input { 0f64 100f64 175000i64 }
-- input { 0f64 100f64 200000i64 }
-- input { 0f64 100f64 250000i64 }
-- input { 0f64 100f64 500000i64 }
-- input { 0f64 100f64 750000i64 }
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
