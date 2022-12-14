open import "interp_registers"

module ir_2  = interp_list_memory f64 { def numregs =  2 : i64 }
module ir_4  = interp_list_memory f64 { def numregs =  4 : i64 }
module ir_8  = interp_list_memory f64 { def numregs =  8 : i64 }
module ir_16 = interp_list_memory f64 { def numregs = 16 : i64 }
module ir_32 = interp_list_memory f64 { def numregs = 32 : i64 }

module it_4 =  interp_tuple_memory f64

-- ==
-- entry: main tuple_4 pure
--- input { 0f64 100f64    100i64 }
--- input { 0f64 100f64    150i64 }
--- input { 0f64 100f64    200i64 }
--- input { 0f64 100f64    250i64 }
--- input { 0f64 100f64    300i64 }
--- input { 0f64 100f64    350i64 }
--- input { 0f64 100f64    400i64 }
--- input { 0f64 100f64    450i64 }
--- input { 0f64 100f64    500i64 }
--- input { 0f64 100f64   1000i64 }
--- input { 0f64 100f64   1250i64 }
--- input { 0f64 100f64   1500i64 }
--- input { 0f64 100f64   1750i64 }
--- input { 0f64 100f64   2000i64 }
--- input { 0f64 100f64   2500i64 }
--- input { 0f64 100f64   5000i64 }
--- input { 0f64 100f64   7500i64 }
--- input { 0f64 100f64  10000i64 }
--- input { 0f64 100f64  25000i64 }
--- input { 0f64 100f64  50000i64 }
--- input { 0f64 100f64  75000i64 }
--- input { 0f64 100f64 100000i64 }
--- input { 0f64 100f64 125000i64 }
--- input { 0f64 100f64 150000i64 }
--- input { 0f64 100f64 175000i64 }
--- input { 0f64 100f64 200000i64 }
--- input { 0f64 100f64 250000i64 }
--- input { 0f64 100f64 500000i64 }
--- input { 0f64 100f64 750000i64 }
entry main (n: i64) : [n]f64 =
  let (min,max) = (0f64, 100f64) in
  -- (max - min) / n
  let interval = (/) ((-) max min) (f64.i64 (n-1)) in

  let input = map (\m -> f64.i64 m * interval + min) (iota n) in

  -- let input = iota n |> map (f64.i64) |> map ()
  let prog = \initial_v -> ir_2.([
    #cnst initial_v,
    #store rb,
    #cnst  0.5f64,
    #mul   rb
  ]) in
  map (\f -> ir_2.(eval (init 0) (prog f) |> return)) input


entry tuple_4 (n: i64) : [n]f64 =
  let (min,max) = (0f64, 100f64) in
  -- (max - min) / n
  let interval = (/) ((-) max min) (f64.i64 (n-1)) in

  let input = map (\m -> f64.i64 m * interval + min) (iota n) in

  -- let input = iota n |> map (f64.i64) |> map ()
  let prog = \initial_v -> it_4.([
      #cnst initial_v
      , #store rb
      , #cnst  0.5f64
      , #mul   rb
  ]) in
  map (\f -> it_4.(eval (init 0) (prog f) |> return)) input


entry pure (n: i64) : [n]f64 =
  let (min,max) = (0f64, 100f64) in
  let interval = (/) ((-) max min) (f64.i64 (n-1)) in

  let input = map (\m -> f64.i64 m * interval + min) (iota n) in

  let prog = \x -> (x * 0.5) in
  map prog input
