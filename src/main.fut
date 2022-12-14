open import "interp_registers"

module id_4 = interp_dynamic_memory f64 { def numregs =  4 : i64 }
module it_4 = interp_tuple_4_memory f64
module iv_4 = interp_vector_4_memory f64

-- ==
-- entry: array_2 vec_4 tuple_4 pure
--- input {    100i64 }
--- input {    150i64 }
--- input {    200i64 }
--- input {    250i64 }
--- input {    300i64 }
--- input {    350i64 }
--- input {    400i64 }
--- input {    450i64 }
--- input {    500i64 }
--- input {   1000i64 }
--- input {   1250i64 }
--- input {   1500i64 }
--- input {   1750i64 }
--- input {   2000i64 }
--- input {   2500i64 }
--- input {   5000i64 }
--- input {   7500i64 }
--- input {  10000i64 }
--- input {  25000i64 }
--- input {  50000i64 }
--- input {  75000i64 }
--- input { 100000i64 }
--- input { 125000i64 }
--- input { 150000i64 }
--- input { 175000i64 }
--- input { 200000i64 }
--- input { 250000i64 }
--- input { 500000i64 }
--- input { 750000i64 }
entry array_4 (n: i64) : [n]f64 =
  let (min,max) = (0f64, 100f64) in
  -- (max - min) / n
  let interval = (/) ((-) max min) (f64.i64 (n-1)) in

  let input = map (\m -> f64.i64 m * interval + min) (iota n) in

  -- let input = iota n |> map (f64.i64) |> map ()
  let prog = \initial_v -> id_4.([
    #cnst initial_v,
    #store rb,
    #cnst  0.5f64,
    #mul   rb
  ]) in
  map (\f -> id_4.(eval (init 0) (prog f) |> return)) input


entry vec_4 (n: i64) : [n]f64 =
  let (min,max) = (0f64, 100f64) in
  -- (max - min) / n
  let interval = (/) ((-) max min) (f64.i64 (n-1)) in

  let input = map (\m -> f64.i64 m * interval + min) (iota n) in

  -- let input = iota n |> map (f64.i64) |> map ()
  let prog = \initial_v -> iv_4.([
    #cnst initial_v,
    #store rb,
    #cnst  0.5f64,
    #mul   rb
  ]) in
  map (\f -> iv_4.(eval (init 0) (prog f) |> return)) input


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
