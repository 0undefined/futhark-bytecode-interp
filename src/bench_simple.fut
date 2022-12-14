-- ==
-- entry: dynamic_4 vector_4 tuple_4 pure
-- input {         100i64 }
-- input {         150i64 }
-- input {         200i64 }
-- input {         250i64 }
-- input {         300i64 }
-- input {         350i64 }
-- input {         400i64 }
-- input {         450i64 }
-- input {         500i64 }
-- input {       1_000i64 }
-- input {       1_250i64 }
-- input {       1_500i64 }
-- input {       1_750i64 }
-- input {       2_000i64 }
-- input {       2_500i64 }
-- input {       5_000i64 }
-- input {       7_500i64 }
-- input {      10_000i64 }
-- input {      25_000i64 }
-- input {      50_000i64 }
-- input {      75_000i64 }
-- input {     100_000i64 }
-- input {     125_000i64 }
-- input {     150_000i64 }
-- input {     175_000i64 }
-- input {     200_000i64 }
-- input {     250_000i64 }
-- input {     500_000i64 }
-- input {     750_000i64 }
-- input {   1_000_000i64 }
-- input {   2_500_000i64 }
-- input {   5_000_000i64 }
-- input {   7_500_000i64 }
-- input {  10_000_000i64 }
-- input {  25_000_000i64 }
-- input {  50_000_000i64 }
-- input {  75_000_000i64 }
-- input { 100_000_000i64 }
open import "interp_registers"

module id_4 = interp_dynamic_memory f64 { def numregs =  4 : i64 }
module it_4 = interp_tuple_4_memory f64
module iv_4 = interp_vector_4_memory f64


def gen_input (n: i64) : [n]f64 =
  let (min,max) = (0f64, 100f64) in
  let interval = (/) ((-) max min) (f64.i64 (n-1)) in
  map (\m -> f64.i64 m * interval + min) (iota n)


entry dynamic_4 (n: i64) : [n]f64 =
  let prog = \initial_v -> id_4.([
    #cnst initial_v,
    #store rb,
    #cnst  0.5f64,
    #mul   rb
  ]) in
  map (\f -> id_4.(eval (init 0) (prog f) |> return)) (gen_input n)


entry vector_4 (n: i64) : [n]f64 =
  let prog = \initial_v -> iv_4.([
    #cnst initial_v,
    #store rb,
    #cnst  0.5f64,
    #mul   rb
  ]) in
  map (\f -> iv_4.(eval (init 0) (prog f) |> return)) (gen_input n)


entry tuple_4 (n: i64) : [n]f64 =
  let prog = \initial_v -> it_4.([
      #cnst initial_v
      , #store rb
      , #cnst  0.5f64
      , #mul   rb
  ]) in
  map (\f -> it_4.(eval (init 0) (prog f) |> return)) (gen_input n)


entry pure (n: i64) : [n]f64 =
  let prog = \x -> (x * 0.5) in
  map prog (gen_input n)
