-- ==
-- entry: half_pure half_d4 half_v4 half_t4
-- random input {         [100]f64 }
-- random input {         [500]f64 }
-- random input {       [1_000]f64 }
-- random input {       [5_000]f64 }
-- random input {      [10_000]f64 }
-- random input {      [50_000]f64 }
-- random input {     [100_000]f64 }
-- random input {     [500_000]f64 }
-- random input {   [1_000_000]f64 }

-- ==
-- entry: euler_pure euler_d4 euler_v4 euler_t4
-- random input {         [100]f64       [100]f64 }
-- random input {         [500]f64       [500]f64 }
-- random input {       [1_000]f64     [1_000]f64 }
-- random input {       [5_000]f64     [5_000]f64 }
-- random input {      [10_000]f64    [10_000]f64 }
-- random input {      [50_000]f64    [50_000]f64 }
-- random input {     [100_000]f64   [100_000]f64 }
-- random input {     [500_000]f64   [500_000]f64 }
-- random input {   [1_000_000]f64 [1_000_000]f64 }
open import "vm_simple"

import "lib/github.com/diku-dk/segmented/segmented"


module real = f64

module iv_4 = interp_vector_4_memory real


--| Run halving function using interpreter with vectorized memory
entry half_v4 [n] (a: [n]f64) : [n]f64 =
  let prog (v: f64) : [5]iv_4.instruction = iv_4.(
    [ #cnst  v
    , #store rb
    , #cnst  0.5f64
    , #mul   rb
    , #halt
    ])
  in

  let programs : []iv_4.instruction = flatten <| map (prog) a in --expand (\_->5) prog a in
  let states   : [n]iv_4.state = iv_4.init 0 |> replicate n in
  let prog_idx = map ((*) 5) (iota n)  in

  iv_4.eval states prog_idx programs |> iv_4.return


