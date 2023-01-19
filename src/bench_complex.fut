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
open import "vm_branch_complex"

import "lib/github.com/diku-dk/segmented/segmented"

module real = f64

module vm = interp_vector_8_branch_complex real

entry half_branch_complex [n] (a: [n]f64) : [n]f64 =
  let prog (v: f64) (i: i64) : vm.instruction = vm.(
    [ #cnst  v
    , #store rb
    , #cnst  0.5f64
    , #mul   rb
    , #halt
    ])[i]
  in

  let programs : []vm.instruction = expand (\_->5) prog a in
  let states   : [n]vm.state = vm.init 0 |> replicate n in
  let prog_idx = map ((*) 5) (iota n)  in
  vm.eval states prog_idx programs |> vm.return
