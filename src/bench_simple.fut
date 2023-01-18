-- ==
-- entry: half_d4 half_v4 half_t4 half_pure
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
-- entry: euler_d4 euler_v4 euler_t4 euler_pure
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

module id_4 = interp_dynamic_memory real { def numregs =  4 : i64 }
module it_4 = interp_tuple_4_memory real
module iv_4 = interp_vector_4_memory real


--| Run halving function using interpreter with dynamic allocated memory
entry half_d4 [n] (a: [n]f64) : [n]f64 =
  let prog (v: f64) (i: i64) : id_4.instruction = id_4.(
    [ #cnst  v
    , #store rb
    , #cnst  0.5f64
    , #mul   rb
    , #halt
    ])[i]
  in

  let programs : []id_4.instruction = expand (\_->5) prog a in
  let states   : [n]id_4.state = id_4.init 0 |> replicate n in
  let prog_idx = map ((*) 5) (iota n)  in
  id_4.eval states prog_idx programs |> id_4.return


--| Run halving function using interpreter with vectorized memory
entry half_v4 [n] (a: [n]f64) : [n]f64 =
  let prog (v: f64) (i: i64) : iv_4.instruction = iv_4.(
    [ #cnst  v
    , #store rb
    , #cnst  0.5f64
    , #mul   rb
    , #halt
    ])[i]
  in

  let programs : []iv_4.instruction = expand (\_->5) prog a in
  let states   : [n]iv_4.state = iv_4.init 0 |> replicate n in
  let prog_idx = map ((*) 5) (iota n)  in
  iv_4.eval states prog_idx programs |> iv_4.return


--| Run halving function using interpreter using tuple as memory structure
entry half_t4 [n] (a: [n]f64) : [n]f64 =
  let prog (v: f64) (i: i64) : it_4.instruction = it_4.(
    [ #cnst  v
    , #store rb
    , #cnst  0.5f64
    , #mul   rb
    , #halt
    ])[i]
  in

  let programs : []it_4.instruction = expand (\_->5) prog a in
  let states   : [n]it_4.state = it_4.init 0 |> replicate n in
  let prog_idx = map ((*) 5) (iota n)  in
  it_4.eval states prog_idx programs |> it_4.return


--| Run halving function purely in futhark
entry half_pure [n] (a: [n]f64) : [n]f64 =
  map ((*) 0.5) a


--| Calculate euclidean distance with dynamic memory
entry euler_d4 [n] (a: [n]f64) (b: [n]f64) : [n]f64 =
  let prog ((x,y): (f64,f64)) (i: i64) : id_4.instruction = id_4.(
  [ -- dist(x, y) = sqrt(x^2 + y^2)
      #cnst  x
    , #store rb
    , #cnst  y

    -- y ^ 2
    , #mul   ra
    , #store rc

    -- x ^ 2
    , #load  rb
    , #mul   ra

    -- (+) y^2
    , #add   rc

    -- (sqrt)
    , #sqrt
    , #halt
  ])[i] in
  let programs : []id_4.instruction = expand (\_->10) prog (zip a b) in
  let states   : [n]id_4.state = id_4.init 0 |> replicate n in
  let prog_idx = map ((*) 10) (iota n)  in
  id_4.eval states prog_idx programs |> id_4.return


--| Calculate euclidean distance with vectorized memory
entry euler_v4 [n] (a: [n]f64) (b: [n]f64) : [n]f64 =
  let prog ((x,y): (f64,f64)) (i: i64) : iv_4.instruction = iv_4.(
  [ -- dist(x, y) = sqrt(x^2 + y^2)
      #cnst  x
    , #store rb
    , #cnst  y

    -- y ^ 2
    , #mul   ra
    , #store rc

    -- x ^ 2
    , #load  rb
    , #mul   ra

    -- (+) y^2
    , #add   rc

    -- (sqrt)
    , #sqrt
    , #halt
  ])[i] in
  let programs : []iv_4.instruction = expand (\_->10) prog (zip a b) in
  let states   : [n]iv_4.state = iv_4.init 0 |> replicate n in
  let prog_idx = map ((*) 10) (iota n)  in
  iv_4.eval states prog_idx programs |> iv_4.return


--| Calculate euclidean distance using tuple as memory structure
entry euler_t4 [n] (a: [n]f64) (b: [n]f64) : [n]f64 =
  let prog ((x,y): (f64,f64)) (i: i64) : it_4.instruction = it_4.(
  [ -- dist(x, y) = sqrt(x^2 + y^2)
      #cnst  x
    , #store rb
    , #cnst  y

    -- y ^ 2
    , #mul   ra
    , #store rc

    -- x ^ 2
    , #load  rb
    , #mul   ra

    -- (+) y^2
    , #add   rc

    -- (sqrt)
    , #sqrt
    , #halt
  ])[i] in
  let programs : []it_4.instruction = expand (\_->10) prog (zip a b) in
  let states   : [n]it_4.state = it_4.init 0 |> replicate n in
  let prog_idx = map ((*) 10) (iota n)  in
  it_4.eval states prog_idx programs |> it_4.return


--| Calculate euclidean distance purely in futhark
entry euler_pure [n] (a: [n]f64) (b: [n]f64) : [n]f64 =
  map2 (\x y -> f64.sqrt (x*x + y*y)) a b
