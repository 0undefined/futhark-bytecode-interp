-- ==
-- entry: half_pure half_d4 half_v4 half_t4
-- random input {     [128]f64 }
-- random input {     [256]f64 }
-- random input {     [512]f64 }
-- random input {    [1024]f64 }
-- random input {    [2048]f64 }
-- random input {    [4096]f64 }
-- random input {    [8192]f64 }
-- random input {   [16384]f64 }
-- random input {   [32768]f64 }
-- random input {   [65536]f64 }
-- random input {  [131072]f64 }
-- random input {  [262144]f64 }
-- random input {  [524288]f64 }
-- random input { [1048576]f64 }
-- random input { [2097152]f64 }
-- random input { [4194304]f64 }

-- ==
-- entry: euler_pure euler_d4 euler_v4 euler_t4
-- random input {     [128]f64     [128]f64 }
-- random input {     [256]f64     [256]f64 }
-- random input {     [512]f64     [512]f64 }
-- random input {    [1024]f64    [1024]f64 }
-- random input {    [2048]f64    [2048]f64 }
-- random input {    [4096]f64    [4096]f64 }
-- random input {    [8192]f64    [8192]f64 }
-- random input {   [16384]f64   [16384]f64 }
-- random input {   [32768]f64   [32768]f64 }
-- random input {   [65536]f64   [65536]f64 }
-- random input {  [131072]f64  [131072]f64 }
-- random input {  [262144]f64  [262144]f64 }
-- random input {  [524288]f64  [524288]f64 }
-- random input { [1048576]f64 [1048576]f64 }
-- random input { [2097152]f64 [2097152]f64 }
-- random input { [4194304]f64 [4194304]f64 }
open import "vm_simple"

import "lib/github.com/diku-dk/segmented/segmented"


module real = f64

module id_4 = interp_dynamic_memory real { def numregs =  4 : i64 }
module it_4 = interp_tuple_4_memory real
module iv_4 = interp_vector_4_memory real



--| Run halving function purely in futhark
entry half_pure [n] (a: [n]f64) : [n]f64 =
  map ((*) 0.5) a


--| Run halving function using interpreter with dynamic allocated memory
entry half_d4 [n] (a: [n]f64) : [n]f64 =
  let prog : [n][4]id_4.instruction = map (\v -> id_4.(
  [ #cnst  v
  , #store rb
  , #cnst  0.5f64
  , #mul   rb
  ])) a in
  map (\p -> id_4.(eval (init 0) p |> return)) prog


--| Run halving function using interpreter with vectorized memory
entry half_v4 [n] (a: [n]f64) : [n]f64 =
  let prog = map (\v -> iv_4.(
  [ #cnst  v
  , #store rb
  , #cnst  0.5f64
  , #mul   rb
  ])) a in
  map (\p -> iv_4.(eval (init 0) p |> return)) prog


--| Run halving function using interpreter using tuple as memory structure
entry half_t4 [n] (a: [n]f64) : [n]f64 =
  let prog = map (\initial_v -> it_4.(
  [ #cnst initial_v
  , #store rb
  , #cnst  0.5f64
  , #mul   rb
  ])) a in
  map (\p -> it_4.(eval (init 0) p |> return)) prog


--| Calculate euclidean distance purely in futhark
entry euler_pure [n] (a: [n]f64) (b: [n]f64) : [n]f64 =
  map2 (\x y -> f64.sqrt (x*x + y*y)) a b


--| Calculate euclidean distance with dynamic memory
entry euler_d4 [n] (a: [n]f64) (b: [n]f64) : [n]f64 =
  let prog = map2 (\x y -> id_4.(
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
  ])) a b in
  map (\p -> id_4.(eval (init 0) p |> return)) prog


--| Calculate euclidean distance with vectorized memory
entry euler_v4 [n] (a: [n]f64) (b: [n]f64) : [n]f64 =
  let prog = map2 (\x y -> iv_4.(
  [ -- dist(a, b) = sqrt(a^2 + b^2)
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
  ])) a b in
  map (\p -> iv_4.(eval (init 0) p |> return)) prog


--| Calculate euclidean distance using tuple as memory structure
entry euler_t4 [n] (a: [n]f64) (b: [n]f64) : [n]f64 =
  let prog = map2 (\x y -> it_4.(
  [ -- dist(a, b) = sqrt(a^2 + b^2)
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
  ])) a b in
  map (\p -> it_4.(eval (init 0) p |> return)) prog
