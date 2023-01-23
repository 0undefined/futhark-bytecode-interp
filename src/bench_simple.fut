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

module id_4 = interp_dynamic_memory real { def numregs =  4 : i64 }
module it_4 = interp_tuple_4_memory real
module iv_4 = interp_vector_4_memory real


-- The following functions are all the same, but for their respective
-- interpreter memory type.

-- Interpreter Dynamic Memory 4 initializer functions:
def init_states_id4 [n] vv =
  replicate n (id_4.init 0) |> map2 (\v s -> id_4.set s id_4.ra v) vv

def init_programs_id4 m p =
  replicate m p |> flatten

def prog_state_init_id4 [n] [m] (p: [m]id_4.instruction) (vv: [n]f64) =
  let starting_indices = map (\i -> i * m) (iota n) in
  let programs         = init_programs_id4 n p in
  let states           = init_states_id4 vv in
    (states, starting_indices, programs)

-- Interpreter Vector Memory 4 initializer functions:
def init_states_iv4 [n] vv =
  replicate n (iv_4.init 0) |> map2 (\v s -> iv_4.set s iv_4.ra v) vv

def init_programs_iv4 m p =
  replicate m p |> flatten

def prog_state_init_iv4 [n] [m] (p: [m]iv_4.instruction) (vv: [n]f64) =
  let starting_indices = map (\i -> i * m) (iota n) in
  let programs         = init_programs_iv4 n p in
  let states           = init_states_iv4 vv in
    (states, starting_indices, programs)

-- Interpreter Vector Memory 4 initializer functions:
def init_states_it4 [n] vv =
  replicate n (it_4.init 0) |> map2 (\v s -> it_4.set s it_4.ra v) vv

def init_programs_it4 m p =
  replicate m p |> flatten

def prog_state_init_it4 [n] [m] (p: [m]it_4.instruction) (vv: [n]f64) =
  let starting_indices = map (\i -> i * m) (iota n) in
  let programs         = init_programs_it4 n p in
  let states           = init_states_it4 vv in
    (states, starting_indices, programs)




--| Run halving function purely in futhark
entry half_pure [n] (a: [n]f64) : [n]f64 =
  map ((*) 0.5) a


--| Run halving function using interpreter with dynamic allocated memory
entry half_d4 [n] (a: [n]f64) : [n]f64 =
  let prog : [4]id_4.instruction = id_4.(
    [ #store rb
    , #cnst  0.5f64
    , #mul   rb
    , #halt
    ])
  in

  let (states, starting_indices, programs) = prog_state_init_id4 prog a in
  id_4.eval states starting_indices programs |> id_4.return


--| Run halving function using interpreter with vectorized memory
entry half_v4 [n] (a: [n]f64) : [n]f64 =
  let prog = iv_4.([ #store rb, #cnst 0.5f64, #mul rb, #halt ]) in

  let (states, starting_indices, programs) = prog_state_init_iv4 prog a in
  iv_4.eval states starting_indices programs |> iv_4.return


--| Run halving function using interpreter using tuple as memory structure
entry half_t4 [n] (a: [n]f64) : [n]f64 =
  let prog = it_4.([ #store rb, #cnst 0.5f64, #mul rb, #halt ]) in

  let (states, starting_indices, programs) = prog_state_init_it4 prog a in
  it_4.eval states starting_indices programs |> it_4.return


--| Calculate euclidean distance purely in futhark
entry euler_pure [n] (a: [n]f64) (b: [n]f64) : [n]f64 =
  map2 (\x y -> f64.sqrt (x*x + y*y)) a b


--| Calculate euclidean distance with dynamic memory
entry euler_d4 [n] (a: [n]f64) (b: [n]f64) : [n]f64 =
  let prog = id_4.(
  [ -- dist(x, y) = sqrt(x^2 + y^2)
    -- assume ra=y and rb=x
    -- y ^ 2
      #mul   ra
    , #store rc

    -- x ^ 2
    , #load  rb
    , #mul   ra

    -- (+) y^2
    , #add   rc

    -- (sqrt)
    , #sqrt
    , #halt
  ]) in
  let (states, starting_indices, programs) = prog_state_init_id4 prog a in
  id_4.eval id_4.(map2 (\s x-> set s rb x) states b) starting_indices programs |> id_4.return


--| Calculate euclidean distance with vectorized memory
entry euler_v4 [n] (a: [n]f64) (b: [n]f64) : [n]f64 =
  let prog = iv_4.([ #mul ra, #store rc, #load rb, #mul ra, #add rc, #sqrt, #halt ]) in

  let (states, starting_indices, programs) = prog_state_init_iv4 prog a in
  iv_4.eval iv_4.(map2 (\s x-> set s rb x) states b) starting_indices programs |> iv_4.return


--| Calculate euclidean distance using tuple as memory structure
entry euler_t4 [n] (a: [n]f64) (b: [n]f64) : [n]f64 =
  let prog = it_4.([ #mul ra, #store rc, #load rb, #mul ra, #add rc, #sqrt, #halt ]) in

  let (states, starting_indices, programs) = prog_state_init_it4 prog a in
  it_4.eval it_4.(map2 (\s x-> set s rb x) states b) starting_indices programs |> it_4.return
