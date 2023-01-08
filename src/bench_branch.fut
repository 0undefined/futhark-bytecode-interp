-- ==
-- entry: branch_complex
-- input {         100i64 }
open import "vm_branch_complex"

module ivc_4 = interp_vector_4_branch_complex f64

asdasd

def gen_input (n: i64) : [n](f64, f64) =
  replicate n (3f64, 6f64)

def branch_complex (n : i64) : [n]f64 =
  let p = \a b -> ivc_4.([
    -- dist(a, b) = sqrt(a^2 + b^2)
      #cnst b
    , #store 1
    , #cnst a

    -- a ^ 2
    , #mul   0
    , #store 2

    -- b ^ 2
    , #load  1
    , #mul   0

    -- (+) a^2
    , #add   1

    -- (sqrt)
    , #sqrt
  ]) in
  map (\f -> ivc_4.(eval (init 0) (prog f) |> return)) (gen_input n)
