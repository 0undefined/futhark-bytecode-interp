-- ==
-- entry: half_branch_jump fac_branch_complex fib_branch_complex fib_simple_branch_complex
-- random input {         [100]f64 }
-- random input {         [500]f64 }
-- random input {       [1_000]f64 }
-- random input {       [5_000]f64 }
-- random input {      [10_000]f64 }
-- random input {      [50_000]f64 }
-- random input {     [100_000]f64 }
-- random input {     [500_000]f64 }
-- random input {   [1_000_000]f64 }
open import "vm_branch_simple"

module real = f64

module vm = interp_vector_8_branch real


def init_states [n] vv =
  replicate n (vm.init 0) |> map2 (\v s -> vm.set s vm.ra v) vv


def init_programs [n] [m] (p: i64 -> [m]vm.instruction) (oo: [n]i64) : [] vm.instruction =
  map p oo |> flatten


def prog_state_init [n] [m] (stdlib: []vm.instruction) (p: i64 -> [m]vm.instruction) (a: [n]f64) : ([n]vm.state, [n]i64, []vm.instruction) =
  let proglen          = length (p 0) in
  let starting_indices = map (\i -> i * proglen + length stdlib) (iota n) in
  let programs         = stdlib ++ init_programs p starting_indices in
  let states           = init_states a in
    (states, starting_indices, programs)


-- Simple non-tail recursive fibonacci sequence
let fibprog : []vm.instruction = vm.([
    -- fib:
      #store rb      --  0 -- assume ra is input, `n` -- move n to rb
    , #cnst   2      --  1 --
    , #store rc      --  2
    , #load  rb      --  3
    , #jmplt rc 36   --  4 -- if n < 2 -> return

    , #push  rb      --  5 -- save n on the stack        -- stack +1
    , #cnst (-1)     --  6 --
    , #store rc      --  7 --
    , #load  rb      --  8 --
    , #add   rc      --  9 -- ra = n - 1

    -- Calculate Fib (n-1)
    , #store rd      -- 10 -- rd = n-1
    , #cnst  15      -- 11 -- jump back to fib(n-2)
    , #push  ra      -- 12
    , #load  rd      -- 13
    , #jmp    0      -- 14 -- ra = fib (n-1)

    , #store rd      -- 15 -- rd = fib(n-1)
    , #pop           -- 16 -- ra = n
    , #store rb      -- 17 -- rb = n
    , #push  rd      -- 18 -- push fib(n-1)

    -- Calculate Fib (n-2)
    , #cnst (-2)     -- 19 --
    , #store rc      -- 20 --
    , #load  rb      -- 21
    , #add   rc      -- 22 -- ra = n - 2

    , #store rd      -- 23 -- rd = n - 2
    , #cnst  29      -- 24 -- jump back to fib(n-1) + fib(n-2)
    , #push  ra      -- 25 -- pc + 2
    , #load  rd      -- 26 -- ra = (n-2)
    , #jmp    0      -- 27 -- ra = fib (n-2)

    , #store rb      -- 28 -- rb = fib (n - 2)
    , #pop           -- 29 -- ra = fib (n - 1)
    , #add   rb      -- 30 -- rb = fib (n - 1) + fib (n - 2)
    , #store rb      -- 31 --

    -- return:
    , #pop           -- 32 -- pops to ra
    , #store  rc     -- 33 -- save result in rc
    , #load   rb     -- 34 -- load result
    , #jmpreg rc     -- 35 -- jump to return address

    -- return1:
    , #cnst 1        -- 36
    , #store rb      -- 37
    , #pop           -- 38 -- pops to ra
    , #store  rc     -- 39 -- save result in rc
    , #load   rb     -- 40 -- load result
    , #jmpreg rc     -- 41 -- jump to return address
    ])


let fibprog_tail : []vm.instruction = vm.([
  -- int a = 1;
  -- int b = 1;
  -- do {
  --   int s = a;
  --   a = b;
  --   b += s;
  --   n--;
  -- } while (n > 0);
  -- return a;
  -- BEGIN
    #store   re   --  0 -- rd = n
  , #cnst    1    --  1 -- set rb and rc to 1
  , #store   rb   --  2 -- a = rb
  , #store   rc   --  3 -- b = rc
  , #cnst    (-1) --  4
  , #store   rf   --  5 -- b = rc
  , #cnst    0    --  6 -- set rb and rc to 1
  , #store   rg   --  7 -- set rb and rc to 1

  -- DO {
  -- s = a;
  , #load    rb   --  8 -- s = a
  , #store   rd   --  9 -- s = rd

  -- a = b;
  , #load    rc   -- 10 -- a = b
  , #store   rb   -- 11

  -- b += s;
  , #add     rd   -- 12 --
  , #store   rc   -- 13 --

  -- n -= 1;
  , #load    re   -- 14 -- n = n-1
  , #add     rf   -- 15
  , #store   re   -- 16

  -- } WHILE (N > 0);
  , #load    rg   -- 17
  , #jmplt   re 8 -- 18


  -- RETURN
  , #pop          -- 19
  , #store   rg   -- 20
  , #load    rb   -- 21
  , #jmpreg  rg   -- 22 -- jump to return address
])


entry half_branch_jump [n] (a: [n]f64) : [n]f64 =
  let stdlib = [] in
  let prog _ : []vm.instruction = vm.(
    [ #store rb
    , #cnst  0.5f64
    , #mul   rb
    , #halt
    ])
  in

  let (states, starting_indices, programs) = prog_state_init stdlib prog a in
  vm.eval states starting_indices programs |> vm.return


entry fib_simple_branch_complex [n] (a: [n]f64) : [n]f64 =
  let stdlib = copy fibprog in
  let prog (progstart:i64) : [6]vm.instruction = vm.(
    [ #store rb
    , #cnst ((f64.i64 progstart) + 5f64)
    , #push ra
    , #load rb
    , #jmp  0
    , #halt
    ]) in

  let (states, starting_indices, programs) = prog_state_init stdlib prog a in
  vm.eval states starting_indices programs |> vm.return


entry fib_tail_jump [n] (a: [n]f64) : [n]f64 =
  let stdlib = copy fibprog_tail in
  let prog (progstart:i64) : []vm.instruction = vm.(
    [ #store rb
    , #cnst ((f64.i64 progstart) + 5f64)
    , #push ra
    , #load rb
    , #jmp 0
    , #halt
    ]) in

  let (states, starting_indices, programs) = prog_state_init stdlib prog a in
  vm.eval states starting_indices programs |> vm.return
