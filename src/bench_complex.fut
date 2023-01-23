-- ==
-- entry: half_simplified_branch_complex half_branch_complex fac_branch_complex fib_branch_complex fib_simple_branch_complex
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

module real = f64

module vm = interp_vector_8_branch_complex real


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




-- Same as fibprog, but without immediates and call/return
-- This is almost identical to the one in bench_jump.fut
-- In F#:
-- let rec fib = function
--   | 0 | 1 -> 1
--   | n     -> fib (n-1) + fib (n-2)
let fibprog : []vm.instruction = vm.([
  -- fib:
    #store rb      --  0 -- assume ra is input, `n` -- move n to rb
  , #cnst   2      --  1 --
  , #cmp   rb      --  2 --
  , #jmpgt 35      --  3 -- if 2 > n -> return

  , #push  rb      --  4 -- save n on the stack        -- stack +1
  , #cnst   1      --  5 --
  , #store rc      --  6 --
  , #load  rb      --  7 --
  , #sub   rc      --  8 -- ra = n - 1

  -- Calculate Fib (n-1)
  , #store rd      --  9 -- rd = n-1
  , #cnst  14      -- 10 -- jump back to fib(n-2)
  , #push  ra      -- 11
  , #load  rd      -- 12
  , #jmp    0      -- 13 -- ra = fib (n-1)

  , #store rd      -- 14 -- rd = fib(n-1)
  , #pop           -- 15 -- ra = n
  , #store rb      -- 16 -- rb = n
  , #push  rd      -- 17 -- push fib(n-1)

  -- Calculate Fib (n-2)
  , #cnst   2      -- 18 --
  , #store rc      -- 19 --
  , #load  rb      -- 20
  , #sub   rc      -- 21 -- ra = n - 2

  , #store rd      -- 22 -- rd = n - 2
  , #cnst  27      -- 23 -- jump back to fib(n-1) + fib(n-2)
  , #push  ra      -- 24 -- pc + 2
  , #load  rd      -- 25 -- ra = (n-2)
  , #jmp    0      -- 26 -- ra = fib (n-2)

  , #store rb      -- 27 -- rb = fib (n - 2)
  , #pop           -- 28 -- ra = fib (n - 1)
  , #add   rb      -- 29 -- rb = fib (n - 1) + fib (n - 2)
  , #store rb      -- 30 --

  -- return:
  , #pop           -- 31 -- pops to ra
  , #store  rc     -- 32 -- save result in rc
  , #load   rb     -- 33 -- load result
  , #jmpreg rc     -- 34 -- jump to return address

  -- return1:
  , #cnst    1     -- 35
  , #store  rb     -- 36
  , #pop           -- 37 -- pops to ra
  , #store  rc     -- 38 -- save result in rc
  , #load   rb     -- 39 -- load result
  , #jmpreg rc     -- 40 -- jump to return address
  ])


-- Same as fibprog, but uses the simpler instructions to abbreviate trivial
-- stuff.
-- fib(n) = fib(n - 1) + fib(n - 2)
let fibprog_simplified : []vm.instruction = vm.(
  [ -- fib:
    #cmpi  2      --  0 --
  , #jmplt 16     --  1 -- if n < 2 -> return1

  , #push  ra     --  2 -- save n on the stack
  , #subi  1      --  3 -- ra = n - 1

  -- Calculate Fib (n-1)
  , #call    0    --  4 -- ra = fib (n-1)

  , #store  rd    --  5 -- rd = fib(n-1)
  , #pop          --  6 -- ra = n
  , #store  rb    --  7 -- rb = n
  , #push   rd    --  8 -- push fib(n-1)

  -- Calculate Fib (n-2)
  , #load  rb     --  9
  , #subi   2     -- 10 -- ra = n - 2

  , #call   0    -- 11 -- ra = fib (n-2)

  , #store rb     -- 12 -- rb = fib (n - 2)
  , #pop          -- 13 -- ra = fib (n - 1)
  , #add   rb     -- 14 -- rb = fib (n - 1) + fib (n - 2)
  , #return       -- 15

  -- return1:
  , #cnst   1     -- 16
  , #return       -- 17 -- jump to return address
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

  -- DO {
  -- s = a;
  , #load    rb   --  4 -- s = a
  , #store   rd   --  5 -- s = rd

  -- a = b;
  , #load    rc   --  6 -- a = b
  , #store   rb   --  7

  -- b += s;
  , #load    rc   --  8 -- b += s
  , #add     rd   --  9 --
  , #store   rc   -- 10 --

  -- n -= 1;
  , #load    re   -- 11 -- n = n-1
  , #subi    1    -- 12
  , #store   re   -- 13

  -- } WHILE (N > 0);
  , #cmpi    0    -- 14
  , #jmpgt   4    -- 15


  -- RETURN
  , #load    rb   -- 16
  , #return       -- 21 -- jump to return address
])


entry complex_half [n] (a: [n]f64) : [n]f64 =
  let prog (_: i64) : []vm.instruction = vm.(
    [ #store rb
    , #cnst  0.5f64
    , #mul   rb
    , #halt
    ])
  in

  let (states, starting_indices, programs) = prog_state_init [] prog a in
  vm.eval states starting_indices programs |> vm.return


entry complex_half_simplified [n] (a: [n]f64) : [n]f64 =
  let prog (_: i64) : []vm.instruction = vm.(
    [ #muli  0.5f64
    , #halt
    ])
  in

  let (states, starting_indices, programs) = prog_state_init [] prog a in
  vm.eval states starting_indices programs |> vm.return


entry complex_fac [n] (a: [n]f64) : [n]f64 =
  let prog (offset: i64): []vm.instruction = vm.(
    -- f (n) = !n
    -- int factorial(int n) {
    --  int a = 1;
    --  if (n < a) return a;
    --  do {
    --    a *= n;
    --    n--;
    --  } while (n > 0);
    --   return a;
    -- }
    [ #store rc     --  0 -- store  n in rb

    , #cnst   1     --  1 --
    , #store rb     --  2 -- a = 1
    , #store rd     --  3 -- alias rd = 1

    , #load  rc     --  4 -- load is necessary when we jump to here
    , #cmp   rd     --  5 --
    , #jmplt (offset + 13i64) --  6 -- return 1

    , #mul   rb     --  7 --
    , #store rb     --  8 --

    , #load  rc     --  9 -- n--;
    , #sub   rd     -- 10 --
    , #store rc     -- 11 --

    , #jmp (offset + (4i64)) -- 12 -- jmp START

    -- END
    , #load  rb     -- 13
    , #halt         -- 14
    ])
  in

  let (states, starting_indices, programs) = prog_state_init [] prog a in
  vm.eval states starting_indices programs |> vm.return


entry pop_branch_complex [n] (a: [n]f64) : [n]f64 =
  let prog (_: i64) : [8]vm.instruction = vm.(
    [ #push ra
    , #cnst 2f64
    , #push ra
    , #pop
    , #store rb
    , #pop
    , #add rb
    , #halt
    ]) in

  let (states, starting_indices, programs) = prog_state_init [] prog a in
  vm.eval states starting_indices programs |> vm.return


entry test_branch_complex [n] (a: [n]f64) : [n]f64 =
  let std : [8]vm.instruction = vm.([
    -- fib:
      #store  rb      --  0 -- assume ra is input, `n` -- move n to rb
    , #cnst   5f64
    , #add    rb
    , #store  rb
    , #pop
    , #store  rc
    , #load   rb
    , #jmpreg rc
    ]) in
  let prog (progstart:i64) : [6]vm.instruction = vm.(
    [ #store rb
    , #cnst ((f64.i64 progstart) + 5f64)
    , #push ra
    , #load rb
    , #jmp  0
    , #halt
    ]) in

  let (states, starting_indices, programs) = prog_state_init std prog a in
  vm.eval states starting_indices programs |> vm.return


entry fib_complex [n] (a: [n]f64) : [n]f64 =
  let prog (progstart:i64) : [6]vm.instruction = vm.(
    [ #store rb
    , #cnst ((f64.i64 progstart) + 5f64)
    , #push ra
    , #load rb
    , #jmp  0
    , #halt
    ]) in

  let (states, starting_indices, programs) = prog_state_init (copy fibprog) prog a in
  vm.eval states starting_indices programs |> vm.return


entry fib_simplified_complex [n] (a: [n]f64) : [n]f64 =
  let prog (_:i64) : []vm.instruction = vm.(
    [ -- immediately call fib with `ra`
      #call 0
    , #halt
    ]) in

  let (states, starting_indices, programs) = prog_state_init (copy fibprog_simplified) prog a in
  vm.eval states starting_indices programs |> vm.return


entry fib_tail_complex [n] (a: [n]f64) : [n]f64 =
  let prog (_:i64) : []vm.instruction = vm.(
    [ -- immediately call fib with `ra`
      #call 0
    , #halt
    ]) in

  let (states, starting_indices, programs) = prog_state_init (copy fibprog_tail) prog a in
  vm.eval states starting_indices programs |> vm.return
