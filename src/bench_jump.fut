-- ==
-- entry: half_branch_complex fac_branch_complex fib_branch_complex fib_simple_branch_complex
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

-- Same as fibprog, but without immediates and call/return
--let fibprog : []vm.instruction = vm.([
--    -- fib:
--      #store rb      --  0 -- assume ra is input, `n` -- move n to rb
--    , #cnst   2      --  1 --
--    , #cmp   rb      --  2 --
--    , #jmpgt 35      --  3 -- if 2 > n -> return
--
--    , #push  rb      --  4 -- save n on the stack        -- stack +1
--    , #cnst   1      --  5 --
--    , #store rc      --  6 --
--    , #load  rb      --  7 --
--    , #sub   rc      --  8 -- ra = n - 1
--
--    -- Calculate Fib (n-1)
--    , #store rd      --  9 -- rd = n-1
--    , #cnst  14      -- 10 -- jump back to fib(n-2)
--    , #push  ra      -- 11
--    , #load  rd      -- 12
--    , #jmp    0      -- 13 -- ra = fib (n-1)
--
--    , #store rd      -- 14 -- rd = fib(n-1)
--    , #pop           -- 15 -- ra = n
--    , #store rb      -- 16 -- rb = n
--    , #push  rd      -- 17 -- push fib(n-1)
--
--    -- Calculate Fib (n-2)
--    , #cnst   2      -- 18 --
--    , #store rc      -- 19 --
--    , #load  rb      -- 20
--    , #sub   rc      -- 21 -- ra = n - 2
--
--    , #store rd      -- 22 -- rd = n - 2
--    , #cnst  27      -- 23 -- jump back to fib(n-1) + fib(n-2)
--    , #push  ra      -- 24 -- pc + 2
--    , #load  rd      -- 25 -- ra = (n-2)
--    , #jmp    0      -- 26 -- ra = fib (n-2)
--
--    , #store rb      -- 27 -- rb = fib (n - 2)
--    , #pop           -- 28 -- ra = fib (n - 1)
--    , #add   rb      -- 29 -- rb = fib (n - 1) + fib (n - 2)
--    , #store rb      -- 30 --
--
--    -- return:
--    , #pop           -- 31 -- pops to ra
--    , #store  rc     -- 32 -- save result in rc
--    , #load   rb     -- 33 -- load result
--    , #jmpreg rc     -- 34 -- jump to return address
--
--    -- return1:
--    , #cnst 1        -- 35
--    , #store rb      -- 36
--    , #pop           -- 37 -- pops to ra
--    , #store  rc     -- 38 -- save result in rc
--    , #load   rb     -- 39 -- load result
--    , #jmpreg rc     -- 40 -- jump to return address
--    ])


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
  , #cnst    (-1)
  , #store   rf   --  4 -- b = rc
  , #cnst    0    --  5 -- set rb and rc to 1
  , #store   rg   --  6 -- set rb and rc to 1

  -- DO {
  -- s = a;
  , #load    rb   --  7 -- s = a
  , #store   rd   --  8 -- s = rd

  -- a = b;
  , #load    rc   --  9 -- a = b
  , #store   rb   -- 10

  -- b += s;
  , #load    rc   -- 11 -- b += s
  , #add     rd   -- 12 --
  , #store   rc   -- 13 --

  -- n -= 1;
  , #load    re   -- 14 -- n = n-1
  , #add     rf   -- 15
  , #store   re   -- 16

  -- } WHILE (N > 0);
  , #jmplt   rg 7 -- 17


  -- RETURN
  , #pop          -- 18
  , #store   rb   -- 19
  , #load    re   -- 20
  , #jmpreg  rb   -- 21 -- jump to return address
])


entry half_branch_complex [n] (a: [n]f64) : [n]f64 =
  let prog (v: f64) : []vm.instruction = vm.(
    [ #cnst  v
    , #store rb
    , #cnst  0.5f64
    , #mul   rb
    , #halt
    ])
  in

  let proglen = length (prog 0) in
  let programs : []vm.instruction = map prog a |> flatten in
  let states   : [n]vm.state = vm.init 0 |> replicate n in
  let prog_idx = map ((*) proglen) (iota n)  in
  vm.eval states prog_idx programs |> vm.return


--entry fib_simple_branch_complex [n] (a: [n]f64) : [n]f64 =
--  let stdprog = fibprog in
--  let prog (progstart:i64) : [6]vm.instruction = vm.(
--    [ #store rb
--    , #cnst ((f64.i64 progstart) + 5f64)
--    , #push ra
--    , #load rb
--    , #jmp  0
--    , #halt
--    ]) in
--  let plen = length (prog 0)
--  let prog_idx = map (\i -> i * plen + length stdprog) (iota n) in
--  let programs : []vm.instruction = stdprog ++ (map prog prog_idx |> flatten)
--  in
--  let states   : [n]vm.state = vm.init 0
--      |> replicate n
--      |> map2 (\v s -> vm.set s vm.ra v) (map (\x -> x * 1 |> f64.ceil) a) in
--  vm.eval states prog_idx programs |> vm.return


entry fib_tail_branch_complex [n] (a: [n]f64) : [n]f64 =
  let stdprog = fibprog_tail in
  let prog (progstart:i64) : []vm.instruction = vm.(
    [ #store rb
    , #cnst ((f64.i64 progstart) + 7f64)
    , #push ra
    , #load rb
    , #jmp 0
    , #halt
    ]) in
  let plen = length (prog 0)
  let prog_idx = map (\i -> i * plen + length stdprog) (iota n) in
  let programs : []vm.instruction = stdprog ++ (map prog prog_idx |> flatten)
  in
  let states   : [n]vm.state = vm.init 0
      |> replicate n
      |> map2 (\v s -> vm.set s vm.ra v) (map (\x -> x * 1 |> f64.ceil) a) in
  vm.eval states prog_idx programs |> vm.return
