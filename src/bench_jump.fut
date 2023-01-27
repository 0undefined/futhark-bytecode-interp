-- ==
-- entry: jump_half
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

open import "vm_branch_simple"
import "lib/github.com/diku-dk/cpprandom/random"

module rng_engine = minstd_rand
module rand = uniform_real_distribution f64 rng_engine

module real = f64

module vm = interp_vector_8_branch real


def pad [n] (p: [n]vm.instruction) (sz:i64) : [sz]vm.instruction =
  if n == sz then p :> [sz]vm.instruction
  else
    let padding = replicate (sz - n) vm.(#halt)
    in (p ++ padding) :> [sz]vm.instruction


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

-- ==
-- entry: jump_fac jump_fib jump_fib_tail
-- script input { rand_10     128i64 }
-- script input { rand_10     256i64 }
-- script input { rand_10     512i64 }
-- script input { rand_10    1024i64 }
-- script input { rand_10    2048i64 }
-- script input { rand_10    4096i64 }
-- script input { rand_10    8192i64 }
-- script input { rand_10   16384i64 }
-- script input { rand_10   32768i64 }
-- script input { rand_10   65536i64 }
-- script input { rand_10  131072i64 }
-- script input { rand_10  262144i64 }
-- script input { rand_10  524288i64 }
-- script input { rand_10 1048576i64 }
-- script input { rand_10 2097152i64 }
-- script input { rand_10 4194304i64 }

--| Yield random values in range [0;10]
entry rand_10 (n: i64) : [n]f64 =
  rng_engine.rng_from_seed [0] -- just use zero as seed
  |> rng_engine.split_rng n
  |> map ((.1) <-< rand.rand (0, 10))



-- Simple non-tail recursive fibonacci sequence
let fibprog : []vm.instruction = vm.([
    -- fib:
      #store rb      --  0 -- assume ra is input, `n` -- move n to rb
    , #cnst   2      --  1 --
    , #store rc      --  2
    , #load  rb       --  3
    , #jmplt rc 36u64 --  4 -- if n < 2 -> return

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
    , #cnst  (f64.from_bits 29)      -- 24 -- jump back to fib(n-1) + fib(n-2)
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


entry jump_half [n] (a: [n]f64) : [n]f64 =
  let prog = vm.(
    [ #store rb
    , #cnst  0.5f64
    , #mul   rb
    , #halt
    ])

  in let starting_indices = replicate n (0, 0)
  in let prog_final = [prog]
  in let states = init_states a

  in vm.eval states starting_indices prog_final |> vm.return


entry jump_fac [n] (a: [n]f64) : [n]f64 =
  let prog : []vm.instruction = vm.(
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
    , #cnst (-1)    --  4
    , #store re     --  5 -- alias rd = 1

    , #load  rc     --  6 -- load is necessary when we jump to here
    , #jmplt rd ((1 << u64.i32 i32.num_bits) | 14u64) --  7 -- return 1

    , #mul   rb     --  8 --
    , #store rb     --  9 --

    , #load  rc     -- 10 -- n--;
    , #add   re     -- 11 --
    , #store rc     -- 12 --

    , #jmp ((1 << u64.i32 i32.num_bits) | 6u64) -- 13 -- jmp START

    -- END
    , #load  rb     -- 14
    , #halt         -- 15
    ])

  in let starting_indices = replicate n (0, 0)
  in let prog_final = [prog]
  in let states = init_states a

  in vm.eval states starting_indices prog_final |> vm.return


entry jump_fib [n] (a: [n]f64) : [n]f64 =
  let stdlib = copy fibprog in
  let prog : [6]vm.instruction = vm.(
    [ #store rb
    , #cnst (f64.from_bits ((1 << u64.i32 i32.num_bits) | 5u64))
    , #push ra
    , #load rb
    , #jmp  0
    , #halt
    ])

  in let maxlen = i64.max (length prog) (length stdlib)
  in let prog' = pad prog maxlen
  in let stdlib' = pad stdlib maxlen

  in let starting_indices = replicate n (1, 0)
  in let prog_final = [stdlib', prog']
  in let states = init_states a
  in vm.eval states starting_indices prog_final |> vm.return


entry jump_fib_tail [n] (a: [n]f64) : [n]f64 =
  let stdlib = copy fibprog_tail in
  let prog : []vm.instruction = vm.(
    [ #store rb
    , #cnst (f64.from_bits ((1 << u64.i32 i32.num_bits) | 5u64))
    , #push ra
    , #load rb
    , #jmp 0
    , #halt
    ])

  in let maxlen = i64.max (length prog) (length stdlib)
  in let prog' = pad prog maxlen
  in let stdlib' = pad stdlib maxlen

  in let starting_indices = replicate n (1, 0)
  in let prog_final = [stdlib', prog']
  in let states = init_states a

  in vm.eval states starting_indices prog_final |> vm.return
