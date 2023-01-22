-- ==
-- entry: half_branch_complex fac_branch_complex
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
--module vm_i64 = interp_vector_8_branch_complex i64

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


entry fac_branch_complex [n] (a: [n]f64) : [n]f64 =
  let prog (offset: i64): []vm.instruction = vm.(
    -- f (x) = !x  -- assume x is initially in ra
    [ #store rc    -- store  x in rb -- counter

    , #cnst  1f64   -- store 1 in rc
    , #store rb
    , #store rd     -- sum

    , #load  rb      -- if x < 1 then jmp END
    , #cmp   rc
    , #jmpgt (offset + 14i64)
                     -- else

    , #load  rc      -- counter -= 1
    , #mul   rd      -- sum *= counter
    , #store rd

    , #load  rc      --
    , #sub   rb
    , #store rc      --

    , #jmp (offset + (4i64)) -- jmp START

    -- END
    , #load rd
    , #halt
    ])
  in

  let proglen = length (prog 0) in
  let prog_idx = map ((*) proglen) (iota n)  in
  let programs : []vm.instruction = map prog prog_idx |> flatten in
  let states   : [n]vm.state = vm.init 0 |> replicate n |> map2 (\v s -> vm.set s vm.ra (v)) a in
  vm.eval states prog_idx programs |> vm.return


entry pop_branch_complex [n] (a: [n]f64) : [n]f64 =
  let proglen = 8 in
  let prog : [8]vm.instruction = vm.(
    [ #push ra
    , #cnst 2f64
    , #push ra
    , #pop
    , #store rb
    , #pop
    , #add rb
    , #halt
    ]) in
  let prog_idx = map (\i -> i * proglen) (iota n) in
  let programs : []vm.instruction = replicate n prog |> flatten
  in
  let states   : [n]vm.state = vm.init 0
      |> replicate n
      |> map2 (\v s -> vm.set s vm.ra v) a in
  vm.eval states prog_idx programs |> vm.return

entry test_branch_complex [n] (a: [n]f64) : [n]f64 =
  let proglen = 6 in
  let fibprog : [8]vm.instruction = vm.([
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
  let prog_idx = map (\i -> i * proglen + length fibprog) (iota n) in
  let programs : []vm.instruction = fibprog ++ (map prog prog_idx |> flatten)
  in
  let states   : [n]vm.state = vm.init 0
      |> replicate n
      |> map2 (\v s -> vm.set s vm.ra v) a in
  vm.eval states prog_idx programs |> vm.return


entry fib_branch_complex [n] (a: [n]f64) : [n]f64 =
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
    , #push rd       -- 17 -- push fib(n-1)

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
    , #cnst 1        -- 35
    , #return        -- 36 -- jump to return address
    ]) in
  let prog (progstart:i64) : [6]vm.instruction = vm.(
    [ #store rb
    , #cnst ((f64.i64 progstart) + 5f64)
    , #push ra
    , #load rb
    , #jmp  0
    , #halt
    ]) in
  let plen = length (prog 0)
  let prog_idx = map (\i -> i * plen + length fibprog) (iota n) in
  let programs : []vm.instruction = fibprog ++ (map prog prog_idx |> flatten)
  in
  let states   : [n]vm.state = vm.init 0
      |> replicate n
      |> map2 (\v s -> vm.set s vm.ra v) a in
  vm.eval states prog_idx programs |> vm.return