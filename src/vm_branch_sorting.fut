open import "interpreter"
local import "lib/github.com/diku-dk/sorts/radix_sort"


module interp_vector_8_branch (t: memtype) : interpreter_branch
  with u = t.t
  with idx = i64
  = {

  type u = t.t

  import "lib/github.com/athas/vector/vector"
  module v2 = cat_vector vector_1 vector_1
  module v4 = cat_vector v2 v2
  module v8 = cat_vector v4 v4
  module v16 = cat_vector v8 v8
  module v32 = cat_vector v16 v16

  type stack =
    { mem:  v32.vector u -- callstack[head] is the current PC
    , head: i64          -- keeps track of the current frame in the callstack
    }

  type state =
    { mem: v8.vector u
    , pc: i64
    , stack: stack
    }

  def stack_init (v: u)       : stack = {mem = v32.replicate v, head = 0i64}
  def stack_peek (s: state)   : u     = v32.get s.stack.head s.stack.mem

  def stack_push (s: state) (v: u) : state =
    let h = assert (s.stack.head + 1 < 32) s.stack.head in
    s with stack.mem  = v32.set h v s.stack.mem
      with stack.head = h + 1
      with pc         = s.pc + 1

  def stack_pop (s: state) : (state, u) =
    let h = assert (s.stack.head > 0) s.stack.head in
    let s'     = s with stack.head = h - 1
    --             with pc = s.pc + 1
    in
    let retval = v32.get (h - 1) s.stack.mem --|> trace
    in (s', retval)

  type idx = i64
  let ra : idx = 0
  let rb : idx = 1
  let rc : idx = 2
  let rd : idx = 3
  let re : idx = 4
  let rf : idx = 5
  let rg : idx = 6
  let rh : idx = 7

  type instruction = instruction_jump idx u

  def init v : state =
    { mem       = v8.replicate v
    , stack     = stack_init v
    , pc        = 0
    }

  def get    (s: state) (i:idx)   : u     = v8.get i s.mem
  def set    (s: state) (i:idx) v : state = s with mem = v8.set i v s.mem
                                              with pc  = s.pc + 1

  def return [n] : [n]state -> [n]u = map (\s' -> v8.get 0 s'.mem)

  def (+) (a: u) (b: u) = t.(a + b)
  def (*) (a: u) (b: u) = t.(a * b)
  def (/) (a: u) (b: u) = t.(a / b)
  def (-) (a: u) (b: u) = t.(a - b)
  def sqrt (a: u)       = t.sqrt a
  def to_i64 (a:u)      = t.to_i64 a

  def (==) (a: u) (b: u) = t.(a == b)
  def (<) (a: u) (b: u)  = t.(a < b)
  --def (!=) (a: vm.instruction) (b: vm.instruction) = vm.instruction.(!=) a b

  def jmp (s: state) (offset: i64) = s with pc = offset

  def eval [m] [n] (s: [m]state) (pidx: [m]i64) (p: [n]instruction) =

    let step (s: state) : state =
      let fstval : u = get s ra in
      match p[s.pc]
      case #add  index -> (+) fstval (get s index) |> set s ra
      case #mul  index -> (*) fstval (get s index) |> set s ra
      case #sqrt       -> sqrt fstval              |> set s ra

      case #cnst v -> set s ra v

      case #store index -> set s index fstval
      case #load  index -> get s index |> set s ra
      case #push  index -> stack_push s (get s index)
      case #pop         -> let (s', v) = stack_pop s in set s' ra v

      -- Jump around!
      case #jmp    offset     -> jmp s offset
      case #jmpreg idx        -> jmp s (get s idx |> t.to_i64)
      case #jmplt  idx offset -> if (<) fstval (get s idx) then jmp s offset else s with pc = i64.(s.pc + 1)

      case #halt -> s --with pc = -1
    in

    -- Normalize pc to be within [0; n]
    let normalize_pc (pc: i64) (program_idx: i64) : i64 =
      -- yields >= 0 if in the program itself, < 0 if the program counter is
      -- within some prepended stdlib
      let pc' = i64.(pc - program_idx)
      in if i64.(pc' < 0) then pc else pc'

    -- Determine wether or not we should sort the indices
    let should_sort_const [m] (normalized_pcs: [m]i64) : bool =
      let npc' = rotate (-1) normalized_pcs in
      map2 (i64.-) normalized_pcs npc' |> i64.maximum
      -- MaximumProgramCounterDiff > 3
      |> i64.((<) 3)

    -- Determine wether or not we should sort the indices
    let should_sort_half [m] (normalized_pcs: [m]i64) : bool =
      let npc' = rotate (-1) normalized_pcs in
      map2 (i64.-) normalized_pcs npc' |> i64.maximum
      -- MaximumProgramCounterDiff > (m / n) / 2
      |> i64.((<) (m / n / 2))

    -- Determine wether or not we should sort the indices
    let should_sort [m] (normalized_pcs: [m]i64) : bool =
      let npc' = rotate (-1) normalized_pcs in
      map2 (i64.-) normalized_pcs npc' |> i64.maximum
      -- MaximumProgramCounterDiff > (m / n)
      |> i64.((<) (m / n))


    let sort' 'a (xs: [m](i64,a)) = radix_sort_by_key (.0) (i64.num_bits) (i64.get_bit) xs
    in

    let sort (ids: [m]i64) (states: [m]state) : ([m]i64, [m]state) =
      let normalized_pcs : [m]i64 =
        map2 (\i s ->
          normalize_pc s.pc pidx[i]
        ) ids states
      in
        if ! should_sort normalized_pcs then
          (ids, states)
        else
          -- TODO: Sort only when entropy > threshold
          zip ids states        -- [m](ids, states)
          |> zip normalized_pcs -- [m](npc, (ids, states))
          |> sort'
          |> unzip
          |> (.1)
          |> unzip -- ([m]ids, [m]states)


    -- sort states by their original ID's s.t. they're in order again
    in ((.1) <-< unzip <-< sort' <-< uncurry zip) <|
      -- States are paired with their original index
      loop (i, s') = (iota m, map2 (\(s':state) pc -> s' with pc = pc) s pidx)
      while (any (\ss ->
        match p[ss.pc]
        case #halt -> false
        case _     -> true
        ) s')
      do let (ii,ss) = map step s' |> sort i in (ii,ss)
}
