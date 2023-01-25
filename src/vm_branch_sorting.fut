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
    , pc: (i32,i32)
    , stack: stack
    }

  def stack_init (v: u)       : stack = {mem = v32.replicate v, head = 0i64}
  def stack_peek (s: state)   : u     = v32.get s.stack.head s.stack.mem

  def stack_push (s: state) (v: u) : state =
    let h = assert (s.stack.head + 1 < 32) s.stack.head in
    s with stack.mem  = v32.set h v s.stack.mem
      with stack.head = h + 1
      with pc         = (s.pc.0, s.pc.1 + 1)

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
    , pc        = (0,0)
    }

  def get    (s: state) (i:idx)   : u     = v8.get i s.mem
  def set    (s: state) (i:idx) v : state = s with mem = v8.set i v s.mem
                                              with pc  = (s.pc.0, s.pc.1 + 1)

  def return [n] : [n]state -> [n]u = map (\s' -> v8.get 0 s'.mem)


  def (+) (a: u) (b: u) = t.(a + b)
  def (*) (a: u) (b: u) = t.(a * b)
  def (/) (a: u) (b: u) = t.(a / b)
  def (-) (a: u) (b: u) = t.(a - b)
  def sqrt (a: u)       = t.sqrt a
  def to_i64 (a:u)      = t.to_i64 a
  def to_bits (a:u)     = t.to_bits a
  def from_bits (a:u64) = t.from_bits a

  def (==) (a: u) (b: u) = t.(a == b)
  def (<) (a: u) (b: u)  = t.(a < b)

  def jmp (s: state) (offset: u64) =
    s with pc = ( i32.u64 (offset >> u64.i32 u32.num_bits)
                -- TODO: Check if this is even remotely close to
                -- the correct answer
                , i32.u64 (offset & u64.u32 u32.highest))

  def eval [m] [n] [num_progs] (s: [m]state) (pidx: [m](i32,i32)) (p: [num_progs][n]instruction) =

    let step (s: state) : state =
      let fstval : u = get s ra in
      match p[s.pc.0][s.pc.1]
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
      case #jmpreg idx        -> jmp s (get s idx |> t.to_bits)
      case #jmplt  idx offset -> if (<) fstval (get s idx) then
                                   jmp s offset
                                 else
                                   s with pc.1 = i32.(s.pc.1 + 1)

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
    let should_sort_half [m] (pcs: [m](i64,state)) : bool =
      --reduce (\a c -> if i64.((==) c.0 0) then i64.((+) a 1) else a) 0 pcs
      --|> i64.((<) (m / n / 2))
      any ((.1) >-> (.pc) >-> (.0) >-> i32.((==) 0)) pcs

    -- Determine wether or not we should sort the indices
    let should_sort [m] (normalized_pcs: [m]i64) : bool =
      let npc' = rotate (-1) normalized_pcs in
      map2 (i64.-) normalized_pcs npc' |> i64.maximum
      -- MaximumProgramCounterDiff > (m / n)
      |> i64.((<) (m / n))


    let sort' [mm] 'a (xs: [mm](i64,state)) =
      let sortkey (s: state) : i64 = ((i64.i32 s.pc.0) << i64.i32 i32.num_bits)
                                    | (i64.i32 s.pc.1)
                                    in
      radix_sort_by_key ((.1) >-> sortkey) (i64.num_bits) (i64.get_bit) xs
    in

    let sort [mm] (idstates: [mm](i64,state)) : [mm](i64, state) =
      --let normalized_pcs : [mm]i64 =
      --  map (\(i,s) ->
      --    normalize_pc s.pc pidx[i]
      --  ) idstates
      --in
        if ! should_sort_half idstates then
          idstates
        else
          -- TODO: Sort only when entropy > threshold
          --zip normalized_pcs idstates -- [m](npc, (ids, states))
          --|>
          sort' idstates
    in

    let halted (instr: instruction) : bool =
      match instr
      case #halt -> true
      case _ -> false
    in

    let evaluate_4 ((i,s): (i64,state)) =
      (i,
      loop s = s
      for i < 4
      do step s
      )

    let evaluate_while ((i,s): (i64,state)) =
      (i,
      loop s = s
      while ! halted p[s.pc.0][s.pc.1]
      do step s
      )

    -- Returns a tuple of (still_running, finished) states
    let evaluate [k] (s: [k](i64,state)) : ([](i64, state), [](i64, state)) =
      let s' = map evaluate_4 s |> sort
      in partition (\(_,s'') -> ! halted p[s''.pc.0][s''.pc.1]) s'

    -- sort states by their original ID's s.t. they're in order again
    in
      -- States are paired with their original index
      let init_states =  map2 (\(s':state) pc -> s' with pc = pc) s pidx in

      (.1) <| loop (running, stopped, k) = (zip (iota m) <| init_states, copy init_states, m)
        while k > 0
        do
          let (running', stopped') = evaluate (take k running) in
          let (stopped_indices, stopped_states) = unzip stopped' in
          let stopped'' = scatter (copy stopped) stopped_indices stopped_states in

          let k' = length running' in
          let running'' = scatter (copy running) (iota k') (running' :> [k'](i64, state)) in
            (running'', stopped'', k')
}
