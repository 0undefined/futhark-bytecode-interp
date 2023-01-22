open import "interpreter"


module interp_vector_8_branch_complex (t: memtype) : interpreter_branch_complex
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
    , head: i64            -- keeps track of the current frame in the callstack
    }

  type state =
    { mem: v8.vector u
    , pc: i64
    , stack: stack
    , zf: bool                     -- zero flag
    , cf: bool                     -- carry flag
    }

  def stack_init (v: u)       : stack = {mem = v32.replicate v, head = 0i64}
  def stack_peek (s: state)   : u     = v32.get s.stack.head s.stack.mem

  def stack_push (s: state) (v: u) : state =
    let h = assert (s.stack.head < 32) s.stack.head in
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

  type instruction = instruction_jump_long idx u

  def init v : state =
    { mem       = v8.replicate v
    , stack     = stack_init v
    , pc        = 0
    , zf        = false
    , cf        = false
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
  --def from_i64 (a:i64)  = t.from_i64 a
  --def i64 (a:i64)     = t.i64 a

  def (==) (a: u) (b: u) = t.(a == b)
  def (<) (a: u) (b: u)  = t.(a < b)

  def reset_flags (s: state) : state =
    s with zf = false with cf = false with pc = i64.(s.pc + 1)

  def jmp (s: state) (offset: i64) = reset_flags s with pc = offset

  def call (s: state) (offset: i64) =
    let s' = stack_push s (i64.(s.pc + 1i64) |> t.i64)
    in jmp s' offset

  def ret  (s: state) =
    let (s', pc) = stack_pop s
    in jmp s' (t.to_i64 pc)

  def eval [m] [n] (s: [m]state) (pidx: [m]i64) (p: [n]instruction) =

    let step (s: state) : state =
      let fstval : u = get s ra in
      match p[s.pc]
      case #add index   -> (+) fstval (get s index) |> set s ra
      case #sub index   -> (-) fstval (get s index) |> set s ra
      case #mul index   -> (*) fstval (get s index) |> set s ra
      case #div index   -> (/) fstval (get s index) |> set s ra
      case #sqrt        -> sqrt fstval              |> set s ra

      case #cnst v      -> set s ra v

      case #store index -> set s index fstval
      case #load  index -> get s index |> set s ra
      case #push  index -> stack_push s (get s index)
      case #pop         -> let (s', v) = stack_pop s in set s' ra v

      -- Jump around!
      case #cmp   index  -> let arg = get s index in
                            s with zf = (==) fstval arg
                              with cf = (<)  fstval arg
                              with pc = i64.(s.pc + 1)

      case #jmp   offset -> jmp s offset
      case #jmpreg index -> jmp s (get s index |> t.to_i64)
      case #jmplt offset -> if s.cf            then jmp s offset else reset_flags s
      case #jmpgt offset -> if !(s.cf || s.zf) then jmp s offset else reset_flags s
      case #jmpeq offset -> if s.zf            then jmp s offset else reset_flags s

      case #call offset -> call s offset
      case #return      -> ret s

      case #halt         -> s with pc = -1

    let evaluate (s: state) =
      loop s = s
      while s.pc != -1
      do step s

    in map evaluate (map2 (\(s':state) pc -> s' with pc = pc) s pidx)
}
