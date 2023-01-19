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

  type frame =
    { callstack: v4.vector i64 -- callstack[head] is the current PC
    , head: i64                -- keeps track of the current frame in the callstack
    }
  type state =
    { mem: v8.vector u
    , zf: bool                     -- zero flag
    , cf: bool                     -- carry flag
    }

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
    , zf        = false
    , cf        = false
    }

  def get    (s: state) (i:idx)   : u     = v8.get i s.mem
  def set    (s: state) (i:idx) v : state = s with mem = v8.set i v s.mem

  def return [n] : [n]state -> [n]u     = map (\s' -> v8.get 0 s'.mem)

  def (+) (a: u) (b: u) = t.(a + b)
  def (*) (a: u) (b: u) = t.(a * b)
  def (/) (a: u) (b: u) = t.(a / b)
  def (-) (a: u) (b: u) = t.(a - b)
  def sqrt (a: u)       = t.sqrt(a)

  def (==) (a: u) (b: u) = t.(a == b)
  def (<) (a: u) (b: u) = t.(a < b)

  def pc_init (start: i64) : frame =
    {callstack = v4.set 0 start (v4.replicate 0), head = 0}

  def pc_get (cs: frame) : i64 =
    v4.get cs.head cs.callstack

  def pc_jmp (cs: frame) (offset: i64) : frame =
    if cs.head >= 3 then ??? else
      let offset' = i64.(pc_get cs + offset)
      in cs with callstack = v4.set cs.head (pc_get cs |> i64.((+) offset)) cs.callstack
            with head = i64.(cs.head + 1)
            with callstack = v4.set cs.head offset' cs.callstack

  def reset_flags (s: state) : state =
    s with zf = false with cf = false

  def jmp (cs: frame) (offset: i64) (s: state) : (frame, state) =
    (pc_jmp cs offset, reset_flags s)

  def pc_inc (cs: frame) : frame =
    cs with callstack = v4.set cs.head (pc_get cs |> i64.((+) 1)) cs.callstack

  def pc_nil (cs: frame) : frame =
    cs with callstack = v4.set cs.head (-1i64) cs.callstack

  def eval [m] [n] (s: [m]state) (pidx: [m]i64) (p: [n]instruction) =

    let step (cs: frame) (s: state) : (frame, state) =
      let fstval : u = get s ra in
      match p[pc_get cs]
      case #add index   -> (pc_inc cs, (+) fstval (get s index) |> set s ra)
      case #sub index   -> (pc_inc cs, (-) fstval (get s index) |> set s ra)
      case #mul index   -> (pc_inc cs, (*) fstval (get s index) |> set s ra)
      case #div index   -> (pc_inc cs, (/) fstval (get s index) |> set s ra)
      case #sqrt        -> (pc_inc cs, sqrt fstval              |> set s ra)

      case #cnst v      -> (pc_inc cs, set s ra v)

      case #store index -> (pc_inc cs, set s index fstval)
      case #load  index -> (pc_inc cs, get s index |> set s ra)

      -- Jump around!
      case #cmp   index  -> let arg = (get s index) in
                            (pc_inc cs, { mem = s.mem
                                        , zf = (==)  fstval arg
                                        , cf = (<)   fstval arg
                                        })

      case #jmp   offset -> (pc_jmp cs offset, s)
      case #jmplt offset -> if s.cf         then jmp cs offset s else (pc_inc cs, reset_flags s)
      case #jmpgt offset -> if s.cf && s.zf then jmp cs offset s else (pc_inc cs, reset_flags s)
      case #jmpeq offset -> if s.zf         then jmp cs offset s else (pc_inc cs, reset_flags s)

      case #halt         -> (pc_nil cs, s)

    let evaluate (cs: frame) (s: state) =
      loop (cs, s) = (cs, s)
      while pc_get cs != -1
      do step cs s

    in map2 evaluate (map pc_init pidx) s
       |> unzip
       |> (.1)
}
