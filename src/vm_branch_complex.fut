open import "interpreter"


module interp_vector_4_branch_complex (t: memtype) : interpreter_branch_complex
  with u = t.t
  with idx = i64
  = {

  type u = t.t

  import "lib/github.com/athas/vector/vector"
  module v2 = cat_vector vector_1 vector_1
  module v4 = cat_vector v2 v2

  --            pc , ZF , CF , state
  type state = (i64,bool,bool, v4.vector u)

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

  def init v                                 : state = (0i64, false, false, replicate (v4.length) v |> v4.from_array)
  def get    ((_,_,_,s): state)    (i:idx)   : u     = v4.get i s
  def set    ((pc,zf,cf,s): state) (i:idx) v : state = (pc, zf,cf, v4.set i v s)
  def return ((_,_,_,s): state)              : u     = v4.get 0 s

  def (+) (a: u) (b: u) = t.(a + b)
  def (*) (a: u) (b: u) = t.(a * b)
  def (/) (a: u) (b: u) = t.(a / b)
  def (-) (a: u) (b: u) = t.(a - b)
  def sqrt (a: u)       = t.sqrt(a)

  def eval [n] (init_state: state) (p: [n]instruction) =
    let step (instr: instruction) ((pc,zf,cf,m): (i64,bool,bool, v4.vector u)) : state =
      -- Add 1 to PC
      let ss : (i64,bool,bool, v4.vector u) = ((+) 1i64 pc, zf, cf, m) :> (i64,bool,bool, v4.vector u) in
      let fstval : u = get m 0 in
      match instr
      case #add index   -> (+) fstval (get m index) |> set ss 0
      case #sub index   -> (-) fstval (get m index) |> set ss 0
      case #mul index   -> (*) fstval (get m index) |> set ss 0
      case #div index   -> (/) fstval (get m index) |> set ss 0
      case #sqrt        -> (u.sqrt) fstval          |> set ss 0

      case #cnst v      -> set s 0 v

      case #store index -> set s index fstval
      case #load  index -> get s index |> set s 0

      -- Jump around!
      case #cmp   index  -> let arg = (get s index)
                            in ( pc + 1i64
                               , fstval - arg == 0
                               , fstval - arg < 0
                               , s)
      case #jmp   offset -> (pc + offset, zf, cf, s)
      case #jmplt offset -> (pc + if cf       then offset else 1, zf, cf, s)
      case #jmpgt offset -> (pc + if cf && zf then offset else 1, zf, cf, s)
      case #jmpeq offset -> (pc + if zf       then offset else 1, zf, cf, s)
      case #halt         -> (pc,zf,cf,s)

    in loop (pc,zf,cf,mem) = init_state
       while pc < n && p[i] != #halt
       do step p[i] (pc,zf,cf,mem)
}
