import "interp"

module InterpRegisterListF64 (t: memtype) (P: {val numregs : i64}) : Interp
  with u = t.t
  with idx = i64
  = {

  type u = t.t

  let length = P.numregs
  type State = [length]u

  type idx   = i64

  type instruction = #add idx | #sub idx
                   | #mul idx | #div idx
                   | #mov idx
                   | #cnst u

  -- def stdreplicate = replicate
  def init (v: u) : State = replicate length v
  def get  (s: State) (i: idx)   : u = s[i]
  def set  (s: State) (i: idx) v : State = (copy s) with [i] = v

  def (+) (a: u) (b: u) = t.(a + b)
  def (*) (a: u) (b: u) = t.(a * b)
  def (/) (a: u) (b: u) = t.(a / b)
  def (-) (a: u) (b: u) = t.(a - b)

  def eval [n] (s: State) (p: [n]instruction) =
    let step (instr: instruction) (s: State) =
      let fstval : u = get s 0 in
      match instr
      case #add index -> (+) fstval (get s index) |> set s 0
      case #sub index -> (-) fstval (get s index) |> set s 0
      case #mul index -> (*) fstval (get s index) |> set s 0
      case #div index -> (/) fstval (get s index) |> set s 0
      case #mov index -> set s index fstval
      case #cnst v -> set s 0 v
    in loop s for i < n do step p[i] s
}
