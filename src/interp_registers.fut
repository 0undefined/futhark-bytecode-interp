import "interp"

module InterpRegisterListF64 (P: {val numregs : i64}) : Interp = {

  type dtype = f64

  let length = P.numregs
  type State = [length]dtype
  type idx = i64

  type instruction = #add idx | #sub idx
                   | #mul idx | #div idx
                   | #cnst dtype

  -- def stdreplicate = replicate
  def init v = replicate length v
  def get  s (i: idx) = s[i]
  def set  s (i: idx) v = (copy s) with [i] = v

  def eval [n] (s: State) (p: [n]instruction) =
    let step (instr: instruction) (s: State) =
      match instr
      case #add index -> let lhs = get s 0 in get s index |> (+) lhs |> set s 0
      case #sub index -> let lhs = get s 0 in get s index |> (+) lhs |> set s 0
      case #mul index -> let lhs = get s 0 in get s index |> (+) lhs |> set s 0
      case #div index -> let lhs = get s 0 in get s index |> (+) lhs |> set s 0
      case #cnst v -> set s 0 v
    in loop s for i < n do step p[i] s
}
