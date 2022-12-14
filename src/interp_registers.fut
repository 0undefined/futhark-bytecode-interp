import "lib/github.com/athas/vector/vector"
open import "interp"


module interp_dynamic_memory (t: memtype) (P: {val numregs : i64}) : interpreter_simple
  with u = t.t
  with idx = i64
  = {

  type u = t.t

  let length = P.numregs
  type state = [length]u

  type idx = i64
  let ra : idx = 0
  let rb : idx = 1
  let rc : idx = 2
  let rd : idx = 3

  type instruction = #add idx  | #sub idx   | #mul idx | #div idx
                   | #load idx | #store idx | #cnst u

  -- def stdreplicate = replicate
  def init v           = replicate length v
  def get  s (i:idx)   = s[i]
  def set  s (i:idx) v = (copy s) with [i] = v
  def return  s        = get s 0

  def (+) (a: u) (b: u) = t.(a + b)
  def (*) (a: u) (b: u) = t.(a * b)
  def (/) (a: u) (b: u) = t.(a / b)
  def (-) (a: u) (b: u) = t.(a - b)

  def eval [n] (s: state) (p: [n]instruction) =
    let step (instr: instruction) (s: state) =
      let fstval : u = get s 0 in
      match instr
      case #add index -> (+) fstval (get s index) |> set s 0
      case #sub index -> (-) fstval (get s index) |> set s 0
      case #mul index -> (*) fstval (get s index) |> set s 0
      case #div index -> (/) fstval (get s index) |> set s 0
      case #store index -> set s index fstval
      case #load  index -> get s index |> set s 0
      case #cnst v -> set s 0 v
    in loop s for i < n do step p[i] s
}


module interp_vector_4_memory (t: memtype) : interpreter_simple
  with u = t.t
  with idx = i64
  = {

  type u = t.t

  module v2 = cat_vector vector_1 vector_1
  module v4 = cat_vector v2 v2

  type state = v4.vector u

  type idx = i64
  let ra : idx = 0
  let rb : idx = 1
  let rc : idx = 2
  let rd : idx = 3

  type instruction = #add idx  | #sub idx   | #mul idx | #div idx
                   | #load idx | #store idx | #cnst u

  -- def stdreplicate = replicate
  def init v           = replicate (v4.length) v |> v4.from_array
  def get  s (i:idx)   = v4.get i s
  def set  s (i:idx) v = v4.set i v s
  def return  s        = get s 0

  def (+) (a: u) (b: u) = t.(a + b)
  def (*) (a: u) (b: u) = t.(a * b)
  def (/) (a: u) (b: u) = t.(a / b)
  def (-) (a: u) (b: u) = t.(a - b)

  def eval [n] (s: state) (p: [n]instruction) =
    let step (instr: instruction) (s: state) =
      let fstval : u = get s 0 in
      match instr
      case #add index -> (+) fstval (get s index) |> set s 0
      case #sub index -> (-) fstval (get s index) |> set s 0
      case #mul index -> (*) fstval (get s index) |> set s 0
      case #div index -> (/) fstval (get s index) |> set s 0
      case #store index -> set s index fstval
      case #load  index -> get s index |> set s 0
      case #cnst v -> set s 0 v
    in loop s for i < n do step p[i] s
}


module interp_tuple_4_memory (t: memtype) : interpreter_simple
  with u = t.t
  = {

  type u = t.t

  let length = 4i64
  type state = (u, u, u, u)

  type idx = (#ra | #rb | #rc | #rd)
  let ra : idx = #ra
  let rb : idx = #rb
  let rc : idx = #rc
  let rd : idx = #rd

  type instruction = #add idx  | #sub idx   | #mul idx | #div idx
                   | #load idx | #store idx | #cnst u

  -- def stdreplicate = replicate
  def init v           = (v,v,v,v)
  def get  ((a,b,c,d):state) (r:idx) =
    match r
    case #ra -> a
    case #rb -> b
    case #rc -> c
    case #rd -> d
  def set  ((a,b,c,d):state) (r:idx) v =
    match r
    case #ra -> (v,b,c,d)
    case #rb -> (a,v,c,d)
    case #rc -> (a,b,v,d)
    case #rd -> (a,b,c,v)
  def return ((a,_,_,_):state) = a

  def (+) (a: u) (b: u) = t.(a + b)
  def (*) (a: u) (b: u) = t.(a * b)
  def (/) (a: u) (b: u) = t.(a / b)
  def (-) (a: u) (b: u) = t.(a - b)

  def eval [n] (s: state) (p: [n]instruction) =
    let step (instr: instruction) (s: state) =
      let fstval : u = get s #ra in
      match instr
      case #add index -> (+) fstval (get s index) |> set s #ra
      case #sub index -> (-) fstval (get s index) |> set s #ra
      case #mul index -> (*) fstval (get s index) |> set s #ra
      case #div index -> (/) fstval (get s index) |> set s #ra
      case #store index -> set s index fstval
      case #load  index -> get s index |> set s #ra
      case #cnst v -> set s #ra v
    in loop s for i < n do step p[i] s
}
