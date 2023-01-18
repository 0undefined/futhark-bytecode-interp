import "lib/github.com/athas/vector/vector"
open import "interpreter"


module interp_dynamic_memory (t: memtype) (P: {val numregs : i64}) : interpreter_simple
  with u = t.t
  with idx = i64
  = {

  type u = t.t

  let length = P.numregs
  -- First one is the program counter
  type state = (i64, [length]u)

  type idx = i64
  let ra : idx = 0
  let rb : idx = 1
  let rc : idx = 2
  let rd : idx = 3
  let re : idx = 4
  let rf : idx = 5
  let rg : idx = 6
  let rh : idx = 7

  type instruction = instruction_simple idx u

  def init v : state                 = (0, replicate length v)
  def get  (s:state) (i:idx) : u     = s.1[i]
  def set  ((p,s) : state) (i:idx) v = (p + 1i64, copy s with [i] = v)
  def return [n] : [n]state -> [n]u  = map (\s' -> s'.1[0])

  def (+) (a: u) (b: u) = t.(a + b)
  def (*) (a: u) (b: u) = t.(a * b)
  def (/) (a: u) (b: u) = t.(a / b)
  def (-) (a: u) (b: u) = t.(a - b)
  def sqrt (a: u)       = t.sqrt(a)

  def eval [m] [n] (s: [m]state) (pidx: [m]i64) (p: [n]instruction) : [m]state =
    let step (i: i64) (s:state) =
      --let s' = s[i]
      let i' = i64.(i + s.0) in
      let fstval : u = get s 0 in
      match p[i']
      case #add index -> (+) fstval (get s index) |> set s 0
      case #sub index -> (-) fstval (get s index) |> set s 0
      case #mul index -> (*) fstval (get s index) |> set s 0
      case #div index -> (/) fstval (get s index) |> set s 0
      case #sqrt      -> sqrt fstval              |> set s 0
      case #store index -> set s index fstval
      case #load  index -> get s index |> set s 0
      case #cnst v -> set s 0 v
      case #halt   -> s

    in loop s while any (\i ->
      -- TODO: Create bug report, cant do p[..] != #halt
      match p[ i64.(pidx[i] + s[i].0) ]
      case #halt -> false
      case _     -> true
      ) (iota m)
    do map2 step pidx s
}


module interp_vector_4_memory (t: memtype) : interpreter_simple
  with u = t.t
  with idx = i64
  = {

  type u = t.t

  module v2 = cat_vector vector_1 vector_1
  module v4 = cat_vector v2 v2

  type state = (i64, v4.vector u)

  type idx = i64
  let ra : idx = 0
  let rb : idx = 1
  let rc : idx = 2
  let rd : idx = 3
  let re : idx = 4
  let rf : idx = 5
  let rg : idx = 6
  let rh : idx = 7

  type instruction = instruction_simple idx u

  def init v                         = (0i64,v4.replicate v)
  def get (s:state) (i:idx)          = v4.get i s.1
  def set (pc,s) (i:idx) v           = (pc + 1i64, v4.set i v s)
  def return  [n] : [n]state -> [n]u = map (\s' -> get s' 0)

  def (+) (a: u) (b: u) = t.(a + b)
  def (*) (a: u) (b: u) = t.(a * b)
  def (/) (a: u) (b: u) = t.(a / b)
  def (-) (a: u) (b: u) = t.(a - b)
  def sqrt (a: u)       = t.sqrt(a)

  def eval [m] [n] (s: [m]state) (pidx: [m]i64) (p: [n]instruction) =
    let step (i: i64) (s:state) =
      let i' = i64.(i + s.0) in
      let fstval : u = get s 0 in
      match p[i']
      case #add index -> (+) fstval (get s index) |> set s 0
      case #sub index -> (-) fstval (get s index) |> set s 0
      case #mul index -> (*) fstval (get s index) |> set s 0
      case #div index -> (/) fstval (get s index) |> set s 0
      case #sqrt      -> sqrt fstval              |> set s 0

      case #cnst v    -> set s 0 v
      case #store index -> set s index fstval
      case #load  index -> get s index |> set s 0

      case #halt -> s

    in loop s while any (\i ->
      -- TODO: Create bug report, cant do p[..] != #halt
      match p[ i64.(pidx[i] + s[i].0) ]
      case #halt -> false
      case _     -> true
      ) (iota m)
    do map2 step pidx s
}


module interp_tuple_4_memory (t: memtype) : interpreter_simple
  with u = t.t
  = {

  type u = t.t

  let length = 4i64
  type state = (i64, u, u, u, u)

  type idx = #ra | #rb | #rc | #rd | #re | #rf | #rg | #rh
  let ra : idx = #ra
  let rb : idx = #rb
  let rc : idx = #rc
  let rd : idx = #rd
  let re : idx = #re
  let rf : idx = #rf
  let rg : idx = #rg
  let rh : idx = #rh

  type instruction = instruction_simple idx u

  def init v           = (0i64,v,v,v,v)
  def get  ((_,a,b,c,d):state) (r:idx) =
    match r
    case #ra -> a
    case #rb -> b
    case #rc -> c
    case #rd -> d
    case _   -> a
  def set  ((pc,a,b,c,d):state) (r:idx) v =
    match r
    case #ra -> (pc + 1, v,b,c,d)
    case #rb -> (pc + 1, a,v,c,d)
    case #rc -> (pc + 1, a,b,v,d)
    case #rd -> (pc + 1, a,b,c,v)
    case _   -> (pc + 1, v,b,c,d)
  def return [n] : [n]state -> [n]u = map (.1)

  def (+) (a: u) (b: u) = t.(a + b)
  def (*) (a: u) (b: u) = t.(a * b)
  def (/) (a: u) (b: u) = t.(a / b)
  def (-) (a: u) (b: u) = t.(a - b)
  def sqrt (a: u)       = t.sqrt(a)

  def eval [m] [n] (s: [m]state) (pidx: [m]i64) (p: [n]instruction) =
    let step (i: i64) (s:state) =
      let i' = i64.(i + s.0) in
      let fstval : u = get s #ra in
      match p[i']
      case #add index -> (+) fstval (get s index) |> set s #ra
      case #sub index -> (-) fstval (get s index) |> set s #ra
      case #mul index -> (*) fstval (get s index) |> set s #ra
      case #div index -> (/) fstval (get s index) |> set s #ra
      case #sqrt      -> sqrt fstval              |> set s #ra

      case #cnst v    -> set s #ra v
      case #store index -> set s index fstval
      case #load  index -> get s index |> set s #ra

      case #halt -> s

    in loop s while any (\i ->
      -- TODO: Create bug report, cant do p[..] != #halt
      match p[ i64.(pidx[i] + s[i].0) ]
      case #halt -> false
      case _     -> true
      ) (iota m)
    do map2 step pidx s
}
