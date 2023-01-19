import "lib/github.com/athas/vector/vector"
open import "interpreter"


module interp_dynamic_memory (t: memtype) (P: {val numregs : i64}) : interpreter_simple
  with u = t.t
  with idx = i64
  = {

  type u = t.t

  let length = P.numregs
  -- First one is the program counter
  type state = [length]u

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

  def init v : state                = replicate length v
  def get  (s:state) (i:idx) : u    = s[i]
  def set  (s: state) (i:idx) v     = copy s with [i] = v
  def return [n] : [n]state -> [n]u = map (\s' -> s'[0])

  def (+) (a: u) (b: u) = t.(a + b)
  def (*) (a: u) (b: u) = t.(a * b)
  def (/) (a: u) (b: u) = t.(a / b)
  def (-) (a: u) (b: u) = t.(a - b)
  def sqrt (a: u)       = t.sqrt(a)

  def eval [m] [n] (s: [m]state) (pidx: [m]i64) (p: [n]instruction) =
    let step (pc: i64) (s: state) =
      let fstval : u = get s 0 in
      match p[pc]
      case #add index -> (i64.(pc + 1i64), (+) fstval (get s index) |> set s 0 )
      case #sub index -> (i64.(pc + 1i64), (-) fstval (get s index) |> set s 0 )
      case #mul index -> (i64.(pc + 1i64), (*) fstval (get s index) |> set s 0 )
      case #div index -> (i64.(pc + 1i64), (/) fstval (get s index) |> set s 0 )
      case #sqrt      -> (i64.(pc + 1i64), sqrt fstval              |> set s 0 )

      case #cnst v      -> (i64.(pc + 1i64), set s 0 v)
      case #store index -> (i64.(pc + 1i64), set s index fstval)
      case #load  index -> (i64.(pc + 1i64), get s index |> set s 0)

      case #halt -> (-1i64,s)

    let evaluate (pc: i64) (s: state) =
      loop (pc, s) = (pc, s)
      while pc != -1
      do step pc s

    in map2 evaluate pidx s
       |> unzip
       |> (.1)
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
  let re : idx = 4
  let rf : idx = 5
  let rg : idx = 6
  let rh : idx = 7

  type instruction = instruction_simple idx u

  def init v                    = v4.replicate v
  def get (s:state) (i:idx)     = v4.get i s
  def set s (i:idx) v           = v4.set i v s
  def return  [n] : [n]state -> [n]u = map (\s' -> get s' 0)

  def (+) (a: u) (b: u) = t.(a + b)
  def (*) (a: u) (b: u) = t.(a * b)
  def (/) (a: u) (b: u) = t.(a / b)
  def (-) (a: u) (b: u) = t.(a - b)
  def sqrt (a: u)       = t.sqrt(a)

  def eval [m] [n] (s: [m]state) (pidx: [m]i64) (p: [n]instruction) =
    let step (pc: i64) (s: state) =
      let fstval : u = get s 0 in
      match p[pc]
      case #add index -> (i64.(pc + 1i64), (+) fstval (get s index) |> set s 0 )
      case #sub index -> (i64.(pc + 1i64), (-) fstval (get s index) |> set s 0 )
      case #mul index -> (i64.(pc + 1i64), (*) fstval (get s index) |> set s 0 )
      case #div index -> (i64.(pc + 1i64), (/) fstval (get s index) |> set s 0 )
      case #sqrt      -> (i64.(pc + 1i64), sqrt fstval              |> set s 0 )

      case #cnst v      -> (i64.(pc + 1i64), set s 0 v)
      case #store index -> (i64.(pc + 1i64), set s index fstval)
      case #load  index -> (i64.(pc + 1i64), get s index |> set s 0)

      case #halt -> (-1i64,s)

    let evaluate (pc: i64) (s: state) =
      loop (pc, s) = (pc, s)
      while pc != -1
      do step pc s

    in map2 evaluate pidx s
       |> unzip
       |> (.1)
}


module interp_tuple_4_memory (t: memtype) : interpreter_simple
  with u = t.t
  = {

  type u = t.t

  type state = (u, u, u, u)

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

  def init v           = (v,v,v,v)
  def get  ((a,b,c,d):state) (r:idx) =
    match r
    case #ra -> a
    case #rb -> b
    case #rc -> c
    case #rd -> d
    case _   -> a
  def set  ((a,b,c,d):state) (r:idx) v =
    match r
    case #ra -> (v,b,c,d)
    case #rb -> (a,v,c,d)
    case #rc -> (a,b,v,d)
    case #rd -> (a,b,c,v)
    case _   -> (v,b,c,d)
  def return [n] : [n]state -> [n]u = map (.0)

  def (+) (a: u) (b: u) = t.(a + b)
  def (*) (a: u) (b: u) = t.(a * b)
  def (/) (a: u) (b: u) = t.(a / b)
  def (-) (a: u) (b: u) = t.(a - b)
  def sqrt (a: u)       = t.sqrt(a)

  def eval [m] [n] (s: [m]state) (pidx: [m]i64) (p: [n]instruction) =
    let step (pc: i64) (s: state) =
      let fstval : u = get s #ra in
      match p[pc]
      case #add index -> (i64.(pc + 1i64), (+) fstval (get s index) |> set s #ra )
      case #sub index -> (i64.(pc + 1i64), (-) fstval (get s index) |> set s #ra )
      case #mul index -> (i64.(pc + 1i64), (*) fstval (get s index) |> set s #ra )
      case #div index -> (i64.(pc + 1i64), (/) fstval (get s index) |> set s #ra )
      case #sqrt      -> (i64.(pc + 1i64), sqrt fstval              |> set s #ra )

      case #cnst v      -> (i64.(pc + 1i64), set s #ra v)
      case #store index -> (i64.(pc + 1i64), set s index fstval)
      case #load  index -> (i64.(pc + 1i64), get s index |> set s #ra)

      case #halt -> (-1i64,s)

    let evaluate (pc: i64) (s: state) =
      loop (pc, s) = (pc, s)
      while pc != -1
      do step pc s

    in map2 evaluate pidx s
       |> unzip
       |> (.1)
}
