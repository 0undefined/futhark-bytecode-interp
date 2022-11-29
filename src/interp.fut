module type Interp = {
  -- | The type of data the vm works on, f32, i64 etc.
  type u
  -- | Typically a [n]dtype or n-tuple
  type State
  -- | The datatype used to identify data in state, typically i64 or a sumtype
  type idx

  type instruction = #add idx | #sub idx
                   | #mul idx | #div idx
                   | #mov idx
                   | #cnst u

  val init : u -> State
  val get  : State -> idx -> u
  val set  : State -> idx -> u -> State

  val eval [n] : State -> [n]instruction -> State
}

module type memtype = {
  type t

  val +: t -> t -> t
  val -: t -> t -> t
  val *: t -> t -> t
  val /: t -> t -> t

  val i32: i32 -> t
}
