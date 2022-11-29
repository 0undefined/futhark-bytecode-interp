module type Interp = {
  type dtype       -- | The type of data the vm works on, f32, i64 etc.
  type State       -- | Typically a [n]dtype or n-tuple
  type idx         -- | The datatype used to identify data in state, typically
                   --   i64 or some #regA | #regB | ...

  type instruction -- = #add idx | #sub idx
                   -- | #mul idx | #div idx
                   -- | #cnst dtype


  val init : dtype -> State
  val get  : State -> idx -> dtype
  val set  : State -> idx -> dtype -> State

  val eval [n] : State -> [n]instruction -> State
}
