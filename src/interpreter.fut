open import "instructions"

-- | Virtual Machine State type
local module type state = {
  -- | The type of data the vm works on, f32, i64 etc.
  type u
  -- | State of the VM, typically a [n]u or n-tuple of `u`
  type state
  -- | The datatype used to identify data in state, typically i64 or a sumtype
  type idx
  val ra : idx
  val rb : idx
  val rc : idx
  val rd : idx
  val re : idx
  val rf : idx
  val rg : idx
  val rh : idx

  -- | Initialize the state with singular value
  val init : u -> state

  -- | Get `idx` in state
  val get : state -> idx -> u

  -- | Set value at `idx` in state
  val set : state -> idx -> u -> state

  -- | Return some value from state
  val return [n] : [n]state -> [n]u
}


-- | Common Interpreter Interface
module type interpreter = {
  include state

  -- | Instructions that need matching in eval
  --   Typically a sumtype with various payloads
  type instruction

  -- | Evaluate a flat segmented list of programs given some initial states
  -- returning the evaluated list of states.
  -- m is the number of programs in the list of instructions
  val eval [m] [n] : [m]state -> [m]i64 -> [n]instruction -> [m]state
}


-- | Simple interpreter interface
module type interpreter_simple = {
  type idx
  type u
  include interpreter
    with idx=idx
    with u=u
    with instruction = instruction_simple idx u
}


-- | Branching interpreter interface
module type interpreter_branch_complex = {
  type idx
  type u
  include interpreter
    with idx=idx
    with u=u
    with instruction = instruction_jump_long idx u
}


-- | Reduced branching interpreter interface
module type interpreter_branch = {
  type idx
  type u
  include interpreter
    with idx=idx
    with u=u
    with instruction = instruction_jump idx u
}


-- | Generic type wrapper,
--   inspired by the scalar module in the vector library at
--   github.com/athas/vector/blob/master/lib/github.com/athas/vector/vspace.fut
module type memtype = {
  type t

  val +: t -> t -> t
  val -: t -> t -> t
  val *: t -> t -> t
  val /: t -> t -> t
  val ==: t -> t -> bool
  val <: t -> t -> bool

  val sqrt : t -> t

  val i32: i32 -> t
  val i64: i64 -> t
  val f32: f32 -> t
  val f64: f64 -> t
}
