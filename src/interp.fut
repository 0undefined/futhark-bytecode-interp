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

  -- | Initialize the state with singular value
  val init : u -> state
  -- | Get `idx` in state
  val get  : state -> idx -> u
  -- | Set value at `idx` in state
  val set  : state -> idx -> u -> state
  val return  : state -> u
}


-- | Common Interpreter Interface
module type interpreter = {
  include state

  -- | Instructions that need matching in eval
  --   Typically a sumtype with various payloads
  type instruction

  -- | Evaluate the program, returning a new state
  val eval [n] : state -> [n]instruction -> state
}


-- | Simple interpreter interface
module type interpreter_simple  = {
  type idx
  type u
  -- | Define the instructionset for the simple interpreter
  include interpreter
    with idx=idx
    with u=u
    with instruction =
             #add idx -- Add value located at `idx` to default storage member
           | #sub idx -- Subtract value ...
           | #mul idx -- ...
           | #div idx -- ...
           | #store idx -- Move value located in default storage member to `idx`
           | #load idx  -- Load value located at `idx` to default storage member
           | #cnst u    -- Load constant value to default storage member
}


-- | Simple interpreter interface
module type interpreter_branch = {
  type idx
  type u
  -- | Define the instructionset for the simple interpreter
  include interpreter
    with idx=idx
    with u=u
    with instruction =
             #add idx -- Add value located at `idx` to default storage member
           | #sub idx -- Subtract value ...
           | #mul idx -- ...
           | #div idx -- ...
           | #store idx -- Move value located in default storage member to `idx`
           | #load idx  -- Load value located at `idx` to default storage member
           | #cnst u    -- Load constant value to default storage member

           | #cmp   idx -- Set ZF and CF flags in state
                        -- ZF iff. r[#] = r[idx]
                        -- CF iff. r[#] < r[idx]
           | #jmp   i64 -- Add i64 to PC
           | #jmplt i64 -- Add i64 to PC if condition is true
           | #jmpgt i64 -- Add i64 to PC if condition is true
           | #jmpeq i64 -- Add i64 to PC if condition is true
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

  val i32: i32 -> t
  val i64: i64 -> t
  val f32: f32 -> t
  val f64: f64 -> t
}
