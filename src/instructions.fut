-- | Instruction set definitions
--
-- These are definitions that can be implemented in each of the MIMD vm
-- interfaces defined in `interp.fut`.
-- They serve as a singular definition to make sure that the instructions are
-- consistent across implementations.

-- | Barebones instruction set. Supports only simple arithmetic.
type instruction_simple 'idx 'a =
    #add idx   -- Add value located at `idx` to default storage member
  | #sub idx   -- Subtract value ...
  | #mul idx   -- ...
  | #div idx   -- ...
  | #sqrt      -- sqrt(default register)
  | #cnst a    -- Load constant value to default storage member
  -- memory stuff
  | #store idx -- Move value located in default storage member to `idx`
  | #load idx  -- Load value located at `idx` to default storage member
  | #halt


-- | An extended version of `instruction_simple`, with branching. It includes
-- | seperate comparison instructions and a diverse set of conditional jumps.
type instruction_jump_long 'idx 'a =
  -- Common instructions with `instruction_simple`
  -- Adds default register and register at `idx`; stores result in default reg.
    #add  idx
  | #sub  idx
  | #mul  idx
  | #div  idx
  -- Adds default register and an immediate value; stores result in default reg.
  | #addi a
  | #subi a
  | #muli a
  | #divi a
  -- Yields square root of the value in the default reg.
  | #sqrt
  -- Load an immediate value in the default reg.
  | #cnst a

  -- Copy value in default reg. to register at `idx`
  | #store idx
  -- Copy value in register at `idx` to default reg.
  | #load idx
  -- Push value in register at `idx` to top of the stack
  | #push idx
  -- Pop value at top of the stack to default register
  | #pop

  | #cmp    idx -- Set ZF and CF flags in state
                -- ZF iff. r[#] = r[idx]
                -- CF iff. r[#] < r[idx]
  | #cmpi   a -- same thing, but against an immediate value

  | #jmp    i64 -- Add i64 to PC
  | #jmplt  i64 -- Add i64 to PC if condition is true
  | #jmpgt  i64 -- Add i64 to PC if condition is true
  | #jmpeq  i64 -- Add i64 to PC if condition is true
  | #jmpreg idx -- jump to offset in reg
  | #call   i64
  | #return
  | #halt


-- | Similar to `instruction_jump_long` but with inlined comparison in the
-- | conditional jump instruction. This instruction set is very restricted, and
-- | serves as a comparison to the more complex implementations.
type instruction_jump 'idx 'a =
    #add   idx
  | #mul   idx
  | #sqrt
  | #cnst  a

  | #store idx
  | #load  idx

  | #jmp   i64
  | #jmplt idx i64
  | #halt
