module MipsIO where

import Mips

-- This should be written as a list of Mips functions. Not as a string.
readIntegerFunction :: [] Mips.Instruction
readIntegerFunction = [Mips.Label "readIntegerFunction",
                       Mips.ADDI "$sp" "$sp" "-4",
                       Mips.SW "$fp" "$sp" "0",
                       Mips.XOR "$fp" "$sp" "$0",
                       Mips.XORI "$v0" "$0" "5",
                       Mips.Syscall,
                       Mips.LW "$fp" "$sp" "0",
                       Mips.ADDI "$sp" "$sp" "4",
                       Mips.JR "$ra"]

writeIntegerFunction :: [] Mips.Instruction
writeIntegerFunction = [Mips.Label "writeIntegerFunction",
                       Mips.ADDI "$sp" "$sp" "-4",
                       Mips.SW "$fp" "$sp" "0",
                       Mips.XOR "$fp" "$sp" "$0",
                       Mips.XORI "$v0" "$0" "1",
                       Mips.Syscall,
                       Mips.LW "$fp" "$sp" "0",
                       Mips.ADDI "$sp" "$sp" "4",
                       Mips.JR "$ra"]
  
