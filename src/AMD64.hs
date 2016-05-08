module AMD64 where

type Reg  = String
type Imm  = String
type Addr = String -- denoted by 

data Instruction = Label String 
                 | Comment String
                 | MOV String String
                 | CMP String String
                 | JMP String -- takes label as arg
                 | JNE String
                 | JZ String -- takes label as arg
                 | JE String -- Equal to JZ
                 | INC String -- increase target reg (or addr.? by 1)
                 | DEC String
                 | ADD String String
                 | SUB String String
                 | IMUL String -- RDX:RAX <- RAX * arg
                 | MUL String -- 
                 | IDIV String -- divides cont of RDX:RAX by arg. Result in RAX, rem. in RDX
                 | DIV String -- as above but unsigned
                 | INT String -- takes a HEX number as arg.
                 | PUSH Reg
                 | POP Reg
                 | RET -- returns from function
                 | CALL String -- call a function
