.globl _start
.text
_start:
JAL main
XOR $a0, $v0, $0
XORI $v0, $0, 17
syscall
readIntegerFunction:
ADDI $sp, $sp, -8
SW $ra, 4($sp)
SW $fp, 0($sp)
XOR $fp, $sp, $0
XORI $v0, $0, 5
syscall
LW $fp, 0($sp)
LW $ra, 4($sp)
ADDI $sp, $sp, 8
JR $ra
writeIntegerFunction:
ADDI $sp, $sp, -8
SW $ra, 4($sp)
SW $fp, 0($sp)
XOR $fp, $sp, $0
XORI $v0, $0, 1
syscall
LW $fp, 0($sp)
LW $ra, 4($sp)
ADDI $sp, $sp, 8
JR $ra
ret_fun0_2:
ADDI $sp, $sp, -8
SW $ra, 4($sp)
SW $fp, 0($sp)
XOR $fp, $sp, $0
ADDI $sp, $sp, -0
ADDI $sp, $sp, 0
XOR $8, $a0, $0
XOR $11, $8, $0
XORI $10, $0, 0
XORI $9, $0, 0
BNE $11, $10, neq_11
J eq_10
eq_10:
XORI $9, $0, 1
neq_11:
XORI $10, $0, 1
BNE $9, $10, if_false6
J if_true5
if_true5:
XORI $8, $0, 0
J if_end7
if_false6:
XOR $11, $8, $0
XORI $10, $0, 1
XORI $9, $0, 0
BNE $11, $10, neq_19
J eq_18
eq_18:
XORI $9, $0, 1
neq_19:
XORI $10, $0, 1
BNE $9, $10, if_false14
J if_true13
if_true13:
XORI $8, $0, 1
J if_end15
if_false14:
XOR $10, $8, $0
XORI $9, $0, 1
SUB $9, $10, $9
XOR $a0, $9, $0
ADDI $sp, $sp, -8
SW $8, 4($sp)
SW $ra, 0($sp)
JAL fun0
LW $8, 4($sp)
LW $ra, 0($sp)
ADDI $sp, $sp, 8
XOR $9, $v0, $0
XOR $10, $8, $0
XORI $8, $0, 2
SUB $8, $10, $8
XOR $a0, $8, $0
ADDI $sp, $sp, -8
SW $9, 4($sp)
SW $ra, 0($sp)
JAL fun0
LW $9, 4($sp)
LW $ra, 0($sp)
ADDI $sp, $sp, 8
XOR $8, $v0, $0
ADD $8, $9, $8
if_end15:
if_end7:
XOR $v0, $8, $0
ADDI $sp, $sp, 0
ADDI $sp, $sp, 0
LW $fp, 0($sp)
LW $ra, 4($sp)
ADDI $sp, $sp, 8
JR $ra
ret_fun1_28:
ADDI $sp, $sp, -8
SW $ra, 4($sp)
SW $fp, 0($sp)
XOR $fp, $sp, $0
ADDI $sp, $sp, -0
ADDI $sp, $sp, 0
ADDI $sp, $sp, -4
SW $ra, 0($sp)
JAL readIntegerFunction
LW $ra, 0($sp)
ADDI $sp, $sp, 4
XOR $10, $v0, $0
XOR $8, $10, $0
XOR $a0, $8, $0
ADDI $sp, $sp, -4
SW $ra, 0($sp)
JAL fun0
LW $ra, 0($sp)
ADDI $sp, $sp, 4
XOR $11, $v0, $0
XOR $9, $11, $0
XOR $a0, $9, $0
ADDI $sp, $sp, -4
SW $ra, 0($sp)
JAL writeIntegerFunction
LW $ra, 0($sp)
ADDI $sp, $sp, 4
XOR $12, $v0, $0
XOR $v0, $12, $0
ADDI $sp, $sp, 0
ADDI $sp, $sp, 0
LW $fp, 0($sp)
LW $ra, 4($sp)
ADDI $sp, $sp, 8
JR $ra