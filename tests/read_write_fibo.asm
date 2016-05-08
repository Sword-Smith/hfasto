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
fibo:
ADDI $sp, $sp, -8
SW $ra, 4($sp)
SW $fp, 0($sp)
XOR $fp, $sp, $0
ADDI $sp, $sp, -0
ADDI $sp, $sp, 0
XOR $8, $a0, $0
XOR $12, $8, $0
XORI $11, $0, 0
XORI $10, $0, 0
BNE $12, $11, neq_11
J eq_10
eq_10:
XORI $10, $0, 1
neq_11:
XORI $11, $0, 1
BNE $10, $11, if_false6
J if_true5
if_true5:
XORI $8, $0, 1
J if_end7
if_false6:
XOR $12, $8, $0
XORI $11, $0, 1
XORI $10, $0, 0
BNE $12, $11, neq_19
J eq_18
eq_18:
XORI $10, $0, 1
neq_19:
XORI $11, $0, 1
BNE $10, $11, if_false14
J if_true13
if_true13:
XORI $8, $0, 1
J if_end15
if_false14:
XOR $11, $8, $0
XORI $10, $0, 1
SUB $10, $11, $10
XOR $a0, $10, $0
ADDI $sp, $sp, -12
SW $9, 8($sp)
SW $8, 4($sp)
SW $ra, 0($sp)
JAL fibo
LW $9, 8($sp)
LW $8, 4($sp)
LW $ra, 0($sp)
ADDI $sp, $sp, 12
XOR $10, $v0, $0
XOR $11, $8, $0
XORI $8, $0, 2
SUB $8, $11, $8
XOR $a0, $8, $0
ADDI $sp, $sp, -12
SW $10, 8($sp)
SW $9, 4($sp)
SW $ra, 0($sp)
JAL fibo
LW $10, 8($sp)
LW $9, 4($sp)
LW $ra, 0($sp)
ADDI $sp, $sp, 12
XOR $8, $v0, $0
ADD $8, $10, $8
if_end15:
if_end7:
XOR $v0, $9, $0
ADDI $sp, $sp, 0
ADDI $sp, $sp, 0
LW $fp, 0($sp)
LW $ra, 4($sp)
ADDI $sp, $sp, 8
JR $ra
main:
ADDI $sp, $sp, -8
SW $ra, 4($sp)
SW $fp, 0($sp)
XOR $fp, $sp, $0
ADDI $sp, $sp, -0
ADDI $sp, $sp, 0
ADDI $sp, $sp, -8
SW $13, 4($sp)
SW $ra, 0($sp)
JAL readIntegerFunction
LW $13, 4($sp)
LW $ra, 0($sp)
ADDI $sp, $sp, 8
XOR $10, $v0, $0
XOR $8, $10, $0
XOR $a0, $8, $0
ADDI $sp, $sp, -8
SW $13, 4($sp)
SW $ra, 0($sp)
JAL fibo
LW $13, 4($sp)
LW $ra, 0($sp)
ADDI $sp, $sp, 8
XOR $11, $v0, $0
XOR $9, $11, $0
XOR $a0, $9, $0
ADDI $sp, $sp, -8
SW $13, 4($sp)
SW $ra, 0($sp)
JAL writeIntegerFunction
LW $13, 4($sp)
LW $ra, 0($sp)
ADDI $sp, $sp, 8
XOR $12, $v0, $0
XOR $v0, $13, $0
ADDI $sp, $sp, 0
ADDI $sp, $sp, 0
LW $fp, 0($sp)
LW $ra, 4($sp)
ADDI $sp, $sp, 8
JR $ra