lw $s0, x

# Empile 
addi $sp, $sp, -4
sw $s0, 0($sp)

mul $t0, $s0

# Depile
addi $sp, $sp, 4


move $a0, $t1
li $v0, 1
syscall

li $v0, 10 # Code exit
syscall
	
.data
x:	.word 1