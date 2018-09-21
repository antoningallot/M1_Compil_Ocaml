lw $s0, n
li $t0, 1
li $t1, 0
loop:
	mul $t2, $t0, $t0
	add $t1, $t1, $t2
	addi $t0, $t0, 1
	ble $t0, $s0, loop
	
sw $t1, res
move $a0, $t1
li $v0, 1
syscall

li $v0, 10 # Code exit
syscall
	
.data
n:	.word 3
res:	.word 0