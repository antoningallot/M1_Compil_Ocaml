# Init
li $a0, 3

li $t0, 1
li $t1, 0
loop:
	mul $t2, $t0, $t0
	add $t1, $t1, $t2
	addi $t0, $t0, 1
	ble $t0, $a0, loop
	
move $a0, $t1
li $v0, 1
syscall

li $v0, 10 # Code exit
syscall
	