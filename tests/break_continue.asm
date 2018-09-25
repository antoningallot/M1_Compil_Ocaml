.text
	beqz $a0, init_end
	lw $a0, 0($a1)
	jal atoi
	la $t0, arg
	sw $v0, 0($t0)
init_end:
	li $t0, -1
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	la $t0, continuee
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	addi $sp, $sp, 4
	lw $t0, 0($sp)
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	sw $t1, 0($t0)
	li $t0, 0
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	la $t0, i
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	addi $sp, $sp, 4
	lw $t0, 0($sp)
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	sw $t1, 0($t0)
_label_1:
	la $t0, continuee
	lw $t0, 0($t0)
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	addi $sp, $sp, 4
	lw $t0, 0($sp)
	bne $zero, $t0, _label_2
	b _label_3
_label_2:
	la $t0, i
	lw $t0, 0($t0)
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	li $t0, 2
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	addi $sp, $sp, 4
	lw $t0, 0($sp)
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	sgt $t0, $t1, $t0
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	addi $sp, $sp, 4
	lw $t0, 0($sp)
	bne $zero, $t0, _label_4
	b _label_5
_label_4:
	b _label_3
_label_5:
	li $t0, 35
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	addi $sp, $sp, 4
	lw $t0, 0($sp)
	move $a0, $t0
	li $v0, 1
	syscall
	la $t0, i
	lw $t0, 0($t0)
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	li $t0, 1
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	addi $sp, $sp, 4
	lw $t0, 0($sp)
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	add $t0, $t1, $t0
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	la $t0, i
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	addi $sp, $sp, 4
	lw $t0, 0($sp)
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	sw $t1, 0($t0)
	b _label_1
_label_3:
	li $t0, 27
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	addi $sp, $sp, 4
	lw $t0, 0($sp)
	move $a0, $t0
	li $v0, 1
	syscall
	li $v0, 10
	syscall
atoi:
	move $t0, $a0
	li $t1, 0
	li $t3, 10
	li $t4, 48
	li $t5, 57
atoi_loop:
	lbu $t2, 0($t0)
	beq $t2, $zero, atoi_end
	blt $t2, $t4, atoi_error
	bgt $t2, $t5, atoi_error
	addi $t2, $t2, -48
	mul $t1, $t1, $t3
	add $t1, $t1, $t2
	addi $t0, $t0, 1
	b atoi_loop
atoi_error:
	li $v0, 10
	syscall
atoi_end:
	move $v0, $t1
	jr $ra
.data
i:
	.word 0
continuee:
	.word 0
arg:
	.word 0
