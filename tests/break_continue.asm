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
	la $t0, cun
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	sw $t1, 0($t0)
	li $t0, -1
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	la $t0, cdeux
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	sw $t1, 0($t0)
	li $t0, 0
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	la $t0, i
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	sw $t1, 0($t0)
_label_1:
	la $t0, cun
	lw $t0, 0($t0)
	bne $zero, $t0, _label_2
	b _label_3
_label_2:
	li $t0, 0
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	la $t0, cun
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	sw $t1, 0($t0)
_label_4:
	la $t0, cdeux
	lw $t0, 0($t0)
	bne $zero, $t0, _label_5
	b _label_6
_label_5:
	li $t0, 10
	move $a0, $t0
	li $v0, 1
	syscall
	b _label_6
	li $t0, 53
	move $a0, $t0
	li $v0, 1
	syscall
	b _label_4
_label_6:
	b _label_3
	li $t0, 20
	move $a0, $t0
	li $v0, 1
	syscall
	b _label_1
_label_3:
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
cun:
	.word 0
cdeux:
	.word 0
arg:
	.word 0
