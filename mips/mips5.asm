.text
	la $t0, x
	lw $t0, 0($t0) # t0 <- x
	mul $t0, $t0, $t0 # t0 <- x*x
	mul $t0, $t0, $t0 # t0 -> x*x * x*x
	
	# Empile
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	
	# t0 <- 2*x*x*x
	la $t0, x
	lw $t0, 0($t0) # t0 <- x
	mul $t1, $t0, $t0 # t1 <- x*x
	mul $t0, $t0, $t1 # t0 <- x* x*x
	li $t1, 2
	mul $t0, $t0, $t1 # 2 * x*x*x
	
	# Empile
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	
	# t0 <- 2*x*x*x
	la $t0, x
	lw $t0, 0($t0) # t0 <- x
	mul $t1, $t0, $t0 # t1 <- x*x
	li $t1, 3
	mul $t0, $t0, $t1 # 3 * x*x
	
	# Empile
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	
	# t0 <- 2*x*x*x
	la $t0, x
	lw $t0, 0($t0) # t0 <- x
	li $t1, 4
	mul $t0, $t0, $t1 # 4 * x
	
	# Empile
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	
	#dépile
	addi $sp, $sp, 4
	lw $t0, 0($sp) #t0 <- 4x
	#dépile
	addi $sp, $sp, 4
	lw $t1, 0($sp) #t1 <- 3*x*x
	
	sub $t0, $t1, $t0
	
	#dépile
	addi $sp, $sp, 4
	lw $t1, 0($sp) #t0 <- 2*x*x*x
	
	sub $t0, $t0, $t1
	
	#dépile
	addi $sp, $sp, 4
	lw $t1, 0($sp) #t0 <- x*x*x*x
	
	add $t0, $t0, $t1
	
	move $a0, $t0
	li $v0, 1
	syscall
.data
x:	.word 3


# t0 <- pow(x, 4)
# on empile le résultat
# t0 <- pow(x, 3)
# t1 <- 2
# t0 <- t1 * t0
# on empile t0
# etc... puis on fait les opération en dépilant