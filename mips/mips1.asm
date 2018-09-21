# Chargement des constantes
li $a0, 3
li $t1, 2
mul $t1, $t1, $a0
addi $t1, $t1, 1
move $a0, $t1
li $v0, 1
syscall

li $v0, 10 # Code exit
syscall