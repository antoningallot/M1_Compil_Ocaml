# Init
li $a0, 3

# Chargement des constantes
li $t0, 4
li $t1, 3
li $t2, 2
mul $t0, $a0, $t0 # 4*x
mul $t3, $a0, $a0 # x*x
mul $t1, $t1, $t3 # 3*x*x
sub $t0, $t0, $t1 # 3*x*x - 4*x
mul $t3, $t3, $a0 # x*x*x
mul $t2, $t2, $t3 # 2*x*x*x
add $t0, $t0, $t2 # 2*x*x*x + ...
mul $t3, $t3, $a0 # pow(x, 4)
sub $t0, $t3, $t0

move $a0, $t0
li $v0, 1
syscall

li $v0, 10 # Code exit
syscall