# EJERCICIO 1

a<-c(2,6,5)
b<-c(1,3,4)

# vector -b
-b

# norma -2a
sqrt(sum((-2*a)^2))

# producto escalar ab
sum(a*b)



# EJERCICIO 2

matriz <- matrix(c(1, 0, 0, 1, 2, 2), 3, 2, byrow = TRUE)
matriz_t <- t(matriz)
matriz_t %*% matriz



# EJERCICIO 3
library(Matrix)
rankMatrix(matrix(c(5, 3, 2, 1), 2, 2, byrow = TRUE))


# EJERCICIO 4
a_matrix <- matrix(c(3, 2, 4, 2, 0, 2, 4, 2, 3), 3, 3, byrow = TRUE)
eig <- eigen(a_matrix)
eig


# EJERCICIO 5
b_matrix <- matrix(c(6, -1, 2, 3), 2, 2, byrow = TRUE)
eig_2 <- eigen(b_matrix)
eig_2