library(stats)
library(LearnBayes)

# Número de goles por portero
y<-c(1, 5, 1, 2, 2, 2, 4, 3, 4, 3, 2, 1, 2, 3, 1, 1, 1)
y

# Número de tiros por cada portero
n<-rep(10, 17)
n

# Una distribución uniforme(0,1) es quivalente a una distribución Beta(1, 1)

# a = 1, b = 1
pi <- Vectorize(function(theta) dbeta(theta, 1, 1))
curve(pi, xlab=~theta, ylab="Density", main="Beta prior: a=1, b=1", lwd=2)

# Total de goles de todos los individuos
tot_y <- sum(y)
tot_y

# Total de ensayos de todos los individuos
total_n <- sum(n)
total_n

# Total de fallos de todos los individuos
fallos = total_n - tot_y
fallos


apriori = c(a=1, b=1) # parámetros distribución beta a priori
datos = c(s=tot_y, f= fallos) # datos
triplot(apriori, datos)


alfa = 1
beta = 10
apriori = c(a=alfa, b = beta) # parámetros distribución beta a priori
triplot(apriori, datos)
alfa/(alfa + beta) # media distribución a priori
# Varianza distribución a priori
(alfa*beta)/((alfa+beta)**2*(alfa+beta+1))



alfa =2
beta = 2
apriori = c(a=alfa, b = beta) # parámetros distribución beta a priori
triplot(apriori, datos)
alfa/(alfa + beta) # media distribución a priori
# Varianza distribución a priori
(alfa*beta)/((alfa+beta)**2*(alfa+beta+1))


