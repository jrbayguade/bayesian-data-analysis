# Cuaderno de ejercicios
source("T1/bayes_theorem.R")

# Ejercicio 1
x <- seq(2, 5, 0.01)
plot(x, dnorm(x, 3.6, 0.2), type="l", ylim=c(0,2), ylab="", lwd=2, col="blue")


# Ejercicio 2
library(stats4)
mu <- 3
sd <- 0.6

datos_aleatorios = c(2.4, 2.5, 2.6, 2.65, 3.2)
yaleatorio = rep(0, times = length(datos_aleatorios))

plot(datos_aleatorios, yaleatorio, type="p", col="blue")

Valores_estimados = mle(NLogV, 
                        start=list(mu=mu, sigma=sd), 
                        lower = c(-Inf, 0.001))

summary(Valores_estimados)


# Ejercicio 3
res <- NNPosteriori(2.67, 0.28, 5, 3.6, 0.2)

res$posteriorimean
res$posteriorisigma

plot(x, dnorm(x, 3.6, 0.2), type="l", ylim=c(0,5), ylab="", lwd=2, col="blue")
lines(x, dnorm(x, 2.67, 0.28), type="l", col="green")
lines(x, dnorm(x, 2.93, 0.1), type="l", col="red")
legend("topright", 
       legend = c("Priori", "Verosimilitud", "A posteriori"), 
       lty = 1, 
       col = c("blue", "green", "red"), 
       lwd = 2)


# Ejercicio 5
datos_aleatorios = rnorm(20, 3, 0.6)
datos_aleatorios

Valores_estimados = mle(NLogV, start = list(mu = mu, sigma = sd))

b_b <- BetaBetaPosteriori(6, 75, 1, 6)
b_b$posteriorialfa
b_b$posterioribeta

p = seq(0, 1, 0.001)
# Prior (alfa=1, beta=6)
plot(p, dbeta(p, 1, 6), type="l", col="blue", lwd=2, ylim=c(0,15))
# Posterior (alfa=7, beta=75)
lines(p, dbeta(p, b_b$posteriorialfa, b_b$posterioribeta), col="red", lwd=2)

legend("topright", c("Prior", "Posterior"), col=c("blue", "red"), lty=1, bty="n")