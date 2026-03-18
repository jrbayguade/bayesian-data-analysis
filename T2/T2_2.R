set.seed(1234)
datos_aleatorios <- rnorm(n = 25, mean = 12, sd = 3)

yaleatorio <- rep(0, length(datos_aleatorios))

plot(datos_aleatorios, yaleatorio, type = "p", col = "blue")

NLogV <- function(mu, sigma) {
  -sum(dnorm(datos_aleatorios, mu, sigma, log = TRUE))
}

Valores_estimados = mle(NLogV, start = list(mu=12, sigma=3))

summary(Valores_estimados)

confint(Valores_estimados)