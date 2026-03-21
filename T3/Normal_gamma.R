#Se toman 28 observaciones

#25 20 25 18 24 26 24 24 24 24 18 18 17 17
#21 15 13 12 25 19 18 24 25 18 25 24 24 25

#Calcular la probabilidad P(\mu>=20)

#Asumimos que la muestra es de una distribución normal X_i \sim N(\mu,\sigma^2), i=1,...,n, con n=28

###Asumiendo distribuciones no informativas para la media y varianza
###Aquí corresponde a que la media tenga una distribución normal con media cero y precisión
####(inverso de la varianza) muy pequeña. En otras palabras, media cero y una dispersión
####enorme. Recordemos que la priori no informativa de mu es proporcional a 1, o sea es 
####una constante la cual sobre el dominio de la normal no es una integral propia, por eso
###partimos de la normal
###Mientras tanto en la gamma asociada a la dispersión (equivalente a gama inversa en la varianza) 
####tiene media \alpha/\beta=0.01/0.01=1 y varianza \alpha/\beta^2=0.01/(0.01)^2=100, detal modo
#que da posibilidad a casi cualquier valor

library("rjags")

#Mismo ejemplo de antes (ojo con los comentarios, ya no se pueden usar símbolos)
mod_normal = "model {
  for (i in 1:n) {
    y[i] ~ dnorm(mu, prec) # y_i|mu se dist N(mu,prec) usa la precision, inverso de la varianza
  }
  mu ~ dnorm(0, 0.001) 
  prec ~ dgamma(0.01, 0.01) 
  sigma2=1/prec
}"

##Set up the model

#Damos la semilla, datos y tamaño de muestra
set.seed(50)
y = c(25, 20, 25, 18, 24, 26, 24, 24, 24, 24, 18, 18, 17, 17, 21, 15, 13, 12, 25, 19, 18, 24, 25, 18, 25, 24, 24, 25)
n = length(y)
n #28
#Los datos se introducen en jags como una lista con los datos y el tamaño de muestra
data_jags = list(y=y, n=n)
#Los parámetro desconocido son \mu y sigma2
params = c("mu","sigma2")

#Inicializamos con un valor de \mu=0, como en el código de arriba
#here are multiple ways to specify initial values here. They can be explicitly set, 
#as we did here, or they can be random, i.e., list("mu"=rnorm(1))

#inits = function() {
#  inits = list("mu"=0.0)
#} # optional (and fixed)

#Compile the model
#Especificamos el modelo según mod_normal. Nos da el tamaño de muestra  (stochastic nodes)
#los parámetros (unobserved stochastic nodes). Graph size toma en cuenta
#los parámetros que especificamos 
mod = jags.model(textConnection(mod_normal), data=data_jags)

##Run the MCMC sampler

#Corremos el muestreo tipo MCMC

update(mod, 500) # burn-in (500 muestras más de burn-in que el número de iteraciones con las que
#me quedo, abajo vemos que son 1000, y entonces son 1500)
#Usamos el modelo, los parámetros que estamos evaluando (en este caso solo \mu
#) y el número de iteraciones, 1000
mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=1000)
mod_sim #la salida son las 1000 simulaciones de la distribución posterior de \mu y \sigma^2

##Postprocessing 

#El summary indica la media, desviación estándar, el error estándar de la media
#y cuartiles según la distribución a posteriori, \mu|x, simulada y similar para la
#varianza \sigma^2|x
summary(mod_sim)
#Trace plot como antes
#incluye las iteraciones después de las 1500 de burn-in, aquí vemos que la serie
#es estacionaria. Además se tiene la distribución posterior de \mu y \sigma^2
library("coda")
plot(mod_sim)
##A partir de la simulación  de la distribución a posteriori, podemos obtener la 
#probabilidad de que \mu sea mayor o igual que 20. Tomaríamos los datos que son >=20, entre
#el número de observaciones simualdas, que fueron 1000 (hubo 500 de burnin y por eso 
#fueron 1500 observaciones)

###Convertir la salida del proceso MCMC en un data.frame
mod_sim_df<-as.data.frame(mod_sim[[1]])
mod_sim_df
##Proporción de observaciones con mu simulada de la distribución a posteriori que son mayores 
##o iguales que 20. Vemos que P(\mu>=20)=0.91, valor cercano al dado en la p. 27 de tema3bayes.pdf
sum(mod_sim_df$mu>=20)/1000

###Solución usando la librería Learnbayes. Se basa en simular la media y las varianzas
###de acuerdo a la distribución teórica a posteriori con prioris no informativas (dafault)
###o informativas, usando los parámetros correspondientes

library(LearnBayes)
#Simulación con priori no informativa 
post_learnbayes<-normpostsim(y,m=1000)
post_learnbayes
#medias simuladas posteriores
post_learnbayes$mu
#Histograma
hist(post_learnbayes$mu)
##Proporción de observaciones con mu simulada de la distribución a posteriori que son mayores 
##o iguales que 20. Vemos que P(\mu>=20)=0.929, valor cercano al dado en la p. 27 de tema3bayes.pdf
sum(post_learnbayes$mu>=20)/1000


###Si usaramos informativas, por ejemplo que supiéramos que la media tiene distribución
###con media 1 y varianza 10 y para la sigma^2 parámetros \alpha=1 y gamma=1

post_learnbayes<-normpostsim(y,list(mu=c(1,10),sigma2=c(1,1)),m=1000)
post_learnbayes
#medias simuladas posteriores
post_learnbayes$mu
#Histograma
hist(post_learnbayes$mu)
##Proporción de observaciones con mu simulada de la distribución a posteriori que son mayores 
##o iguales que 20. Vemos que P(\mu>=20)=0.483, vemos que es importante usar una a priori 
#adecuada pues pueden obtenerse distintos resultados
sum(post_learnbayes$mu>=20)/1000


