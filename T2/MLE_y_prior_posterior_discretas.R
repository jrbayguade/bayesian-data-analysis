#Cálculo de estimador máximo verosímil

#Se hace un experimento con 17 porteros que no se conocen  y se mide el número de veces que
#se les mete gol al hacer 10 tiros a la portería. Basándose en los resultados del siguiente 
#vector de número de goles por portero, proporcionar un estimador de la probabilidad de
#de meter un gol según los resultados del experimento  y del número esperado de goles

#Podemos asumir que la distribución de Y_i=número de goles al portero i corresponde
# a una binomial, Y_i ~ Bin(10,p), con p la probabilidad de éxito (un gol) y que las Y_i
#son independientes pues los jugadores no se conocen (han entrenado juntos), así que
#tenemos una m.a. de binomiales

library(stats)
# MLE for Binomial Distribution. 

#Número de goles por portero
y<-c(1, 5, 1, 2, 2, 2, 4, 3, 4, 3, 2, 1, 2, 3, 1, 1, 1)
y
#Número de tiros por cada portero
n<-rep(10,17) 
n

#La función de verosimilitud de v.a. indep es la suma de
#las distribuciones de cada individuo

# formulation for the log likelihood for the binomial 
logL <- function(p) sum(log(dbinom(y, n, p)))
#Log verosimilitud para una p dada,  0 < p < 1
logL(0.7)

#Plot logL, el eje x es una secuencia de probabilidades de 0 a 0.99 de 1 en 1
p.seq <- seq(0, 0.99, 0.01)
plot(p.seq, sapply(p.seq, logL), type="l")

#Optimum:
optimo<-optimize(logL, lower=0, upper=1, maximum=TRUE)
optimo

#También está disponible la funci\'on mle() de la librería stats

library(stats4)
#datos_aleatorios=rbinom(n=100,size=10,p=0.2)
#datos_aleatorios
#NLogV=function(mu,sigma){-sum(dnorm(y,mu,sigma,log=TRUE))}
mlogL <- function(p){-sum(dbinom(y, n, p,log=TRUE))}
mlogL
mle(mlogL,start=list(p=0.25),nobs = NROW(y))

#Esto indica que la probabilidad que maximiza la función de log verosimilitud es 0.2235
#En otras palabras, según los datos el estimador de la probabilidad de meter un gol
#que tiene más posibilidad de ser observado es de 0.2235

#El número esperado de goles por jugador, corresponde a E[X_i]=10*p, así que podemos estimarlo

#El siguiente sería un estimador máximo verosímil de np, n=10, o sea de la esperanza,
#de acuerdo a la propiedad de invarianza en estimadores máximo verosímiles

10*optimo$maximum

#El número esperado de goles es de 2.23

#La varianza de número de goles por jugador corresponde a V[X_i]=10*p*(1-p)

#El siguiente sería un estimador máximo verosímil de np(1-p), n=10, o sea de la varianza,
#de acuerdo a la propiedad de invarianza en estimadores máximo verosímiles

10*optimo$maximum*(1-optimo$maximum)

##De forma manual se demuestra que el estimador de esta proporción p es (\sum x_i/m)/n con
# m el tamaño de la muestra y n el parámetro de la binomial

#Este coincide con el valor calculado
mean(y)/10


#######Cálculo de la distribución posteriori para una distribución a priori 
###de un parámetro discreto y distribución en los datos (o verosimilitud) también discreta

#One wishes to learn about a baseball player's probability of getting
#a hit p. A reasonable set of probabilities are 0.20, 0.21, ..., 0.36 and assigning to
#these probabilities the corresponding weights 1, 1, 1, 2, 2, 2, 4, 4, 4, 4, 2, 2, 2, 2, 1, 1, 1. In
#R


#options(digits = 4)
# options(width = 60)

#Pesos a cada probabilidad
prior = c(1, 1, 1, 2, 2, 2, 4, 4, 4, 4, 2, 2, 2, 2, 1, 1, 1)
#Secuencia de probabilidades
names(prior) = seq(0.2, 0.36, by = 0.01)
#la distribución a priori es estimada como la frecuencia relativa
prior = prior/sum(prior)
prior

plot(prior)

##Otra opción de gráfico

#valores de p
p = seq(0.2, 0.36, by = 0.01)
plot (p, prior , type ="h", ylab =" Probabilidad a priori ", col =4)

#Observe the player's hitting performance for four periods of 80 at-bats (opportuntities). For
#these four periods, he is 20 for 80, 22 for 80, 19 for 80, and 29 for 80. I

#Si lo pensamos estamos midiendo en cada uno de los 4 periodos el número de veces que
#hay éxito (hit) en n=80 ensayos. Esto corresponde a una v.a. Binomial(80,p)

#Éxitos
y = c(20, 22, 19, 29)
y
#Número de ensayos
n = c(80, 80, 80, 80)
n
#Desde el punto de vista bayesiano, tenemos X_i|p ~ Bin(80,p), i=1,...,4, con p que tiene una distribución
#empírica dada por prior. Esto lo podemos hacer a través de learn Bayes

#Queremos saber cómo se actualiza el parámetro p, después de observar los datos del jugador
#o sea queremos p|x con x=(x_1,...,x_4)

library(LearnBayes)

out = discrete.bayes(dbinom, prior, y, size = n)
#Probabilidades a posteriori
print(out)

#Histograma de la distribución a priori y posteriori (valores guardados en out$prob)
#Vemos cómo la información se actualiza

par(mfrow = c(2, 1))
barplot(prior, main = "Prior Probabilities", xlab = "p", ylim = c(0, 0.2))
barplot(out$prob, main = "Posterior Probabilities", xlab = "p")
par(mfrow = c(1, 1))

#####Cáluclo de una a posteriori cuando los parámetros a priori son continuos
#####y la verosimilitud es discreta
#Establecer la distribución a priori cuando el parámetro asociado es continuo
#a través de generar intervalos de valores del parámetro y posibles pesos en cada intervalo

###Se podría analizar por intervalos también, por ejemplo, supóngase que le damos peso
###a las pobabilidades por intervalos: (0.1,0.2),....,(0.8,0.9)
### con pesos 4; 8; 8; 4; 2; 1; 1; 1; en cada intervalo

#Punto medio de cada intervalo para poder obtener el histograma de 
#con un mejor dibujo
midpt = seq (0.15, 0.85 , by = 0.1)
midpt
#Pesos que a priori conocemos debe tener cada intervalo
prior = c(4, 8, 8, 4, 2, 1, 1, 1)
prior
#Frecuencias relativas por intervalo
prior = prior /sum( prior )
prior
#Histograma de la distribución a priori de p

#Valores que puede tomar p, entre 0.1 a 0.9, 500 valores (dividimos en 500 porque son
#los valroes que se tienen en la instrucción histprior())
p = seq (0.1,0.9, length = 500)
p
plot (p, histprior(p,midpt,prior), type ="l", ylab =" Densidad a priori ", ylim =c(0, .3 ))

###Distribución posterior, es proporcional al producto de la verosimilitud y la densidad a priori

#Verosimilitud, es el producto de las distribuciones binomiales de los 4 periodos

prod=1
for (i in 1:length(y)){
  prod=prod*dbinom(y[i], n[i], p)
}
###Para cada p tenemos un valor
prod

### A mano
#p1=dbinom(y[1], n[1], p)
#p2=dbinom(y[2], n[2], p)
#p3=dbinom(y[3], n[3], p)
#p4=dbinom(y[4], n[4], p)
#p1*p2*p3*p4

#La distribución a posteriori es el producto de la verosimilitud y la priori que obtuvimos a partir
#de las frecuencias relativas del parámetro p cuando viene con intervalos y el peso de cada intervalo

#producto de la verosimilitud
post = prod * histprior (p, midpt , prior )
plot (p, post , type ="l", ylab =" Densidad a posteriori ", col =4)

##Muestra de valores de p de acuerdo a la distribución posterior que obtuvimos

#Distribución empírica
post = post /sum ( post )
#Muestra con remplazo de las p de acuerdo a las probabilidades a posteriori
#obtenidas
ps = sample (p, replace =TRUE , prob = post )
ps
hist (ps , xlab ="p")

###Simulación de valores de y, número de éxitos o hits, de acuerdo a la probabilidad a
#posteriori simuladas (recordar hay 80 ensayos), a esto se le llama predictiva

#Simulamos 1000 observaciones
y = rbinom (1000 , 80, ps)
table (y)
#Frecuencias simuladas del número de hits
freq = table (y)

#Dibujamos la distribución de las predicciones. La moda está en 23 hits

predprob = freq /sum( freq )
predprob
plot (predprob , type ="h",xlab ="y",ylab =" Probabilidad predictiva ")

