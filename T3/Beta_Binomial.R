###Regresando a nuestros datos de los goles, supongamos que queremos abordarlo
#desde un enfoque bayesiano. 

library(stats)
library(LearnBayes)

#Número de goles por portero
y<-c(1, 5, 1, 2, 2, 2, 4, 3, 4, 3, 2, 1, 2, 3, 1, 1, 1)
y
#Número de tiros por cada portero
n<-rep(10,17) 
n

##Y_i|p ~ Bin(10,p) y necesitamos una distribución para la probabilidad de un gol
##Como no tenemos información, asumamos que cada probabilidad tiene la misma 
#probabilidad p ~ Uniforme(0,1)

###Una distribución Uniforme(0,1) es equivalente a una distribución Beta(1,1)

# a = 1, b = 1
pi <- Vectorize(function(theta)  dbeta(theta,1,1))
curve(pi, xlab=~theta, ylab="Density", main="Beta prior: a=1, b=1",lwd=2)


##Tenemos entonces una muestra de m observaciones de v.a.s indep Y_i|p~ Bin(n_i,p),
##con i=1,...,m; m=17, y n_i = 10 para toda i. Puede demostrarse que
##si se usa una distribuci\'on a priori p ~ Beta(\alpha,\beta) y la información
## de toda la muestra observada y=(y_1,...,y_n), entonces
## la distribución posteriori es  p|y ~ Beta(alpha+\sum^m_{i=1} y_i, \beta + \sum^m_{i=1} n_i-\sum y_i)

#Número de goles por portero
y<-c(1, 5, 1, 2, 2, 2, 4, 3, 4, 3, 2, 1, 2, 3, 1, 1, 1)
y
#Número de tiros por cada portero
n<-rep(10,17) 
n

#Totales

#Total de goles de todos los individuos
tot_y<-sum(y)
tot_y
#Total de ensayos de todos los individuos 
tot_n<-sum(n)
tot_n
#Total de fallos de todos los individuos
fallos=tot_n-tot_y
fallos
##Si usamos una distribución uniforme, la distribución a priori y a posterori coinciden
###(no es alacanza a ver la línea roja), en este caso se dice que es no informativa

apriori = c( a= 1, b = 1 ) #parametros de la distribución beta a priori
datos=c(s=tot_y, f=fallos) #datos
triplot(apriori,datos)

#pbetap(c(1,1), tot_n, tot_y)

##Cambiar \alpha y \beta a valores ligeramente lejanos a 1 (se ve similar)

apriori = c( a= 1.2, b = 1.2 ) #parametros de la distribución beta a priori
datos=c(s=tot_y, f=fallos) #datos
triplot(apriori,datos)

##Probar otros valores
###Media de una beta \alpha/(\alpha+\beta)
##varianza de una beta (\alpha)(\beta)/((\alpha+\beta)^2(\alpha+\beta+1))

alfa=1
beta=10
apriori = c( a= alfa, b = beta ) #parametros de la distribución beta a priori
datos=c(s=tot_y, f=fallos) #datos
triplot(apriori,datos)
##Media distribución a priori
alfa/(alfa+beta)
#Varianza distribución a priori
(alfa*beta)/((alfa+beta)**2*(alfa+beta+1))


###En este ejemplo, nuestra información previa indicaría que 
#la media de la probabilidad de anotar un gol es de 0.5
alfa=2
beta=2
apriori = c( a= alfa, b = beta ) #parametros de la distribución beta a priori
datos=c(s=tot_y, f=fallos) #datos
triplot(apriori,datos)
##Media distribución a priori
alfa/(alfa+beta)
#Varianza distribución a priori
(alfa*beta)/((alfa+beta)**2*(alfa+beta+1))

###En este ejemplo, nuestra información previa indicaría que 
#la media de la probabilidad de anotar un gol es de 0.5 con mayor varianza
alfa=0.5
beta=0.5
apriori = c( a= alfa, b = beta ) #parametros de la distribución beta a priori
datos=c(s=tot_y, f=fallos) #datos
triplot(apriori,datos)
##Media distribución a priori
alfa/(alfa+beta)
#Varianza distribución a priori
(alfa*beta)/((alfa+beta)**2*(alfa+beta+1))

###En este ejemplo, nuestra información previa indicaría que 
#la media de la probabilidad de anotar un gol es de 0.83
#Notar que aquí nuestra información previa está más cargada a que
#la probabilidad de anotar gol es mayor y sí se mueve más la distribución
#posterior a la derecha
alfa=5
beta=1
apriori = c( a= alfa, b = beta ) #parametros de la distribución beta a priori
datos=c(s=tot_y, f=fallos) #datos
triplot(apriori,datos)
##Media distribución a priori
alfa/(alfa+beta)
#Varianza distribución a priori
(alfa*beta)/((alfa+beta)**2*(alfa+beta+1))

###Supóngase que se elige la última priori, podemos obtener una estimación
###puntual bayesiana, el cual bajo pérdida cuadrática corresponde a la media
##de la distribución a posteriori p|y ~ Beta(alpha+\sum^m_{i=1} Y_i, \beta + \sum^m_{i=1} n_i-\sum Y_i)
### así que corresponde a
### (alpha+\sum^m_{i=1} Y_i)/(alpha+\sum^m_{i=1} Y_i+\beta + \sum^m_{i=1} n_i-\sum Y_i)

##Distribución posteriori a detalle

a=alfa+tot_y
b=beta+tot_n-tot_y
pi <- Vectorize(function(theta)  dbeta(theta,a,b))
curve(pi, xlab=~theta, ylab="Density", main="Beta posterior",lwd=2)

###Estimador puntual bayesiano de la proporción de goles con la priori usada
###Da un valor de 0.2443
(alfa+tot_y)/(alfa+tot_y+beta+tot_n-tot_y)

###En el caso no informativo, priori Beta(1,1), o sea uniforme

alfa=1
beta=1

###Estimador puntual bayesiano de la proporción de goles con la priori usada
###Vemos que casi coincide con el estimador puntual obtenido en el enfoque clásico,
###que resulto de 0.223

(alfa+tot_y)/(alfa+tot_y+beta+tot_n-tot_y)


###Suponer que se tiene la hipótesis de que la proporción de goles en un equipo
##con oportunidad de competir debiera estar entre 0.3 y 0.4 ¿qué podemos decir
##de nuestro equipo?

###De la probabilidad a posteriori (con alpha=5,beta=1) podemos calcular P(0.3<=p<0.4)=P(p<0.4)-P(p<0.3)

#Probabilidad de que se menor a 0.4

pbeta(0.4,a,b)

#Probabilidad de que se menor a 0.3

pbeta(0.3,a,b)

###P(0.3<=p<0.4) es muy baja, así que el equipo no tiene mucha oportunidad de competir

pbeta(0.4,a,b)-pbeta(0.3,a,b)

#####Resolución con MCMpack (Funciona  través de simulaciones que se verán más adelante)
####Es una solución aproximada

library (MCMCpack )

#Cálculo asumiendo una priori con alpha=1 y beta=5, requerimos el total de goles 
#en todos los inidviduos, tot_y el total de ensayos en todos los individuos, tot_n
posterior = MCbinomialbeta (tot_y , tot_n, alpha = 5, beta = 1, mc =10000)
###vemos que la media de las simulaciones coincide casi con el valor que calculamos
#arriba
summary ( posterior )

#Distribución posterior
plot ( posterior )

###Valores sobre los que se grafica (eje X)
grid = seq (0 ,1 ,0.01 )
#Gráfica de la distribución a priori
plot (grid , dbeta (grid , 1, 5), type ="l", col ="red ",
      lwd =3, ylim =c(0 ,13), xlab = expression (pi), ylab =" Densidad ")
#Agregamos la distribución a posteriori de acuerdo a las simulaciones
lines ( density ( posterior ), col=" blue ", lwd =3)
legend (0.5 , 10 , c("A priori ", "A posteriori "),
        lwd =2, col =c("red", " blue "))
