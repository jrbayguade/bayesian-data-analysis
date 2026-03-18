library(LearnBayes)

# parametros de la distribución beta a priori
apriori = c(a=3, b=27)

#datos 
datos = c(s=15, f=25)

triplot(apriori, datos)