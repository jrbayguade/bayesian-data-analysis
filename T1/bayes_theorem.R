###Teorema de Bayes

#define function for Bayes' Theorem

bayesTheorem <- function(pA, pB, pBA) {
  pAB <- pA * pBA / pB
  return(pAB)
}

#define probabilities

pRain <- 0.1
pCloudy <- 0.4
pCloudyRain <- 0.5

#use function to calculate conditional probability

bayesTheorem(pRain, pCloudy, pCloudyRain)


###Fórmula de Bayes


####La fórmula en el caso de que B es binario está dada por:

###P(B|G)=P(G|B)P(B)/(P(G|B)P(B)+P(G|not B)P(not B))

###La fórmula necesita la priori P(B), P(G|B) que puede verse como
###la verosimilitud y de P(G|not B)

bayes_theorem <- function(p_b, p_g_given_b, p_g_given_not_b){
  # calculate P(not B) (complemento de P(B))
  not_b = 1 - p_b
  # calculate P(G). Es el denominador de la fórmula, P(G|B)P(B)+P(G|not B)P(not B)
  p_g = p_g_given_b * p_b + p_g_given_not_b * not_b
  # calculate P(B|G)
  p_b_given_g = (p_g_given_b * p_b) / p_g
  return(p_b_given_g)
}

bayesRule <- function(PBA, PBnoA, PA, PnoA){
  +(BPA * PA) / (BPA * PA + PBnoA * PnoA)
} 

###En nuestro ejemplo
#P(B) (es la probabilidad de que sea inocente)
p_b = 0.15
# P(G|B) (es la probabilidad de que sea convicto dado que es inocente)
p_g_given_b = 0.2
# P(G|notB) (es la probabilidad de que sea convicto dado que es culpable)
p_g_given_not_b = 0.82
# calculate P(B|G)
result = bayes_theorem(p_b, p_g_given_b, p_g_given_not_b)
result



# Definim la funció Negative Log-Likelihood (NLogV)
# Els arguments han de ser els paràmetres que vols estimar
NLogV <- function(mu, sigma) {
  # Calculem el logaritme de la densitat per a cada dada
  log_likelihood <- dnorm(datos_aleatorios, mean = mu, sd = sigma, log = TRUE)
  
  # Retornem el negatiu de la suma (perquè mle minimitza)
  return(-sum(log_likelihood))
}


NNPosteriori <- function(datamean, sigma, n, mu, tau){
  posteriorimean <- (n*datamean*tau^2+mu*sigma^2)/(n*tau^2+sigma^2)
  posteriorisigma <- sqrt(sigma^2*tau^2/(n*tau^2+sigma^2))
  
  resultado = list("posteriorimean"=posteriorimean, 
                   "posteriorisigma"=posteriorisigma)
  
  return(resultado)
}

BetaBetaPosteriori <- function (datasum, n, alfa, beta) {
  posteriorialfa <- alfa + datasum
  posterioribeta <- beta + n - datasum
  
  resultado = list("posteriorialfa"= posteriorialfa,
                   "posterioribeta"= posterioribeta)
  
  return(resultado)
}