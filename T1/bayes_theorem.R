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
