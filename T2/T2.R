library(ggplot2)

datos <- read.csv("T2/datosupon.csv", sep = ",", header = TRUE)
datos

ggplot(datos, aes(x = factor(Ratio), y = Frecuencia, fill = Autores)) +
  geom_bar(stat = "identity", position = "dodge")

PHamilton = 0.75
PMadison = 0.25

# Filtro "Hamilton", columna 2 (frecuencias)
datosFrecuenciaHamilton = datos[datos$Autores == "Hamilton",][2]
datosFrecuenciaHamilton

FrecuenciaHamilton0.996 <- datos[datos$Ratio == "(0,1]" & datos$Autores == "Hamilton",][2]
FrecuenciaHamilton0.996

SumaHamilton <- sum(datos$Frecuencia[datos$Autores == "Hamilton"])
P0_996Hamilton = FrecuenciaHamilton0.996 / SumaHamilton
P0_996Hamilton

# Filtro "Madison", columna 2 (frecuencias)
datosFrecuenciaMadison = datos[datos$Autores == "Madison",][2]
datosFrecuenciaMadison

FrecuenciaMadison0.996 <- datos[datos$Ratio == "(0,1]" & datos$Autores == "Madison",][2]
FrecuenciaMadison0.996

SumaMadison <- sum(datos$Frecuencia[datos$Autores == "Madison"])
P0_996Madison = FrecuenciaMadison0.996 / SumaMadison
P0_996Madison

source("T1/bayes_theorem.R")

PHamilton0_996 = bayes_theorem(PHamilton, P0_996Hamilton, P0_996Madison)
PHamilton0_996

PHMadison0_996 = bayes_theorem(PMadison, P0_996Madison, P0_996Hamilton)
PHMadison0_996
 
resultados0_966 <- data.frame("autor" = c("Hamilton", "Madison"), 
                              "ProbaPriori" = c(PHamilton, PMadison),
                              "Verosimilitud" = c(P0_996Hamilton$Frecuencia, P0_996Madison$Frecuencia),
                              "ProbaPosteriori" = c(PHamilton0_996$Frecuencia, PHMadison0_996$Frecuencia))
resultados0_966