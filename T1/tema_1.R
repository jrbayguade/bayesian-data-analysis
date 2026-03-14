source("T1/bayes_theorem.R")  # Load functions within bayes_theorem.R file
library(LearnBayes)           # Load library

# Basic data manipulation exercises in R and the Bayes rule

# Exercise 1: preloaded dataset
HairEyeColor

# Exercise 2: (add up two tables)
addition = apply(HairEyeColor, c("Hair", "Eye"), sum)
addition

# Exercise 3: frequencies
propportionHairEye <- addition / sum(addition)
propportionHairEye

# Exercise 4
propportionHair <- apply(propportionHairEye, c("Hair"), sum)
propportionHair

# Exercise 5
propportionEye <- apply(propportionHairEye, c("Eye"), sum)
propportionEye

# Add to the table:
propportionHairEye <- cbind(propportionHairEye, propportionHair)
propportionHairEye <- rbind(propportionHairEye, c(propportionEye, 1.0))
propportionHairEye

# Calculate probabilities with the Bayes Theorem
probBlack <- propportionHair["Black"]
probBlue <- propportionEye["Blue"]

probBlueBlack <- propportionHairEye["Black", "Blue"] / probBlack
probBlueBlack

probBlackBlue <- probBlueBlack * probBlack / probBlue
probBlackBlue

#Exercise 6: 
propportionHairEye["Brown",] / propportionHair["Brown"]