#Declaration of features and feature values
setwd("D:\\SCMA 2024\\Data")
getwd()
====================================================================

feature_values <- list()
feature_values[[1]] <- c("Strong", "Average")
feature_values[[2]] <- c("AFN 30 - 50","AFN 50 - 70 ", "AFN 70 - 90")
feature_values[[3]] <- c("100 - 200 ml", "200 - 350 ml")
feature_values[[4]] <- c("Opaque", "Pearliest", "Clear")
feature_values[[5]] <- c("High", "Medium")
feature_values[[6]] <- c("Dandruff" , "Hair Fall",  "Hair Texture")

#All concept generation
articles <- expand.grid(
  Brand=feature_values[[1]],
  Price=feature_values[[2]],
  Size=feature_values[[3]],
  Colour=feature_values[[4]],
  Foaming=feature_values[[5]],
  Remedy=feature_values[[6]]
)


set.seed(100)
library (conjoint)
maxNumberOfArticles = 12
#Selection of relevant concepts
selectedArticles <- caFactorialDesign(
  data=articles,
  type='fractional',
  cards=maxNumberOfArticles
)

#These are your cards for administering
selectedArticles
#Checking if selected concepts are relevant for study
corrSelectedArticles <- caEncodedDesign(selectedArticles)
print(corrSelectedArticles)
#TO SHOW THE SELECTION IS ORTHOGONAL
print(cor(corrSelectedArticles))

# Here is a sample result which are the hypothetical ranking
ranking <- read.csv("ranking.csv", header=TRUE)
View(ranking)
levels = read.csv("levels.csv", header=TRUE)
levels

c <- Conjoint(ranking,selectedArticles,unlist(feature_values))

imp <- caImportance(ranking,selectedArticles)
print("Importance summary: ", quote=FALSE)
print(imp)


library(reshape2)
molted=melt(ranking,id.vars=c())
molted
melted = molted[,c(2)]
melted = data.frame(melted)
sim= read.csv('xxx.csv', header=TRUE)
fix(sim)

simutil<-caLogit(s=sim,y= melted, x=selectedArticles)


print("Percentage participation of profiles: ", quote=FALSE)
print(simutil)
dim(selectedArticles)
sim = t(selectedArticles)


===






caPartUtilities(ranking, selectedArticles, z) ? function calculates matrix of individual levels utilities for respondents
(with intercept on first place)

caImportance(y=ranking, x=selectedArticles)
caTotalUtilities(y=ranking, x=selectedArticles)

segments <- caSegmentation(y=ranking, x=selectedArticles, c=2)
print(segments$seg)

caTotalUtilities(y=ranking, x=selectedArticles)

Conjoint(y=ranking, x=selectedArticles, z= levels, y.type= "rank")
ShowAllUtilities(y=ranking, x=selectedArticles, z= levels)



===============================================================================

library(rugarch)
library(conjoint)
library(reshape2)

# Part 1: Synthetic Profile Generation

# Feature values as specified
feature_values <- list()
feature_values[[1]] <- c("Strong", "Average")
feature_values[[2]] <- c("AFN 30 - 50","AFN 50 - 70 ", "AFN 70 - 90")
feature_values[[3]] <- c("100 - 200 ml", "200 - 350 ml")
feature_values[[4]] <- c("Opaque", "Pearliest", "Clear")
feature_values[[5]] <- c("High", "Medium")
feature_values[[6]] <- c("Dandruff", "Hair Fall", "Hair Texture")

# Generate all possible combinations of features
articles <- expand.grid(
  Brand = feature_values[[1]],
  Price = feature_values[[2]],
  Size = feature_values[[3]],
  Colour = feature_values[[4]],
  Foaming = feature_values[[5]],
  Remedy = feature_values[[6]]
)

set.seed(100)
maxNumberOfArticles = 12

# Selection of relevant concepts using fractional factorial design
selectedArticles <- caFactorialDesign(
  data = articles,
  type = 'fractional',
  cards = maxNumberOfArticles
)

# Display selected articles for administering
print(selectedArticles)

# Check if selected concepts are relevant for study
corrSelectedArticles <- caEncodedDesign(selectedArticles)
print(corrSelectedArticles)

# To show the selection is orthogonal
print(cor(corrSelectedArticles))

# Part 2: Conjoint Analysis

# Read the hypothetical ranking data
ranking <- read.csv("ranking.csv", header = TRUE)
levels <- read.csv("levels.csv", header = TRUE)
print(levels)
View(ranking)


# Conjoint analysis
c <- Conjoint(ranking, selectedArticles, unlist(feature_values))

# Calculate importance of features
imp <- caImportance(ranking, selectedArticles)
print("Importance summary: ", quote = FALSE)
print(imp)

# Reshape ranking data for simulation
molted <- melt(ranking, id.vars = c())
melted <- data.frame(molted[, c(2)])

# Read hypothetical simulation data
sim <- read.csv('xxx.csv', header = TRUE)
fix(sim)

# Logistic regression for utilities
simutil <- caLogit(s = sim, y = melted, x = selectedArticles)
print("Percentage participation of profiles: ", quote = FALSE)
print(simutil)

# Part 3: Additional Analysis

# Load journey data (example)
data(journey)
print(jpref)
print(jprof)
print(jlevn)
print(jsimp)

# Model and segmentation
caModel(y = ranking, x = selectedArticles, z = levels)
caPartUtilities(ranking, selectedArticles, z = levels)
caImportance(y = ranking, x = selectedArticles)
caTotalUtilities(y = ranking, x = selectedArticles)

# Segmentation
segments <- caSegmentation(y = ranking, x = selectedArticles, c = 2)
print(segments$seg)

# Total utilities
caTotalUtilities(y = ranking, x = selectedArticles)

# Conjoint analysis with utility display
Conjoint(y = ranking, x = selectedArticles, z = levels, y.type = "rank")
ShowAllUtilities(y = ranking, x = selectedArticles, z = levels)
======================================================

calculatePriceElasticity <- function(ranking, selectedArticles, levels, priceAttribute) {
  # Extract the part-worth utilities for the price attribute
  utilities <- caPartUtilities(ranking, selectedArticles, levels)
  
  # Assuming the price attribute is specified in the levels
  priceLevels <- levels[[priceAttribute]]
  priceUtilities <- utilities[, priceAttribute]
  
  # Calculate the average utility for each price level
  avgPriceUtilities <- colMeans(priceUtilities)
  
  # Calculate price elasticity
  # Elasticity = (dQ / dP) * (P / Q)
  # Assuming the change in price (dP) and the change in quantity (dQ) are based on the differences in utilities
  elasticity <- numeric(length(priceLevels) - 1)
  for (i in 2:length(priceLevels)) {
    dP <- priceLevels[i] - priceLevels[i - 1]
    dQ <- avgPriceUtilities[i] - avgPriceUtilities[i - 1]
    P <- priceLevels[i - 1]
    Q <- avgPriceUtilities[i - 1]
    elasticity[i - 1] <- (dQ / dP) * (P / Q)
  }
  
  return(elasticity)
}

# Example usage:
priceAttribute <- "price" # Replace with the actual name of the price attribute in your data
priceElasticity <- calculatePriceElasticity(ranking, selectedArticles, levels, priceAttribute)
print(priceElasticity)

xxxxxxxxxxxxxxxxxxxxxxxxx
calculatePriceElasticity <- function(ranking, selectedArticles, levels, priceAttribute) {
  # Extract the part-worth utilities for all attributes
  utilities <- caPartUtilities(ranking, selectedArticles, levels)
  
  # Print the structure of levels and utilities for debugging
  print("Levels structure:")
  print(str(levels))
  
  print("Utilities structure:")
  print(str(utilities))
  
  # Extract the levels and utilities for the price attribute
  priceLevels <- levels[[priceAttribute]]
  print("Price levels:")
  print(priceLevels)
  
  priceUtilities <- utilities[, priceAttribute, drop = FALSE]
  print("Price utilities:")
  print(priceUtilities)
  
  # Check dimensions to ensure they match
  if (length(priceLevels) != ncol(priceUtilities)) {
    stop("Number of price levels does not match number of utility columns for price attribute")
  }
  
  # Calculate the average utility for each price level
  avgPriceUtilities <- colMeans(priceUtilities, na.rm = TRUE)
  
  # Print average price utilities for debugging
  print("Average price utilities:")
  print(avgPriceUtilities)
  
  # Calculate price elasticity
  elasticity <- numeric(length(priceLevels) - 1)
  for (i in 2:length(priceLevels)) {
    dP <- priceLevels[i] - priceLevels[i - 1]
    dQ <- avgPriceUtilities[i] - avgPriceUtilities[i - 1]
    P <- priceLevels[i - 1]
    Q <- avgPriceUtilities[i - 1]
    elasticity[i - 1] <- (dQ / dP) * (P / Q)
  }
  
  return(elasticity)
}

# Example usage:
priceAttribute <- "Price" # Replace with the actual name of the price attribute in your data
priceElasticity <- calculatePriceElasticity(ranking, selectedArticles, levels, priceAttribute)
print(priceElasticity)

