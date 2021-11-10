# Author: Tom Kral
# About:  This script is for comparing sentiments
# Manual: The entire script can be called at once to perform the analyses. At least two files are needed


library(ggplot2)


# Load data
setwd("/home/tom/Projects/LotR_Books_vs_movies")

bookData <- read.csv("BookData/sentimentResults_books.csv")
movieData <- read.csv("MovieData/sentimentResults_movies.csv")



# Scale sentiments
normalize <- function(x) {
  return((x- min(x)) / (max(x)-min(x)))
}

bookData$n_norm <- normalize(bookData$n)
movieData$n_norm <- normalize(movieData$n)


# Combine data
combinedDatasets <- rbind(bookData, movieData)


# Plot the results
sentimentPlot <- ggplot(combinedDatasets, aes(fill = origin, y = n_norm, x = emotion)) + 
  geom_bar(position = "dodge", stat = "identity")

sentimentPlot



