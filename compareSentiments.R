# Author: Tom Kral
# About:  This script is for comparing sentiments
# Manual: The entire script can be called at once to perform the analyses. At least two files are needed

# Load data
setwd("/home/tom/Projects/LotR_Books_vs_movies")

bookData <- read.csv("BookData/sentimentResults_books.csv")
movieData <- read.csv("MovieData/sentimentResults_movies.csv")

# Combine data
combinedDatasets <- left_join(bookData, movieData, by = "Emotion")

