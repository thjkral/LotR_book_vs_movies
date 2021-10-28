setwd("/home/tom/Projects/LotR Books_vs_movies/BookData")

# Load libraries
#install.packages("RWeka") # install RWeka package
#install.packages("readr")
library(tidyverse) # data manipulation
library(tm) # text mining
library(wordcloud) # word cloud generator
library(wordcloud2) # word cloud generator
library(tidytext) # text mining for word processing and sentiment analysis
library(reshape2) # reshapes a data frame
library(radarchart) # drawing the radar chart from a data frame
library(RWeka) # data mining tasks
library(knitr) # dynamic report generation
library(readr)
library(stringr)


# load data

#fotr <- read_file("01 - The Fellowship Of The Ring.txt")
#ttt <- read_file("02 - The Two Towers.txt")
#rotk <- read_file("03 - The Return Of The King.txt")
fotr <- read_file("test.txt")


cleanText <- function(text){
  
  text <- gsub("[[\\n\\r]]", "", text)
  text <- gsub("[[:punct:]]", "", text)
  text <- gsub("[[:digit:]]", "", text)
  text <- tolower(text)
  text <- str_squish(text)
  
  return(text)
  
}

printDiversity <- function(text){
  
  wordList <- unlist(str_split(text, " "))
  totalWords <- length(wordList)
  uniqueWords <- length(unique(wordList))
  lexDiv <- uniqueWords / totalWords
  
  print(cat("Total words: ", totalWords, "\n Unique words: ", uniqueWords, "\n Lexical diversity: ", lexDiv, "\n"))
  
}


fotr <- cleanText(fotr)

printDiversity(fotr)


# Sentiment analysis




