# Author: Tom Kral
# About:  This script is for analyzing a text/book in .txt format. 
# Manual: The entire script can be called at once to perform the analyses. As per R rules, all functions are generated first.
#         Operations are called at the bottom of the script.

################
# DEPENDENCIES #
################

#install.packages("RWeka") # install RWeka package
#install.packages("readr")
#install.packages("textdata")
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



#############
# FUNCTIONS #
#############

cleanText <- function(text){ # cleans text 
  
  text <- str_conv(text, "latin1") # change encoding
  text <- gsub("[[\\n\\r]]", "", text) # remove \n and \r made by read_fie
  text <- gsub("[[:punct:]]", "", text) # delete all punctuation
  text <- gsub("[[:digit:]]", "", text) # remove all numbers
  text <- tolower(text) # makes all characters lowercase
  text <- str_squish(text) # removes repeating whitespace
  
  return(text)
  
}

removeStopwords <- function(text){ # removes all English stopwords from a text
  
  textList <- unlist(str_split(text, " "))
  text_nsw <- textList[!(textList) %in% stop_words$word]
  text_cleaned <- paste(text_nsw, collapse = " ")
  
  return(text_cleaned)
  
}

printDiversity <- function(text){ # print the lexical diversity of a text
  
  wordList <- unlist(str_split(text, " "))
  totalWords <- length(wordList)
  uniqueWords <- length(unique(wordList))
  lexDiv <- uniqueWords / totalWords
  
  print(cat("Total words: ", totalWords, "\n Unique words: ", uniqueWords, "\n Lexical diversity: ", lexDiv, "\n"))
  
}



##################################
# MAIN FUNCTION TO ANAYZE A BOOK #
##################################

analyzeBook <- function(book){
  
  # Print some metrics
  printDiversity(book)
  
  # Clean the texts
  book_clean <- cleanText(book)
  book_clean <- removeStopwords(book_clean)
  
  print(book_clean)
  
  
}



#############
# LOAD DATA #
#############


# Load books as strings

setwd("/home/tom/Projects/LotR_Books_vs_movies/BookData")

#fotr <- read_file("01 - The Fellowship Of The Ring.txt")
#ttt <- read_file("02 - The Two Towers.txt")
#rotk <- read_file("03 - The Return Of The King.txt")

# load test file instead
test <- read_file("test.txt")

test_cleaned <- analyzeBook(test)


# Sentiment analysis

performSentimentAnalysis <- function(text){
  
  textList <- unlist(str_split(text, " "))
  df <- data.frame(matrix(unlist(textList), nrow = length(textList), byrow = TRUE), stringsAsFactors = FALSE)
  

}

fotrTokens <- performSentimentAnalysis(fotr_nsw)





