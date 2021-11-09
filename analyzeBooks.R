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
#install.packages("dplyr")
library(tidyverse) # data manipulation
library(tm) # text mining
library(tidytext) # text mining for word processing and sentiment analysis
library(reshape2) # reshapes a data frame
library(radarchart) # drawing the radar chart from a data frame
library(RWeka) # data mining tasks
library(knitr) # dynamic report generation
library(readr)
library(stringr)
library(dplyr)



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


performSentimentAnalysis <- function(trilogy){ # perform a sentiment analysis

  
  for (row in 1:nrow(trilogy)) {
    
    contents <- trilogy[row, "Content"]
    tokens <- tibble(text = contents) %>% unnest_tokens(word, text)
    
  }
  
}



##############
# OPERATIONS #
##############


# Load books as strings

setwd("/home/tom/Projects/LotR_Books_vs_movies/BookData")

titles <- c("The Fellowship of the Ring", "The Two Towers", "The Return of the King")

fotr <- read_file("01 - The Fellowship Of The Ring.txt")
ttt <- read_file("02 - The Two Towers.txt")
rotk <- read_file("03 - The Return Of The King.txt")

# load test file instead
#test <- read_file("test.txt")


# Clean text
fotr_c <- cleanText(fotr)
ttt_c <- cleanText(ttt)
rotk_c <- cleanText(rotk)

# remove stopwords
fotr_nsw <- removeStopwords(fotr_c)
ttt_nsw <- removeStopwords(ttt_c)
rotk_nsw <- removeStopwords(rotk_c)

# Merge all books into data frame
books <- c(fotr_nsw, ttt_nsw, rotk_nsw)

trilogy <- data.frame(titles, books, stringsAsFactors=FALSE)
names(trilogy) <- c("Title", "Content")


performSentimentAnalysis(trilogy)







