# Author: Tom Kral
# About:  This script is for analyzing a movie script in .txt format. 
# Manual: The entire script can be called at once to perform the analyses. As per R rules, all functions are generated first.
#         Operations are called at the bottom of the script.

################
# DEPENDENCIES #
################

#install.packages("RWeka") # install RWeka package
#install.packages("readr")
#install.packages("textdata")
#install.packages("dplyr")
#install.packages("ggplot2")
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
library(ggplot2)



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
  text <- gsub("merry", "", text) # remove the word "merry" from the text. This also the name of a Hobbit
  
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
  
  sentimentDataframe <- data_frame()
  
  for (row in 1:nrow(trilogy)) {
    
    tokens <- tibble(text = trilogy[row, "Content"]) %>% unnest_tokens(word, text)
    
    sentiments <- tokens %>%
      inner_join(get_sentiments("nrc"), "word") %>%
      count(word, sentiment, sort = TRUE)
    
    sentimentDataframe <- bind_rows(sentimentDataframe, sentiments)
    return(sentimentDataframe)
    
  }
  
}



##############
# OPERATIONS #
##############


# Load books as strings

setwd("/home/tom/Projects/LotR_Books_vs_movies/MovieData")

titles <- c("The Fellowship of the Ring", "The Two Towers", "The Return of the King")

fotr <- read_file("FellowshipOfTheRing.txt")
ttt <- read_file("TwoTowers.txt")
rotk <- read_file("ReturnOfTheKing.txt")

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


sentimentResults <- performSentimentAnalysis(trilogy)

# Plot the results
sentimentPlot <- ggplot(data=sentimentResults, aes(x=reorder(sentiment, -n, sum), y=n)) + 
  geom_bar(stat="identity", aes(fill=sentiment), show.legend=FALSE) +
  labs(x="Sentiment", y="Frequency") +
  theme_bw() 

sentimentPlot + labs(title = "Emotions in the The Lord of the Rings by Peter Jackson")


# Condense the data
results <- subset(sentimentResults, select = -word)
results <- aggregate(results$n, list(results$sentiment), FUN = sum)
colnames(results) <- c('Emotion', 'n_movie')

# Write results to file
write.csv(x=results, file="sentimentResults_movies.csv", row.names = FALSE)




