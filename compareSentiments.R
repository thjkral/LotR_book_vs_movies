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

# Calculate differences in movie compared to book ( (v1*100/v2) = percentage) )
diff_pos <- combinedDatasets$n_norm[combinedDatasets$origin == 'movie' & combinedDatasets$emotion == 'positive'] - combinedDatasets$n_norm[combinedDatasets$origin == 'book' & combinedDatasets$emotion == 'positive']
diff_pos_perc <- round((diff_pos / combinedDatasets$n_norm[combinedDatasets$origin == 'book' & combinedDatasets$emotion == 'positive']) * 100, 1)

diff_neg <- combinedDatasets$n_norm[combinedDatasets$origin == 'movie' & combinedDatasets$emotion == 'negative'] - combinedDatasets$n_norm[combinedDatasets$origin == 'book' & combinedDatasets$emotion == 'negative']
diff_neg_perc <- round((diff_neg / combinedDatasets$n_norm[combinedDatasets$origin == 'book' & combinedDatasets$emotion == 'negative']) * 100, 1)

diff_fear <- combinedDatasets$n_norm[combinedDatasets$origin == 'movie' & combinedDatasets$emotion == 'fear'] - combinedDatasets$n_norm[combinedDatasets$origin == 'book' & combinedDatasets$emotion == 'fear']
diff_fear_perc <- round((diff_fear / combinedDatasets$n_norm[combinedDatasets$origin == 'book' & combinedDatasets$emotion == 'fear']) * 100, 1)

diff_trust <- combinedDatasets$n_norm[combinedDatasets$origin == 'movie' & combinedDatasets$emotion == 'trust'] - combinedDatasets$n_norm[combinedDatasets$origin == 'book' & combinedDatasets$emotion == 'trust']
diff_trust_perc <- round((diff_trust / combinedDatasets$n_norm[combinedDatasets$origin == 'book' & combinedDatasets$emotion == 'trust']) * 100, 1)

diff_sad <- combinedDatasets$n_norm[combinedDatasets$origin == 'movie' & combinedDatasets$emotion == 'sadness'] - combinedDatasets$n_norm[combinedDatasets$origin == 'book' & combinedDatasets$emotion == 'sadness']
diff_sad_perc <- round((diff_sad / combinedDatasets$n_norm[combinedDatasets$origin == 'book' & combinedDatasets$emotion == 'sadness']) * 100, 1)

diff_ant <- combinedDatasets$n_norm[combinedDatasets$origin == 'movie' & combinedDatasets$emotion == 'anticipation'] - combinedDatasets$n_norm[combinedDatasets$origin == 'book' & combinedDatasets$emotion == 'anticipation']
diff_ant_perc <- round((diff_ant / combinedDatasets$n_norm[combinedDatasets$origin == 'book' & combinedDatasets$emotion == 'anticipation']) * 100, 1)

diff_joy <- combinedDatasets$n_norm[combinedDatasets$origin == 'movie' & combinedDatasets$emotion == 'joy'] - combinedDatasets$n_norm[combinedDatasets$origin == 'book' & combinedDatasets$emotion == 'joy']
diff_joy_perc <- round((diff_joy / combinedDatasets$n_norm[combinedDatasets$origin == 'book' & combinedDatasets$emotion == 'joy']) * 100, 1)

diff_anger <- combinedDatasets$n_norm[combinedDatasets$origin == 'movie' & combinedDatasets$emotion == 'anger'] - combinedDatasets$n_norm[combinedDatasets$origin == 'book' & combinedDatasets$emotion == 'anger']
diff_anger_perc <- round((diff_anger / combinedDatasets$n_norm[combinedDatasets$origin == 'book' & combinedDatasets$emotion == 'anger']) * 100, 1)

diff_surp <- combinedDatasets$n_norm[combinedDatasets$origin == 'movie' & combinedDatasets$emotion == 'surprise'] - combinedDatasets$n_norm[combinedDatasets$origin == 'book' & combinedDatasets$emotion == 'surprise']
diff_surp_perc <- round((diff_surp / combinedDatasets$n_norm[combinedDatasets$origin == 'book' & combinedDatasets$emotion == 'surprise']) * 100, 1)

diff_dis <- combinedDatasets$n_norm[combinedDatasets$origin == 'movie' & combinedDatasets$emotion == 'disgust'] - combinedDatasets$n_norm[combinedDatasets$origin == 'book' & combinedDatasets$emotion == 'disgust']
diff_dis_perc <- round((diff_dis / combinedDatasets$n_norm[combinedDatasets$origin == 'book' & combinedDatasets$emotion == 'disgust']) * 100, 1)

differences <- c(diff_pos_perc, diff_neg_perc, diff_fear_perc, diff_trust_perc, diff_sad_perc, diff_ant_perc, diff_joy_perc, diff_anger_perc, diff_surp_perc, 0)
print(differences)

# Plot the results

combinedDatasets$emotion = factor(combinedDatasets$emotion, levels = c('positive', 'negative', 'fear', 'trust', 'sadness', 'anticipation', 'joy', 'anger', 'surprise', 'disgust'))

sentimentPlot <- ggplot(combinedDatasets, aes(fill = origin, y = n_norm, x = emotion)) + 
  geom_bar(position = "dodge", stat = "identity") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

sentimentPlot + labs(title = "Sentiment in The Lord of the Rings - Comparison between books and movies", x = "Sentiment and emotion", y = "Frequency", fill = "Medium")




