setwd("G:/GIT/Review Analysis")

# Libraries
library(ggplot2) # Plotting
library(e1071)
library(caret)
library(Matrix)
library(tm)
library(quanteda) # Text Analytics
library(irlba) # Singular Value Decomposition
library(randomForest) # Random Forest
library(doSNOW) # Parallel processing
library(qdap) # Polarity
library(qdapDictionaries)
library(lsa) #
library(stringr)
library(xgboost) # XGBoost


#==================================================
# LOAD FILE:
#==================================================
# Reading all files scraped from Web scraping script.

Arrow = read.csv("files/Arrow_2017-11-11.csv", stringsAsFactors = FALSE)

Constantine = read.csv("files/Constantine_2017-11-11.csv", stringsAsFactors = FALSE)

LegendsofTomorrow = read.csv("files/Legends of Tomorrow_2017-11-11.csv", stringsAsFactors = FALSE)

Lucifer = read.csv("files/Lucifer_2017-11-11.csv", stringsAsFactors = FALSE)

Supergirl = read.csv("files/Supergirl_2017-11-11.csv", stringsAsFactors = FALSE)

TheFlash = read.csv("files/The Flash_2017-11-11.csv", stringsAsFactors = FALSE)

# Combining all files in a single data frame
FullData = rbind(Arrow,Constantine,LegendsofTomorrow,Lucifer,Supergirl,TheFlash)

str(FullData)
# 1313 obs. of  4 variables

# Converting Show as factor
FullData$Show = as.factor(FullData$Show)

# Check for NA data
which(is.na(FullData) == TRUE)
# integer(0)

summary(FullData$Rating)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   3.000   6.000   6.018   9.000  10.000

# Overall Distribution of Ratings
ggplot(data = FullData,aes(x= Rating,fill = factor(Rating))) +
  geom_bar(stat = "count") +
  scale_x_continuous(breaks = seq(1,10,1)) +
  labs(x = "Ratings of all shows",
       y = "Count of Ratings",
       title = "Overall Distribution of Ratings",
       fill = "Rating")
ggsave("figures/1. Overall Distribution of Ratings.png", units="in", width=9, height=4.5, dpi=300)

# Distribution of Ratings by Show
ggplot(data = FullData,aes(x = Rating ,fill = factor(Rating))) +
  geom_bar(stat = "count") +
  facet_grid(~Show) +
  scale_x_continuous(breaks = seq(1,10,1)) +
  labs(x = "Ratings",
       y = "Count of Ratings",
       title = "Distribution of Ratings by Show",
       fill = "Rating")
ggsave("figures/2. Distribution of Ratings by Show.png", units="in", width=9, height=4.5, dpi=300)

# Count of Ratings by Show
ggplot(data = FullData,aes(x = Show,fill = factor(Rating))) +
  geom_bar(stat = "count") +
  labs(x = "Ratings",
       y = "Count of Ratings",
       title = "Count of Ratings by Show",
       fill = "Shows")
ggsave("figures/3. Count of Ratings by Show.png", units="in", width=9, height=4.5, dpi=300)


#===============================================
# SENTIMENT SCORE:
#===============================================
# Removing period from each review
for(i in 1:nrow(FullData)){
  FullData$Review[i] = gsub("\\."," ",FullData$Review[i])
}

# Polarity
df = polarity(strip(FullData$Review), grouping.var = 4,
              polarity.frame = qdapDictionaries::key.pol, constrain = FALSE,
              negators = qdapDictionaries::negation.words,
              amplifiers = qdapDictionaries::amplification.words,
              deamplifiers = qdapDictionaries::deamplification.words,
              question.weight = 0, amplifier.weight = 0.8, n.before = 4,
              n.after = 2, rm.incomplete = FALSE, digits = 3)["all"]

FullData$SentimentScore = round(df$all$polarity,digits = 1)

write.csv(FullData,"results/1. Review_Data.csv",row.names = FALSE)

summary(FullData$SentimentScore)
#    Min. 1st Qu.  Median    Mean  3rd Qu.    Max. 
# -1.8000 -0.2000  0.2000  0.1687  0.5000  1.8000

#==================================================
# NEW RATING POINTS:
#==================================================
# A sentiment analysis is a process that computationally identifies and categorizes
# opinions expressed in a text. However, the main focus is to determine whether the
# writer's attitude towards a particular topic, product, etc., is positive, 
# negative or neutral.
# http://deemly.co/blog/sentiment-analysis-value-numeric-ratings-review-systems/

# Merging Sentiment score with Rating
# If a review is positive, the same positiveness will reflect in sentiment score
# Same goes for negative reviews.
# These sentiment score can reduce or increase rating values.

Rating = FullData$Rating
Senti = FullData$SentimentScore

# Set Lower and Upper Limit for NRP between 1 - 10
for(i in 1:nrow(FullData)){
  FullData$NewRatingPoints[i] = Rating[i] + Senti[i]
  FullData$NewRatingPoints[i] = round(FullData$NewRatingPoints[i],digits = 1)
  if(FullData$NewRatingPoints[i] <= 1){
    FullData$NewRatingPoints[i] = 1
  }
  if(FullData$NewRatingPoints[i] >= 10){
    FullData$NewRatingPoints[i] = 10
  }
}

# Remove temp variable
rm(Rating,Senti)

summary(FullData$NewRatingPoints)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   2.700   6.700   6.119   9.600  10.000 

summary(FullData$Rating)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   3.000   6.000   6.018   9.000  10.000 

# Not much diffenent but even minor changes is valuable.

#===============================================
# POLARITY:
#===============================================
# Since Median and Mean is 6.000 and 6.018 respectively
# Labeling values below 6.0 as Negative and value greater than 6.0 as Positive

for(i in 1:nrow(FullData)){
  if(FullData$NewRatingPoints[i] >= 0 & FullData$NewRatingPoints[i] <= 6.0){
    FullData$Polarity[i] = "Negative"
  }else if(FullData$NewRatingPoints[i] > 6.0 & FullData$NewRatingPoints[i] <= 10){
    FullData$Polarity[i] = "Positive"
  }else{
    FullData$Polarity[i] = "ERROR"
  }
}

table(FullData$Polarity)
# Negative Positive 
#      607      706

write.csv(FullData,"results/2. Review_Data_Polarity.csv",row.names = FALSE)

# Save image file
#save.image("data/1. Start.RData")
