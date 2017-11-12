load("data/1. Start.RData")

#===============================================
# Divide data 
#===============================================
# 60:20:20
# 60 for Training
# 20 for Validation
# 20 for Testing
set.seed(1612)
indexes = createDataPartition(FullData$Polarity,
                              times = 1,
                              p = 0.6,
                              list = FALSE)
Train = FullData[indexes,]
Temp = FullData[-indexes,]

set.seed(1612)
indexes = createDataPartition(Temp$Polarity,
                              times = 1,
                              p = 0.5,
                              list = FALSE)
Validation = Temp[indexes,]
Test = Temp[-indexes,]

rm(Temp)

prop.table(table(Train$Polarity))
# Negative  Positive 
# 0.4626109 0.5373891
nrow(Train) # 789

prop.table(table(Validation$Polarity))
# Negative  Positive 
# 0.4618321 0.5381679
nrow(Validation) # 262

prop.table(table(Test$Polarity))
# Negative  Positive 
# 0.4618321 0.5381679
nrow(Test) # 262

#===============================================
# Pre-Processing: Train and Validation Set
#===============================================
# Tokenization
train.token = tokens(Train$Review,what = "word",
                     removeNumbers = TRUE, removePunct = TRUE,
                     removeSymbols = TRUE, removeHyphens = TRUE)
train.token[[6]]

valid.token = tokens(Validation$Review,what = "word",
                     removeNumbers = TRUE, removePunct = TRUE,
                     removeSymbols = TRUE, removeHyphens = TRUE)

valid.token[[6]]

# Lower case
train.token = tokens_tolower(train.token)
train.token[[6]]

valid.token = tokens_tolower(valid.token)
valid.token[[6]]

# Stop words
train.token = tokens_select(train.token,stopwords(),
                            selection = "remove")
train.token[[6]]

valid.token = tokens_select(valid.token,stopwords(),
                            selection = "remove")
valid.token[[6]]

# Perform stemming
train.token = tokens_wordstem(train.token,language = "english")
train.token[[6]]

valid.token = tokens_wordstem(valid.token,language = "english")
valid.token[[6]]

# Add bigrams to feature matrix. 
train.token = tokens_ngrams(train.token, n = 1:2)
train.token[[6]]

valid.token = tokens_ngrams(valid.token, n = 1:2)
valid.token[[6]]

# Bag-of-words models
train.token.dfm = dfm(train.token)
train.token.dfm
#Document-feature matrix of: 789 documents, 72,557 features (99.7% sparse).

valid.token.dfm = dfm(valid.token)
valid.token.dfm
#Document-feature matrix of: 262 documents, 28,481 features (99.3% sparse).

# Validation set should have same number of features as Train set
valid.token.dfm = dfm_select(valid.token.dfm, features = train.token.dfm)
valid.token.dfm
#Document-feature matrix of: 262 documents, 72,557 features (99.8% sparse).


# Tranform to a matrix & inspect
train.token.matrix = as.matrix(train.token.dfm)
dim(train.token.matrix)
# [1] 789 72557

valid.token.matrix = as.matrix(valid.token.dfm)
dim(valid.token.matrix)
# [1] 262 72557

## Setup a feature data frame with label:
train.token.df = cbind(Polarity = Train$Polarity,
                       as.data.frame(train.token.dfm))

valid.token.df = cbind(Polarity = Validation$Polarity,
                       as.data.frame(valid.token.dfm))

## Clean Column names
names(train.token.df) = make.names(names(train.token.df))

names(valid.token.df) = make.names(names(valid.token.df))

# Save image file
save.image("data/2. Divide_Preprocess.RData")

