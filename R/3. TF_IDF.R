#load("data/2. Divide_Preprocess.RData")

#===============================================
# TF - IDF
#===============================================
# Function to calculate TF
termfrequency = function(row){
  row/sum(row)
}

# Function to calculate IDF
inverse.doc.freq = function(col){
  corpus.size = length(col)
  doc.count = length(which(col>0))
  log10(corpus.size/doc.count)
}

# Function to calculate TF-IDF
tfidf = function(tf,idf){
  tf*idf
}

## Step 1 Calculate TF
train.token.df = apply(train.token.matrix, 1 , termfrequency)
valid.token.df = apply(valid.token.matrix, 1 , termfrequency)

## Step 2 Calculate IDF
train.token.idf = apply(train.token.matrix, 2 , inverse.doc.freq)

## Step 3 Calculate TF-IDF
train.token.tfidf = apply(train.token.df, 2 , tfidf, idf = train.token.idf)
dim(train.token.tfidf)
# [1] 72557   789

valid.token.tfidf = apply(valid.token.df, 2 , tfidf, idf = train.token.idf)
dim(valid.token.tfidf)
# [1] 72557   262

## Transpose the matrix
train.token.tfidf = t(train.token.tfidf)
dim(train.token.tfidf)
# [1]  789 72557

valid.token.tfidf = t(valid.token.tfidf)
dim(valid.token.tfidf)
# [1]  262 72557

summary(valid.token.tfidf[1,])
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.000e+00 0.000e+00 0.000e+00 1.922e-05 0.000e+00 2.062e-02

# Convert to data frame
train.token.df = as.data.frame(train.token.tfidf)
#View(train.token.df)

valid.token.df = as.data.frame(valid.token.tfidf)
#View(valid.token.df)

# Check for incomplete cases
## Check for Train set
incomplete.cases =  which(!complete.cases(train.token.df))
incomplete.cases 
# integer(0)

Train$Review[incomplete.cases]
# character(0)

# Fix incomplete cases
#train.token.tfidf[incomplete.cases,] = rep(0.0,ncol(train.token.tfidf))

dim(train.token.tfidf)
# [1]  789 72557

sum(which(!complete.cases(train.token.tfidf)))
# [1] 0

## Check for Valid set
incomplete.cases =  which(!complete.cases(valid.token.df))
incomplete.cases 
# integer(0)

Validation$Review[incomplete.cases]
# character(0)

# Fix incomplete cases
#valid.token.tfidf[incomplete.cases,] = rep(0.0,ncol(valid.token.tfidf))

dim(valid.token.tfidf)
# [1]  262 72557

sum(which(!complete.cases(valid.token.tfidf)))
# [1] 0

## Setup a feature data frame with label:
train.token.df = cbind(Polarity = Train$Polarity,
                       as.data.frame(train.token.tfidf))
dim(train.token.df)
# [1] 789 72558

valid.token.df = cbind(Polarity = Validation$Polarity,
                       as.data.frame(valid.token.tfidf))
dim(valid.token.df)
# [1] 262 72558

## Clean Column names
names(train.token.df) = make.names(names(train.token.df))
names(valid.token.df) = make.names(names(valid.token.df))

# Save image file
#save.image("data/3. TF_IDF.RData")

