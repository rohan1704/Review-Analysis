#load("data/5. Cosine_Sim_rf.cv.2.RData")

#===============================================
# Preparing Test set for final prediction on 
#===============================================
# Preparing Test set
# Tokenization
test.token = tokens(Test$Review,what = "word",
                    removeNumbers = TRUE, removePunct = TRUE,
                    removeSymbols = TRUE, removeHyphens = TRUE)

test.token[[6]]

# Lower case
test.token = tokens_tolower(test.token)

test.token[[6]]

# Stop words
test.token = tokens_select(test.token,stopwords(),
                           selection = "remove")

test.token[[6]]

# Perform stemming
test.token = tokens_wordstem(test.token,language = "english")

test.token[[6]]

# Add bigrams to feature matrix. 
test.token = tokens_ngrams(test.token, n = 1:2)

test.token[[6]]

# Bag-of-words models
test.token.dfm = dfm(test.token)
#View(test.token.dfm)

dim(test.token.dfm)
# [1]  262 28119

# Explore train and test set dfm
train.token.dfm
#Document-feature matrix of: 789 documents, 72,557 features (99.7% sparse).
test.token.dfm
#Document-feature matrix of: 262 documents, 28,119 features (99.3% sparse).


# Validation set should have same number of features as Train set
test.token.dfm = dfm_select(test.token.dfm, features = train.token.dfm)
test.token.dfm
#Document-feature matrix of: 262 documents, 72,557 features (99.8% sparse).

# Tranform to a matrix & inspect
test.token.matrix = as.matrix(test.token.dfm)

#View(test.token.matrix)
dim(test.token.matrix)
# [1] 262 72557

## Setup a feature data frame with label:
test.token.df = cbind(Polarity = Test$Polarity,
                      as.data.frame(test.token.dfm))

## Clean Column names
names(test.token.df) = make.names(names(test.token.df))


# Normalize all documents via TF
test.token.df = apply(test.token.matrix, 1 , termfrequency)
str(test.token.df)

# Claculate TF-IDF for testing set
# for idf use train.token.idf
test.token.tfidf = apply(test.token.df, 2 , tfidf,idf = train.token.idf)

dim(test.token.tfidf)
# [1] 72557   262

#View(test.token.tfidf[1:25,1:25])

# Transpose the matrix
test.token.tfidf = t(test.token.tfidf)

# Convert to data frame
test.token.df = as.data.frame(test.token.tfidf)

# Fix incomplete data
incomplete.cases =  which(!complete.cases(test.token.df))
incomplete.cases 
# integer(0)

Test$Review[incomplete.cases]
# character(0)

# Fix incomplete cases
#test.token.tfidf[incomplete.cases,] = rep(0.0,ncol(test.token.tfidf))

dim(test.token.tfidf)
# [1]  262 72557

sum(which(!complete.cases(valid.token.tfidf)))
# [1] 0

# test svd
test.svd.raw = t(sigma.inverse * u.transpose %*% t(test.token.tfidf))

# Add Label
test.svd = data.frame(Polarity = Test$Polarity, test.svd.raw)

# Calculate similarity
test.cosine = rbind(test.svd.raw,train.irlba$v[neg.index,])

test.cosine = cosine(t(test.cosine))

test.svd$NegativeSimilarity = rep(0.0,nrow(test.svd))

neg.cols = (nrow(test.svd)+1):ncol(test.cosine)


for(i in 1:nrow(test.svd)){
  test.svd$NegativeSimilarity[i] = mean(train.cosine[i,neg.cols])
}


# Add Sentiment Score to test svd
test.svd$SentimentScore = Test$SentimentScore

#===============================================
# Make final prediction on test set
#===============================================
# Using rf.cv.1

predFinal = predict(rf.cv.1,newdata = test.svd)

confusionMatrix(test.svd$Polarity,predFinal,positive = "Positive")

#          Reference
# Prediction Negative Positive
# Negative       92       29
# Positive       13      128
# Accuracy    : 0.8397
# Kappa       : 0.6745
# Sensitivity : 0.8153         
# Specificity : 0.8762


# Using rf.cv.2

predFinal = predict(rf.cv.2,newdata = test.svd)

confusionMatrix(test.svd$Polarity,predFinal,positive = "Positive")

#            Reference
# Prediction Negative Positive
# Negative       78       43
# Positive        9      132
# Accuracy    : 0.8015
# Kappa       : 0.5926 
# Sensitivity : 0.7543         
# Specificity : 0.8966


