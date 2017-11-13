#load("data/3. TF_IDF.RData")

#===============================================
# SVD
#===============================================
# Time code execution
start.time = Sys.time()

# Perform SVD, reduce dimentionality down to 500 columns for 
# LSA
# Can use svd()
train.irlba = irlba(t(train.token.tfidf), nv = 300, maxit = 600)

# Check time
total.time = Sys.time() - start.time
total.time
#Time difference of 59.79384 mins

# Inspect new data features
#View(train.irlba$v)

sigma.inverse = 1/train.irlba$d
u.transpose = t(train.irlba$u)
document = train.token.tfidf[1,]
document.hat = sigma.inverse * u.transpose %*% document
# %*% is used to perform matrix multiplication

document.hat[1:10]
train.irlba$v[1:10]

# New dataframe with document semantic space of 300
train.svd = data.frame(Polarity = Train$Polarity, train.irlba$v)
dim(train.svd)
#[1] 789 301

head(train.svd,1)

# preparing Valid svd
valid.svd.raw = t(sigma.inverse*u.transpose %*% t(valid.token.tfidf))
valid.svd.raw

valid.svd = data.frame(Polarity = Validation$Polarity, valid.svd.raw)
dim(valid.svd)
#[1] 262 301

head(valid.svd,1)

#===============================================
# Model 1
#===============================================

# Add Sentiment Score to train svd
train.svd$SentimentScore = Train$SentimentScore

# Add Sentiment Score to valid svd
valid.svd$SentimentScore = Validation$SentimentScore

##
set.seed(1612)
cv.folds = createMultiFolds(Train$Polarity, k=10, times=3)

ctrlcv = trainControl(method = "repeatedcv", number = 10, repeats = 3,
                      index = cv.folds)

# Time code execution
start.time = Sys.time()

# Create a cluster to work on 3 logical cores
cl = makeCluster(3, type = "SOCK")
registerDoSNOW(cl)

set.seed(1612)
# for rf add importance = TRUE
rf.cv.1 = caret::train(Polarity ~ ., data = train.svd, method = "rf", 
                       trControl = ctrlcv, tuneLength = 7,
                       importance = TRUE)


# stop the cluster when processing is done
stopCluster(cl)

# Check time
total.time = Sys.time() - start.time
total.time
#Time difference of 15.22344 mins

rf.cv.1
# mtry Accuracy   Kappa
# 51   0.8149106  0.6273283

# Variable Importance
varImpPlot(rf.cv.1$finalModel)

#===============================================
# Prediction on vadition model
#===============================================

pred1 = predict(rf.cv.1,newdata = valid.svd)

confusionMatrix(valid.svd$Polarity,pred1,positive='Positive')

#            Reference
# Prediction Negative Positive
# Negative       93       28
# Positive       32      109
# Accuracy    : 0.771 
# Kappa       : 0.5404
# Sensitivity : 0.7956          
# Specificity : 0.7440

#save.image("data/4. SVD_rf.cv.1.RData")

