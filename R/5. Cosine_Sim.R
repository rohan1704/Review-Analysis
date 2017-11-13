#load("data/4. SVD_rf.cv.1.RData")

#===============================================
# COSINE SIMILARITY:
#===============================================
# Calculate cosine
# Removing first and last column
train.cosine = cosine(t(as.matrix(train.svd[,-c(1,ncol(train.svd))])))

# Indexes of all Negative reviews
neg.index = which(Train$Polarity == "Negative")

# Add new column and fill it with 0.0
train.svd$NegativeSimilarity = rep(0.0,nrow(train.svd))

for(i in 1:nrow(train.svd)){
  train.svd$NegativeSimilarity[i] = mean(train.cosine[i,neg.index])
}

# Calculate Validate similarity
valid.cosine = rbind(valid.svd.raw,train.irlba$v[neg.index,])

valid.cosine = cosine(t(valid.cosine))

valid.svd$NegativeSimilarity = rep(0.0,nrow(valid.svd))

neg.cols = (nrow(valid.svd) + 1) :ncol(valid.cosine)

for(i in 1:nrow(valid.cosine)){
  valid.svd$NegativeSimilarity = mean(train.cosine[i,neg.cols])
}

#===============================================
# Model 2
#===============================================
set.seed(1612)
cv.folds = createMultiFolds(Train$Polarity, k=10, times=5)

ctrlcv = trainControl(method = "repeatedcv", number = 10, repeats = 5,
                      index = cv.folds)

# Time code execution
start.time = Sys.time()

# Create a cluster to work on 3 logical cores
cl = makeCluster(3, type = "SOCK")
registerDoSNOW(cl)

set.seed(1612)
# for rf add importance = TRUE
rf.cv.2 = caret::train(Polarity ~., data = train.svd, method = "rf", 
                       trControl = ctrlcv, tuneLength = 7,
                       importance = TRUE)

# stop the cluster when processing is done
stopCluster(cl)

# Check time
total.time = Sys.time() - start.time
total.time
#Time difference of 24.47847 mins

rf.cv.2
# cp  Accuracy   Kappa
# 52  0.8281344  0.6536804

# Variable Importance
varImpPlot(rf.cv.2$finalModel)

#===============================================
# Prediction on vadition model
#===============================================

pred2 = predict(rf.cv.2,newdata = valid.svd)

confusionMatrix(valid.svd$Polarity,pred2,positive='Positive')

#           Reference
# Prediction Negative Positive
# Negative      103       18
# Positive       44       97

# Accuracy    : 0.7634
# Kappa       : 0.5311
# Sensitivity : 0.8435          
# Specificity : 0.7007

#save.image("data/5. Cosine_Sim_rf.cv.2.RData")

