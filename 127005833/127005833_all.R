# setwd('127005833/')  # set working directory as the one containing the RData files
set.seed(0)

###########################################
# PROBLEM 1
###########################################


##### K Nearest Neighbor
load("class_data.RData") 
y = as.factor(y)
train = x
train$target = y

library(class)

# Parameter Tuning
require(caret)
trControl <- trainControl(method  = "cv", number  = 5)

fit <- train(target ~ .,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:10),
             trControl  = trControl,
             metric     = "Accuracy",
             data       = train)

best.k <- fit$bestTune$k

# Test Set Predictions
knn.ynew=knn(x,xnew,y,k=best.k)
knn.ynew=as.numeric(knn.ynew)-1

# Test Error Estimate
knn.te <- function(x, y, k, K=10){
  n = nrow(x) ### sample size
  folds = sample(1:K, n, replace=TRUE) ### create folds id for CV
  fold.error = rep(0, K) ### place holder to save error for each fold
  
  for (j in 1:K){#loop through 1 to K
    pred=knn(x[folds!=j,], x[folds==j,], y[folds!=j], k=k)
    fold.error[j] = sum(pred != y[folds==j])
  }
  return(sum(fold.error)/n)
}

# knn.test_error <- knn.te(x,y,best.k)
knn.test_error <- 1 - fit$results$Accuracy[best.k]

##### Logistic Regression
load("class_data.RData") 
y = as.factor(y)
train = x
train$target = y

library(glmnet)
glm.fit=glm(target~.,data=train,family=binomial)
summary(glm.fit)

# On Training Data
glm.probs=predict(glm.fit, newdata=x, type="response") #predict response probability 
glm.pred=ifelse(glm.probs>0.5, 1, 0)
y_pred = glm.pred
table(y, y_pred)

# Test Set Predictions
glm.probs=predict(glm.fit, newdata=xnew, type="response") #predict response probability 
lr.ynew=ifelse(glm.probs>0.5, 1, 0)
lr.ynew = as.numeric(lr.ynew)

# Test Error Estimate (through K-Fold CV)
library(boot)
lr.test_error = cv.glm(train, glm.fit, K=10)$delta[1]

##### Linear Discriminant Analysis
load("class_data.RData") 
y = as.factor(y)
train = x
train$target = y

library(MASS)
lda.fit=lda(target~.,data=train)
plot(lda.fit)

# Test Set Predictions
lda.ynew = predict(lda.fit, newdata = xnew)$class
lda.ynew = as.numeric(lda.ynew)-1

# Test Error Estimate (through LOOCV)
lda.loocvfit=lda(target~.,data=train, CV=TRUE)
table(lda.loocvfit$class, train$target)
lda.test_error = sum(lda.loocvfit$class != train$target)/length(y)

##### Support Vector Machine
load("class_data.RData") 
y = as.factor(y)

library(e1071)

# Function for estimating Test Error through K-Fold CV
svm.te <- function(x, y, cost=1, K=10){
  n = nrow(x) ### sample size
  folds = sample(1:K, n, replace=TRUE) ### create folds id for CV
  fold.error = rep(0, K) ### place holder to save error for each fold
  
  for (j in 1:K){#loop through 1 to K
    fit=svm(x[folds!=j,], y[folds!=j], kernel="linear", cost=cost, scale=FALSE)
    pred = predict(fit, newdata=x[folds==j,]) # make predictions for classification
    fold.error[j] = sum(pred != y[folds==j])
  }
  return(sum(fold.error)/n)
}

# Parameter Tuning
costs = c(1,4,6,8,10)
errors = rep(0, length(costs))
for (i in 1:length(costs)){
  errors[i] = svm.te(x,y,cost=costs[i])
}
best.cost = costs[which.min(errors)]

# Test Error Estimate
svm.test_error = svm.te(x, y, best.cost)

# Test Set Predictions
ynew = predict(svm(x,y,kernel="linear",cost=best.cost,scale=FALSE), newdata=xnew)
svm.ynew = as.numeric(ynew)-1

##### Random Forest
load("class_data.RData") 
y = as.factor(y)
train = x
train$target = y

# Parameter Tuning

# library(caret)
# control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
# tunegrid <- expand.grid(.mtry=c(sqrt(ncol(x))))
# 
# modellist <- list()
# for (ntree in c(300, 500, 1000)) {
#   fit <- train(target~., data=train, method="rf", tuneGrid=tunegrid, trControl=control, ntree=ntree)
#   key <- toString(ntree)
#   modellist[[key]] <- fit
# }
# 
# results <- resamples(modellist)
# summary(results)

library(randomForest)

ntrees = c(300,500,1000)
oob.err = rep(0,length(ntrees))
mtry = sqrt(ncol(x))

for(j in 1:length(ntrees)){
    fit=randomForest(target~., data=train, mtry=mtry, ntree=ntrees[j])
    oob.err[j]=fit$err.rate # Mean squared error for 400 trees
}

best.ntree = ntrees[which.min(oob.err)]

# Estimating Test Error
rf.test_error = min(oob.err)

# Test Set Predictions
rf.ynew = predict(fit, newdata=xnew ,type="response")
rf.ynew = as.numeric(rf.ynew)-1

##### Gradient Boosting Machine
load("class_data.RData") 
train = x
train$target = y

library(gbm)

# Parameter Tuning
gbm.model = gbm(target~.,data=train,distribution="bernoulli",n.trees=3000,shrinkage=0.01,interaction.depth=4,cv.fold=10,verbose=TRUE)
best.iter = gbm.perf(gbm.model, method="cv")
identical(which.min(gbm.model$cv.error), best.iter)

# On Training Data
y_pred.probs=predict(gbm.model, newdata=x, n.trees=best.iter, type="response")
y_pred=ifelse(y_pred.probs>0.5,1,0)
table(y_pred,y)

# Function for estimating Test Error through K-Fold CV
gbm.te <- function(x, y, n.trees=1000, K=10){
  n = nrow(x) ### sample size
  folds = sample(1:K, n, replace=TRUE) ### create folds id for CV
  fold.error = rep(0, K) ### place holder to save error for each fold
  
  for (j in 1:K){#loop through 1 to K
    fit=gbm.fit(x[folds!=j,], y[folds!=j], distribution="bernoulli",n.trees=n.trees,shrinkage=0.01,interaction.depth=4)
    pred = predict(fit, newdata=x[folds==j,], type="response") # make predictions for classification
    pred = ifelse(pred>0.5, 1, 0)
    fold.error[j] = sum(pred != y[folds==j])
    # cat(pred)
  }
  return(sum(fold.error)/n)
}

# Estimating Testing Error
gbm.test_error = gbm.te(x,y,n.trees = best.iter, K=10)

# Test Set Predictions
ynew.probs=predict(gbm.model, newdata=xnew, n.trees=best.iter, type="response")
gbm.ynew=ifelse(ynew.probs>0.5,1,0)


# OVERALL

models <- c('knn','lr','lda','svm','rf','gbm')
# models <- c('knn','lr')
errors <- rep(0, length(models))
predictions <- matrix(0, ncol=length(xnew[,1]), nrow=length(models))

for (i in 1:length(models)){
  errors[i] <- get(paste(models[i],'test_error',sep='.'))
  predictions[i,] <- get(paste(models[i],'ynew',sep='.'))
}

ynew <- as.integer(predictions[which.min(errors),])
test_error <- errors[which.min(errors)]

save(ynew,test_error,file="127005833.RData")

###########################################
# PROBLEM 2
###########################################

library(factoextra)
library(cluster)
library(ggplot2)
library(kselection)

load("cluster_data.RData") 
y.scaled <- scale(y)

# Elbow method (visually find knee and mark it in xintercept)
fviz_nbclust(y.scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(y.scaled, kmeans, method = "silhouette")+labs(subtitle = "Silhouette method")
# fviz_nbclust(y.scaled, hcut, method = "silhouette")+labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# fviz_nbclust(y.scaled, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+labs(subtitle = "Gap statistic method")

gap_stat <- clusGap(y.scaled, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

# Pham et. al. method
ks <- kselection(y.scaled, fun_cluster = stats::kmeans, max_centers = 15, k_threshold = 0.85, progressBar = TRUE, trace = TRUE)
num_clusters(ks)
which.min(ks$f_k)

