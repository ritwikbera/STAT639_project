##### Gradient Boosting Machine
# setwd('127005833/')  # set working directory as the one containing the RData files
set.seed(0)
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
test_error = gbm.te(x,y,n.trees = best.iter, K=10)

# Test Set Predictions
ynew.probs=predict(gbm.model, newdata=xnew, n.trees=best.iter, type="response")
ynew=ifelse(ynew.probs>0.5,1,0)
ynew = as.integer(ynew)
save(ynew, test_error, file='127005833.RData')




