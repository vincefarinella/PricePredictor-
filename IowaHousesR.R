library(caret)

mydata <- read.table("train.csv", header=TRUE, 
                     sep=",")
summary(mydata)
names(mydata)

#MULTIPLE LINEAR REGRESSION
lm.fit=lm(SalePrice~OverallQual + GrLivArea + GarageCars + GarageArea + TotalBsmtSF + FirstFlrSF + FullBath + TotRmsAbvGrd + YearBuilt, data = mydata)
summary(lm.fit)

DF1 <- read.table("DF1.csv", header=TRUE,   
                  sep=",")

outputLM=predict(lm.fit, newdata=DF1)

print(outputLM)
saveRDS(lm.fit, "./LM_fit.rds")









# GAM MODEL
library(gam)
library(splines)
library(ggplot2)
attach(mydata)

gam1 = lm(SalePrice~ns(OverallQual,4) + ns(GrLivArea,4), data = mydata)
summary(gam1)

plot(SalePrice, OverallQual, main="Plot", 
     xlab="SalePrice", ylab="OverallQual", pch=19)
abline(lm(OverallQual~SalePrice), col="red") # regression line (y~x) 
lines(lowess(SalePrice,OverallQual), col="blue") # lowess line (x,y)

## CREATE TOTAL SQ FT (same as GrLiveArea?)
mydata$TotalSqFt <- (mydata$FirstFlrSF + mydata$SecondFlrSF)
summary(mydata$TotalSqFt)

sum(is.na(mydata$SalePrice))

## SUBSET SELECTION
library(leaps)
regfit.full=regsubsets(SalePrice~OverallQual + GrLivArea + GarageCars + GarageArea + TotalBsmtSF + FirstFlrSF + FullBath + TotRmsAbvGrd + YearBuilt, data = mydata)
summary(regfit.full)
reg.summary=summary(regfit.full)
reg.summary$rsq
reg.summary$adjr2
par(mfrow=c(2,2))
plot(reg.summary$rsq,xlab = "number of variables",ylab = "RSS",type = "l")
plot(reg.summary$adjr2,xlab = "number of variables",ylab = "adjusted r2",type = "l")
which.max((reg.summary$adjr2))
which.max((reg.summary$rsq))
plot(reg.summary$cp,xlab = "number of variables",ylab = "cp",type = "l")
which.min((reg.summary$cp))
which.max((reg.summary$bic))
plot(regfit.full,scale = "r2")
par("mar")
par(mar=c(1,1,1,1))

## RIDGE REGRESSION
x = model.matrix(SalePrice~OverallQual + GrLivArea + GarageCars + GarageArea + TotalBsmtSF + FirstFlrSF + FullBath + TotRmsAbvGrd + YearBuilt, data = mydata)[,-1]
y = mydata$SalePrice

library(glmnet)
grid = 10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha = 0, lambda = grid)
dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))
              #Splitting dataset into train / test
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]
ridge.mod=glmnet(x[train,] ,y[train],alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)
mean((mean(y[train])-y.test)^2)
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T) #ERROR ??
mean((ridge.pred-y.test)^2)

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
out=glmnet(x,y,alpha=0)
predict(out,type = "coefficients",s=bestlam)[1:10,] #max is 10. wont let me do 20.

DF1 <- read.table("DF1.csv", header=TRUE,   
                  sep=",")

outputRR=predict(ridge.mod, newdata=DF1)

print(outputRR)




#LASSO

lasso.mod=glmnet(x[train,] ,y[train], alpha=1, lambda=grid)
plot(lass.mod)

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)-2)

out=glmnet(x,y,alpha=1, lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:10,]
lasso.coef
lasso.coef[lasso.coef!=0]

#FITTING REGRESSION TREES
library(MASS)
library(ISLR)
library(tree)
set.seed(1)
tree.houses=tree(SalePrice~OverallQual + GrLivArea + GarageCars + GarageArea + TotalBsmtSF + FirstFlrSF + FullBath + TotRmsAbvGrd + YearBuilt, data = mydata)
#BAGGING and RANDOM FOREST

TestData <- read.table("test.csv", header=TRUE,   
                     sep=",")

houses.test = TestData["SalePrice"]  ####??????

library(randomForest)
set.seed(1)
bag.houses=randomForest(SalePrice~OverallQual + GrLivArea + GarageCars + GarageArea + TotalBsmtSF + FirstFlrSF + FullBath + TotRmsAbvGrd + YearBuilt, data = mydata,
                        subset=train, mtry=9,importance=TRUE)
bag.houses

yhat.bag = predict(bag.houses,newdata = mydata[-train,])
plot(yhat.bag, houses.test)  ## houses.test not defined????
abline(0,1)
mean((yhat.bag-houses.test)^2)

bag.houses=randomForest(SalePrice~OverallQual + GrLivArea + GarageCars + GarageArea + TotalBsmtSF + FirstFlrSF + FullBath + TotRmsAbvGrd + YearBuilt, data = mydata,
                        subset=train, mtry=9, ntree=25)
yhat.bag = predict(bag.houses,newdata = mydata[-train,])
mean((yhat.bag-houses.test)^2)

set.seed(1)
rf.houses=randomForest(SalePrice~OverallQual + GrLivArea + GarageCars + GarageArea + TotalBsmtSF + FirstFlrSF + FullBath + TotRmsAbvGrd + YearBuilt, data = mydata,
                       subset=train,mytry=6)
yhat.rf = predict(rf.houses,newdata=mydata[-train,])
mean((yhat.rf-houses.test)^2)
importance(rf.houses)

##need to figure out houses.test

## RANDOM FOREST BAGGING K-FOLD CV

maxnumpreds <- 9 #slightly greater than sqrt(13) - random forest
maxnumtrees <- 300 ## an arbitrarily-chosen value. Experiment by changing
matMSEs <- matrix(nrow=maxnumpreds, ncol=maxnumtrees, byrow=TRUE) # matrix to hold MSE values

# k-fold CV for estimating overall MSE
samplesize <- 9 # fetch the number of rows
numfolds <- 5 # we'll use 5-fold CV

quotient <- samplesize %/% numfolds # e.g., 10 %/% 3 = 3
remainder <- samplesize %% numfolds # e.g., 10 %% 3 = 1

# create the sizes of each "fold" subsample
lstsizes <- rep(quotient,numfolds)
if (remainder > 0) {
  for (i in 1:remainder){
    lstsizes[i] <- lstsizes[i]+1
  }
}

seedval <- 100
for(numpreds in 1 : maxnumpreds){
  for(numtrees in 1 : maxnumtrees) {
    vctkfoldMSEs <- numeric(numfolds)
    start <- 1
    for(kn in 1:length(lstsizes)){
      end <- start + lstsizes[kn] - 1
      ## note that the randomForest will need a set of indices
      ## to indicate which rows in the dataset to use for
      ## fitting a model
      trainsubset <- seq(1:samplesize)[-(start:end)]
      testsubset <- seq(1:samplesize)[start:end]
      ## build the model
      set.seed(seedval) ## needed to ensure replicability
      model.bagged <- randomForest(SalePrice~OverallQual + GrLivArea + GarageCars + GarageArea + TotalBsmtSF + FirstFlrSF + FullBath + TotRmsAbvGrd + YearBuilt, # specify the model via formula - all variables included as predictors
                                   data=mydata, # specify dataset
                                   subset=trainsubset, #specify which subset of the dataset to use for fitting
                                   mtry=numpreds, # number of predictors to consider at each fitting step
                                   ntree=numtrees, # number of trees to consider at each fitting step
                                   importance=TRUE) #specify that importance values are needed
      
      ## make predictions on the holdout (test) subset of the data
      ## note that for prediction we need to pass actual rows
      ## from the dataset. This is done by using the specific
      ## rows from the dataset that correspond to the testing
      ## subset.
      pred.vals.bagged <- predict(model.bagged,
                                  newdata=mydata[testsubset,])
      testvals <- mydata$SalePrice[testsubset]
      
      vctkfoldMSEs[kn] <- mean((pred.vals.bagged - testvals)^2) ## compute the MSE and save it 
    }
    
    matMSEs[numpreds, numtrees] <- mean(vctkfoldMSEs) ## store the MSE for the current combination of no. preds X no. of trees.
    ## display information that indicates # of iterations completed
    ##print(paste("     Processed trees:",numtrees))
    ## print(importance(model.bagged))
  }
  print(paste("Processed predictors:", numpreds))
}

## identify the best value via look-up using the which() function
loc <- which(matMSEs == min(matMSEs), arr.ind=TRUE)

## the location is returned in the form of a vector with two values
## the first one indicates the row number and the second one the column
## number, thus indicating the optimal number of predictors and
## trees, respectively.
print(paste("The optimal configuration is:", loc[1], "predictor(s) and",
            loc[2], "tree(s)."))

print(paste("The lowest MSE is:", min(matMSEs)))

set.seed(seedval) ## needed to ensure replicability
optimal.model <- randomForest(SalePrice~OverallQual + GrLivArea + GarageCars + GarageArea + TotalBsmtSF + FirstFlrSF + FullBath + TotRmsAbvGrd + YearBuilt, data=mydata, #subset=train,
                              mtry=loc[1],  ntree=loc[2],
                              importance=TRUE)
print(importance(optimal.model))

png(filename="importance-stats.png")
varImpPlot(optimal.model)
dev.off()

#saving to disk
saveRDS(optimal.model, "./final_model.rds")


## viewing results

DF1 <- read.table("DF1.csv", header=TRUE,   
                 sep=",")
#optimal.model2 <- randomForest(SalePrice~OverallQual + GrLivArea + GarageCars + GarageArea + TotalBsmtSF + FirstFlrSF + FullBath + TotRmsAbvGrd + YearBuilt, data=DF1, #subset=train,
#                              mtry=loc[1],  ntree=loc[2],
#                             importance=TRUE)


output=predict(optimal.model, newdata=DF1)

print(output)

View(optimal.model)
View(DF1)



