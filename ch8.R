library(ISLR2)
attach(Carseats)
sample=sample(nrow(Carseats),nrow(Carseats)/2)
train=Carseats[sample,]
#Regression trees
library(tree)
tree= tree(Sales~.,data=train)
summary(tree)
#deviance(i.e sum of squared errors)=2.169, shelveloc and price appear to be the most important variables
plot(tree)
text(tree, pretty=0)
pred=predict(tree, newdata=Carseats[-sample,])
mean((pred-Sales[-sample])^2)
#The test mse is 5.31, higher than training mse as expected
#Performing cv to obtain optimal complexity for tree
treecv=cv.tree(tree)
plot(treecv$size, treecv$dev, type='b')
#The largest tree size appears to give the lowest mse (16)
which.min(treecv$dev)
#Hence pruning the tree doesn't improve the results, and a more complex tree is actually optimal for the mse
library(randomForest)
#Now using bagging for this model:
#Since bagging is essentially random forests with m=p (i.e we use all the predictors)
bag= randomForest(Sales~., data= train, mtry=10, importance=TRUE)
#Random Forest model (here the no. of predictors used for each tree is p/3) )
rf= randomForest(Sales~., data=train, importance=TRUE)
dim(Carseats)
pred=predict(bag,newdata=Carseats[-sample,])
predrf= predict(rf, newdata=Carseats[-sample,])
mean((pred-Sales[-sample])^2)
mean((predrf-Sales[-sample])^2)
#test MSE= 3.16 (for bagging)
#test MSE= 3.25 (for random forests)
#It appears, using all the predictors for each regression tree was a better fit than p/3
importance(bag)
#ShelveLoc and Price are by far the most important variables for the bagging model as well with the greatest % increases in MSE and increases in node purity
importance(rf)
#The results are very similar for the random forest model
attach(Hitters)
Hitters= Hitters[!is.na(Salary),] #Removing all the rows that have na values for salary
#Now, we should log transform the Salary.The log transformation is particularly relevant when the data vary a lot on the relative scale, and since Salary is often in the thousands, this applies here.
Hitters$Salary= log(Hitters$Salary)
dim(Hitters)
length(Salary)
train=Hitters[1:200,]
test= Hitters[201:nrow(Hitters),]
library(gbm)
#For loop that computes the test MSE for each value of shrinkage:
pows = seq(-10, -0.2, by = 0.1)
lambdas = 10^pows
test_err=rep(NA,length(lambdas))
for (i in 1:length(lambdas)){
  boost=gbm(Salary~.,data=train, distribution='gaussian',n.trees=1000 ,shrinkage=lambdas[i])
  pred=predict(boost,newdata=test, n.trees=1000)
  mse= mean((pred-test$Salary)^2)
  test_err[i]=mse
}
plot(lambdas, test_err, xlab='shrinkage value', ylab='test mse')
#Optimal shrinkage value=0.4
which.min(test_err)
# 9.284967e-05
#Comparing this to Least Squares:
lm.fit=lm(Salary~.,data=train)
pred=predict(lm.fit, newdata=test)
mean((pred-test$Salary)^2)
#the boosting model outperforms LS regression for this data
attach(OJ)
sample=sample(nrow(OJ),800)
train= OJ[sample,]
test=OJ[-sample,]
View(OJ)
?OJ
tree= tree(Purchase~.,data=train,)
summary(tree)
#Only 3 variables used: "LoyalCH"   "PriceDiff" "DiscMM" , 8 terminal nodes.
#The misclassification error rate is fairly low (0.16)
tree
#LoyalCH appears to be the most important variable, with the first split being the most divisive (one side of the the split gives MM and the other side gives CH)
pred= predict(tree, test, type='class')
mean(pred!=test$Purchase)
#the test error rate is 0.17
#Using cv to find out whether we should prune the tree:
cvtree=cv.tree(tree, FUN=prune.misclass)
cvtree
#The largest tree corresponds to the lowest deviance (i.e misclassification), hence we will not prune it 
rf= randomForest(Purchase~., data=train, importance=TRUE)
prob=predict(rf, newdata=test,type='response')
mean(prob!=test$Purchase)
#The test error rate is 0.18, which is slightly higher than for a single tree
#Since Bernoulli requires the response to be in {0,1}, we can transform the predictor values to take on either 1 or 0
OJ$Purchase=ifelse(OJ$Purchase=='MM',1,0)
boost=gbm(OJ$Purchase~.,data=train, distribution='bernoulli',n.trees=1000)
prob=predict(boost, newdata=test)
mean(prob!=test$Purchase)
contrasts(Purchase)

