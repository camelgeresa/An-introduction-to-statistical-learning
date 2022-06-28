?Hitters
attach(Hitters)
k=10
n=nrow(Hitters)
set.seed(1)
folds=sample(rep(1:k,n))
folds
Hitters=na.omit(Hitters)
library(leaps)
predict.regsubsets = function (object , newdata , id) {
  form = as.formula (object$call[[2]])
  mat = model.matrix (form , newdata)
  coefi = coef (object , id = id)
  xvars = names (coefi)
  mat[, xvars] %*% coefi
} 
cv.errors=matrix(NA,k,19,dimnames=list(NULL,paste(1:19)))
cv.errors
for(j in 1:k){ #jth cross validation fold (k folds in total)
  best.fit=regsubsets(Salary~., data=Hitters[folds!=j,],nvmax=19)
  for(i in 1:19){  #best i variable model
    pred=predict(best.fit,Hitters[folds==j,], id=i)
    cv.erros[j,i]=mean((Salary[folds==j]-pred)^2)
  }
}
#Finding the best model for the test(i.e !=j), and then predicting on cv (==j).
#Results in 10x19 matrix where the (j, i)th element corresponds to the test MSE for the jth cross-validation fold for the best i-variable model
set.seed(1)
X=rnorm(100)
e=rnorm(100)
Y=3+3*X+3*X^2+3*X^3+e
xy=data.frame(X=X,Y=Y)
regfit=regsubsets(Y~X+I(X^2)+I(X^3)+I(X^4)+I(X^5)+I(X^6)+I(X^7)+I(X^8),data=xy,nvmax=9)
reg.summary=summary(regsubsets(Y~poly(X,10),data=xy,nvmax=10))
plot(reg.summary$adjr2, xlab='No. of variables', ylab='Adjusted R^2',type='l')
points(which.max(reg.summary$adjr2),reg.summary$adjr2[which.max(reg.summary$adjr2)],col='red',cex=2,pch=20)
coef(regfit,id=3)
coef(regfit, which.max(reg.summary$adjr2))
#Feature selection with forward selection:
summary(regsubsets(Y~X+I(X^2)+I(X^3)+I(X^4)+I(X^5)+I(X^6)+I(X^7)+I(X^8),data=xy,nvmax=9,method='forward'))
summary(regsubsets(Y~X+I(X^2)+I(X^3)+I(X^4)+I(X^5)+I(X^6)+I(X^7)+I(X^8),data=xy,nvmax=9,method='backward'))
#The terms used are quite similar for each of the models
library(glmnet)
xmat=model.matrix(Y~X+I(X^2)+I(X^3)+I(X^4)+I(X^5)+I(X^6)+I(X^7)+I(X^8))
lasso=cv.glmnet(xmat,Y,alpha=1)
plot(lasso)
best.lam=lasso$lambda.min
lasso=glmnet(xmat,Y,alpha=1)
lasso.predict=predict(lasso, s=best.lam,type='coefficients')
lasso.predict
#Has set I(X^6) & I(X^8)=0
library(ISLR2)
attach(College)
sample=sample(1:nrow(College),nrow(College)/2, replace=T) #When sampling, you use-, not!
train=College[sample,]
test=College[-sample,]
lm.fit=lm(Apps~.,data=train)
pred=predict(lm.fit,test)
mean((pred-test$Apps)^2)
#MSE=1198449 (Least squares)
summary(lm.fit)
x=model.matrix(Apps~.,College)[,-1]
y=Apps
lassocv=cv.glmnet(x[sample,],y[sample],alpha=1)
lasso.mod=glmnet(x[sample,],y[sample],alpha=1)
pred=predict(lasso.mod,s=lassocv$lambda.min, newx=x[-sample,])
pred=predict(lasso.mod,s=lassocv$lambda.min, type='coefficients')
mean((pred-y[-sample])^2)
#MSE=1322449 (Lasso)
ridgecv=cv.glmnet(x[sample,],y[sample],alpha=0)
ridge.mod=glmnet(x[sample,],y[sample],alpha=0)
pred=predict(ridge.mod,s=lassocv$lambda.min, newx=x[-sample,])
mean((pred-y[-sample])^2)
#MSE=1132922 (Ridge)
#Interestingly, the MSE for ridge regression is the lowest. This might be perhaps because all the variables are related to the response, and lasso sets some of the coefficients=0, which is why it has the highest MSE. Ridge also has the benefit of decreasing the variance (compared to LS model) so this might be why it performs the best.
library(pls)
pcr.fit=pcr(Apps~.,data=College, scale=TRUE, validation='CV')
summary(pcr.fit)
#Want to find the optimal number of components:
validationplot (pcr.fit , val.type = "MSEP")
#From this plot, we can see the optimal number of components is around 5.
#Now, fit the model to the whole data set.
pcr.fit=pcr(Apps~.,data=College, scale=TRUE, subset=sample)
pcr.pred=predict(pcr.fit, x[-sample,],ncomp=5)
mean((pcr.pred-y[-sample])^2)
#MSE=2468476 (PCR).PCR appears to be the worst model, this might be due to the fact PCR is better suited in situations where p>>n, which is not the case here.
