attach(Wage)
library(boot)
#Performing cross validation to obtain optimal value for the polynomial degree:
mse=rep(NA,10)
for (i in 1:10){
  fit=glm(wage~poly(age,i),data=Wage)
  mse[i]=cv.glm(Wage,fit,K=10)$delta[1]
}
plot(1:10,mse,xlab='degree',ylab='mse',type='l')  
#From the plot it seems test mse is smallest at d=9, however d=3 and d=6 give similar values
points(which.min(mse),mse[which.min(mse)],col = "red", cex = 2, pch = 20)
mse
which.min(mse)
#The which.min() function confirms d=9 is the optimal point
#The anova analysis shows that cubic and quartic fits are sufficient fits for this data.
#Now, using a step function:
mse=rep(NA,10)
for (i in 2:10){
  Wage$age.cut = cut(age, i)
  fit=glm(wage~age.cut,data=Wage)
  mse[i]=cv.glm(Wage,fit,K=10)$delta[1]
}
which.min(mse)
#8 is the optimal number of cuts
#Exploring other predictors
pairs(Wage)
boxplot(wage~health,data=Wage, main='Health')
boxplot(wage~maritl,data=Wage, main='Marital Status')
boxplot(wage~race,data=Wage,main='Race')
#Race and Health appear to influence wages
library(gam)
fit=gam(wage~cut(age,8)+s(year,5),data=Wage)
fit1=gam(wage~cut(age,8)+s(year,5)+race+health,data=Wage)
fit2=gam(wage~cut(age,8)+year+race+ health,data=Wage)
anova(fit,fit1,fit2,test='F')
#The second model appears to be the best model. There is no evidence model 3 is better than model 1, i.e there is a non-linear relationship between year and wages.
attach(College)
sample= sample(nrow(College),nrow(College)*0.7)
train=College[sample,]
#Forward stepwise selection to select the best subset of features
library(leaps)
fit= regsubsets(Outstate~.,data=train, nvmax=17, method='forward')
summary(fit)
n=nrow(College)
k=10
folds=sample(rep(1:k,length=n))
predict.regsubsets = function (object , newdata , id) {
  form = as.formula (object$call[[2]])
  mat = model.matrix (form , newdata)
  coefi = coef (object , id = id)
  xvars = names (coefi)
  mat[, xvars] %*% coefi
} 
cv.errors=matrix(NA,k,17,dimnames=list(NULL,paste(1:17)))
for(j in 1:k){ #jth cross validation fold (k folds in total)
  best.fit=regsubsets(Outstate~., data=College[folds!=j,],nvmax=17)
  for(i in 1:17){  #best i variable model
    pred=predict(best.fit,College[folds==j,], id=i)
    cv.errors[j,i]=mean((Outstate[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
which.min(mean.cv.errors)
#According to the cross val scores, 16 is the optimal number of variables, and from the results of the earlier fit model, the variable that should be excluded in this case in P.Undergrad
plot(1:17,mean.cv.errors,xlab='no. of predictors',ylab='cv error')
#From the plot, we can see that with 6 predictors, the cv error is quite similar to lowest one, so for ease of interpretability, we will stick with 6 predictors
#Room.Board, Terminal,Private,perc.alumni, Expend, Grad.Rate
pairs(College)
#Trying to find variables with non-linear relationship to Outstate

fit=gam(Outstate~s(Room.Board,df=5)+s(Terminal,df=5)+Private+s(perc.alumni,df=5)+s(Expend,df=5)+s(Grad.Rate,df=5),data=College)
summary(fit)
#The ANoVA for nonparametric effects shows that there is only 1 strong non-linear relationship (Expend), and 2 other weak non-linear relationships (Room.Board & Grad.Rate).
