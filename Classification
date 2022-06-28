library(ISLR2)
?Weekly
pairs(Weekly)
#All the variables do not appear to be correlated with each other, maybe except for year and volume. No strong relationshoips with lag.
#1st model: Logistic Regression
summary(glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly, family=binomial))
#Lag2 appears to be the only statistically significant predictor.
glm.logreg=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly, family=binomial)
logreg_prob= predict(glm.logreg, type='response')
logreg_pred= rep('Down',nrow(Weekly))
logreg_pred[logreg_prob>0.5]='Up'
table(logreg_pred, Weekly$Direction)
#The logistic regression model appears to mis-classify a lot of down points as up, and it is in general quite bad at predicting the down points, however it does a fairly good job predicting the Up points.
mean(logreg_pred==Weekly$Direction)
#Holding out the data for years 2009 and 2010
attach(Weekly)
train=(Year<2009)
test= Weekly[!train,]
testd= Direction[!train]
glm.fits=glm(Direction~Lag2, data=Weekly,family=binomial, subset=train)
probs= predict(glm.fits,test, type='response')
glm.pred= rep('Down',nrow(test))
glm.pred[probs>0.5]='Up'
table(glm.pred, testd)
mean(glm.pred==testd)
#This results in an improved fit
#2nd Model: LDA
library(MASS)
lda.fits=lda(Direction~Lag2, data=Weekly, subset=train)
probs=predict(lda.fits, test, type='response')
lda_class= probs$class
table(lda_class, testd)
mean(lda_class==testd)
#As expected, LDA performs very similarily to logistic regression
#3rd Model:QDA
qda.fits= qda(Direction~Lag2, data=Weekly, subset=train)
qda_class= predict(qda.fits, test, type='response')$class
table(qda_class, testd)
mean(qda_class==testd)
#QDA performs slightly worse, suggesting the decision boundary might be more linear in nature that the LDA & log reg models are better able to handle.
#4th Model: Naive Bayes
library(e1071)
nb.fit= naiveBayes(Direction~Lag2,data=Weekly, subset=train)
nb.class=predict(nb.fit, test)
table(nb.class,testd)
mean(nb.class==testd)
#The NB model predicts everything as up, similar to the QDA classifier.
#Model 5: KNN
library(class)
train=Weekly[train,]
X_train= as.matrix(train[,3])
y_train=Direction[train]
X_test=as.matrix(test[,3])
knn.pred= knn(X_train,X_test,y_train,k=1)
table(knn.pred,testd)
mean(knn.pred==testd)
#For k=1, the error rate is 0.5 which is the worst possible score to have, meaning it performs worst than a random classifier.
#For k=3
knn.pred= knn(X_train,X_test,y_train,k=3)
table(knn.pred,testd)
mean(knn.pred==testd)
#Slight improvement after increasing k, due to decrease in variance of model.
attach(Auto)
mpg01=rep(0,nrow(Auto))
mpg01[mpg>median(mpg)]=1
mpg01=as.data.frame(mpg01)
mpg01df=cbind(Auto,mpg01)
pairs(mpg01df)
boxplot(acceleration~mpg01, data=mpg01df, main='Acceleration vs mpg01')
#mpg01 tends to be 1 for higher accelerations, evidence of some association between the 2
boxplot(weight~mpg01,data=mpg01df, main='Weight vs mpg01')
boxplot(displacement~mpg01,data=mpg01df, main='displacement vs mpg01')
boxplot(horsepower~mpg01,data=mpg01df, main='horsepower vs mpg01')
#weight, displacement and horsepower also appear to be associated with mpg01.
#Splitting into training and test set (70/30 split):
test=sample(c(TRUE, FALSE), nrow(Auto), replace=TRUE, prob=c(0.7,0.3))
sum(is.na(mpg01df))
df=na.omit(mpg01df)
new_train=df[train,]
test=df[!train,-'mpg01']
test_=df[!train,'mpg01']
test= within(test, rm('mpg01')) 
lda.fit=lda(mpg01~acceleration+weight+displacement+horsepower+cylinders,data=mpg01df, subset=train)
probs=predict(lda.fit, test, type='response')
lda_class= probs$class
table(lda_class, test_)
length(lda_class)
length(test_)
test_
mean(lda_class!=test_)
sum(is.null(test_))
#The test error rate is 0.144, which is fairly good.
#For QDA:
qda.fits=qda(mpg01~acceleration+weight+displacement+horsepower+cylinders,data=mpg01df, subset=train)
qda_class= predict(qda.fits, test, type='response')$class
table(qda_class, test_)
mean(qda_class!=test_)
#Test error is 0.16, which is slightly higher than LDA
Power2=function(x,a){
  result=x^a
  return(result)
}
Power2(2,3)
plot(1:10,Power2(1:10,2),xlab='x',ylab='x^2')
Plotpower=function(x,a){
  plot(x,Power2(x,a))
}
