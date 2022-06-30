x= rnorm(100)
y=3+x^2+rnorm(100)
sample=sample(100,50)
y[sample]=y[sample]+1
y[sample]=y[sample]-1
plot(x[sample], y[sample], col = "red", xlab = "X", ylab = "Y")
points(x[-sample], y[-sample], col = "blue")
#Non-linear decision boundary
z=rep(-1,100)
z[sample]=1
dat=data.frame(x=x,y=y, z=as.factor(z))
View(dat)
train=dat[sample,]
test=dat[-sample,]
svmfit=svm(z~.,data=train,kernel='linear',cost=10)
plot(svmfit,train)
table(predict = predict(svmfit, train), truth = train$z)
#6+13=19 misclassification errors
#Fit a model with a polynomial kernal, that will allow a more flexible boundary
svmfit=svm(z~.,data=train,kernel='polynomial',cost=10)
#22 errors with non-linear boundary
svmfit=svm(z~.,data=train,kernel='radial',cost=10, gamma=1)
#gamma refers to the amount of 'curvature' the slope will have
#The model with the radial kernel only misclassifies 13 points!
x1=runif(500)-0.5
x2=runif(500)-0.5
y=1*(x1^2-x2^2 > 0)
plot(x1,x2,xlab='x1',ylab='x2',col=(4-y))
#The linear boundary is clearly non-linear
dat=data.frame(x1=x1,x2=x2,y=y)
sample=sample(nrow(dat),nrow(dat)/2)
train=dat[sample,]
test=dat[-sample,]
fit=glm(y~.,data=train,family='binomial')
summary(fit)
#Neither x1 nor x2 are significant
probs=predict(fit, newdata=test, type='response')
preds = rep(0, 500)
preds[probs > 0.5] = 1
mean(preds!=test$y)
#Misclassification rate is 0.516, as expected not too good, since we are using a linear decision boundary for a decision boundary that is obviously non-linear
fit=glm(y~I(x1*x2),data=train,family='binomial')
#0.496, slightly better
fit=glm(y~I(x1^2)+I(x2^2),data=train,family='binomial')
data$y = as.factor(data$y)
svm.fit = svm(y ~ ., data=train, kernel = "linear", cost = 0.01)
preds = predict(svm.fit, test)
preds= ifelse(preds>0.5,1,0)
table(predict=preds, truth=test$y)
preds
#Here the Support Vector classifier just classed everything as a 1, which gave a similar misclassification rate (~0.5)
svm.fit=svm(y ~ ., data=train, kernel = "polynomial", cost = 0.01)
#This decision boundary is still similar to the one with the linear kernel.
