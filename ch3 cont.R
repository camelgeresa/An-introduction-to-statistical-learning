set.seed(1)
x=1:100
y=x[order(rnorm(100))]
lm1=lm(x~y+0)
lm2=lm(y~x+0)
summary(lm1)
summary(lm2)
x=rnorm(100)
eps= rnorm(100,sd=0.25)
Y= -1+0.5*x+eps
length(Y)
#B0=-1, B1=0.5, length=100
plot(x,Y)
#Positive, linear relationship with a lot of noise, 
summary(lm(Y~x))
#B0=-0.993 B1=0.487
xy=data.frame(x,Y)
attach(xy)
plot(x,Y)
abline(lm(Y~x))
abline(a=-1,b=0.5, col='red')
summary(lm(Y~x+I(x^2)))
#There is no evidence adding a quadratic term would improve the fit. The extra term is not statistically significant. However the adjusted R-squared value improved so maybe the second model was a better fit, however it is a very small improvement. 
eps= rnorm(100,sd=0.1)
Y= -1+0.5*x+eps
summary(lm(Y~x))
confint(lm(Y~x))
#Improved R squared, not surprising given that there is less noise in the data.
eps= rnorm(100,sd=0.5)
Y= -1+0.5*x+eps
summary(lm(Y~x))
confint(lm(Y~x))
#With more noise in the data, the R squared value for the model has decreased. Furthermore, the confidence intervals have increased, showing the model is less certain in predicitng the coefficients. 
set.seed (1)
x1 = runif (100)
x2 = 0.5 * x1 + rnorm (100) / 10
y = 2 + 2 * x1 + 0.3 * x2 + rnorm (100)
plot(x1,x2)
#There is a weak positive relationship between x1 and x2 suggesting their is some collinearlity in the variables
lm3=lm(y~x1+x2)
summary(lm3)
#B0=2.13, B1=1.44, B2=1
#Collinearality increases the uncertainity when estimating the coefficients. This increases the standard error for the coefficients, and can lead to us incorrectly identifying a non zero coefficient.
#As the p value for x2 is quite high we would normally reject it. However as we saw earlier, the collinearality between the 2 variables can lead to an inflated t statistic, so it is more likely we would misidentify that coefficent as zero.
summary(lm(y~x1))
#B0=2.11, B1=1.98
summary(lm(y~x2))
#B0=2.39, B1=2.90
#Both x1 and x2 are indeed non-zero coefficients.
x1 = c(x1 , 0.1)
x2 = c(x2 , 0.8)
y =c(y, 6)
#For the models with the new data points added:
#Model 1: B0=2.22, B1=0.53, B2=2.51
#Model 2: B0=2.25, B1=1.76
#Model 3: B0=2.34, B1=3.12
#The new data point changes the significant variables, with x2 being the significant variable in this case. The coefficients for each model have changed suggesting the extra point had a fairly high leverage.
par(mfrow=c(2,2))
plot(lm3)
#Looking at the residuals vs leverage plot, the point we added (101) is an influential one as it has a high leverage as well as being an outlier (large standardized residuals)
?Boston
#Creating a for loop that will fit a simple regression model for each predictor
R2
slope=c()
P=c()
for (i in 1:12){
  R2[i]=summary(lm(crim~Boston[,i+1],data=Boston))$r.squared 
  P[i] =summary(lm(crim ~ Boston[,i+1],data=Boston))$coefficients[2, 4]
  slope[i] =summary(lm(crim ~ Boston[,i+1], data=Boston))$coefficients[2, 1]
}
Bos= subset(Boston, select=-(crim))
crime_preds = data.frame(varname = names(Bos), 
                          R2 = round(R2, 5), 
                          P = round(P,5), 
                          slope = round(slope, 5))
View(crime_preds)
#All variables apart from chas appear to be significant 
summary(lm(crim~.,data=Boston))
#For the second model with all the varaibles, there are a lot less variables that appear to have a non zero coefficient.
x=summary(lm(crim~.,data=Boston))$coefficients[2:13,1]
multiple= data.frame(predictor=names(Bos),Estimate=x, slope=round(slope,4))
View(multiple)
plot(multiple$Estimate,multiple$slope,col=factor(multiple$predictor))
#The majority of the coefficients seem to lie close to each other, with the univariate regression coefficients being similar to the multivariate regression coefficients, except for nox predictor which had quite a large discrepancy between the two.