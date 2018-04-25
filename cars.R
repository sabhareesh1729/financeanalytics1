#MLR using mtcars dataset
mtcars
?mtcars
data() #available datasets
dim(mtcars)
#To predict mileage based on other variables
names(mtcars)
head(mtcars)
mtcarsfit1=lm(formula=mpg~cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb, data=mtcars)
mtcarsfit2=lm(formula=mpg~., data=mtcars)
summary(mtcarsfit1)
summary(mtcarsfit2)
install.packages('MASS')
library(MASS)
#library()
(model1a=lm(mpg~.,data=mtcars))  #All other factors except mpg
(model1b=lm(mpg~1,data=mtcars)) #No other factors just intercept
summary(model1a)
summary(model1b)

stepboth=stepAIC(model1a, direction='both')
fit1=lm(mpg~wt+qsec+am, data=mtcars)
summary(fit1)

stepfwd=stepAIC(model1a, direction='forward', scope= list(upper=model1a, lower=model1b))
stepback=stepAIC(model1a, direction='backward')
