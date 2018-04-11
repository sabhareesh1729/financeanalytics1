#Simple Linear Regression : IIT Gauhati
# Regression : Areas vs Sales

#Method1 : creating data from Vectors
#X -is area sqft Y-sales in 1000s units; FOR very small data , Find relationship betn X & Y
X = c(1.7,1.6,2.8,5.6,1.3,2.2, 1.3,1.1,3.2,1.5,5.2,4.6,5.8,3 )
Y = c(3.7,3.9,6.7,9.5,3.4,5.6,3.7,2.7,5.5,2.9,10.7,7.6,11.8,4.1 )

#create a data frame from X & Y
df1 =data.frame(X,Y)
df1
#first few values

#2nd method of importing data
#import from ggsheet  #pickup the correct url
library(gsheet)      #We just installed the library and made it a comment
url="https://docs.google.com/spreadsheets/d/1h7HU0X_Q4T5h5D1Q36qoK40Tplz94x_HZYHOJJC_edU/edit#gid=2023826519"
df2=as.data.frame(gsheet2tbl(url))    #Command to import from googlesheets
str(df2)
df2

#df2 = 
#first few values

#Third method of importing data from CSV file
df3 =read.csv("./data/lr.csv") #TO import the csv file into R studio-----Way1
str(df3)
df3
df=df3

df4= read.csv(file.choose())
df4
#first few values

#Method4 for importing from CSV file with choose
#df4 = read.csv(file.choose())
#first few values

# Use Vector Data or method used to import data
#make one of the DF active
#df = 

#Find mean, sum of X & Y, Covariance (COV), Correlation (COR)
sum(df$X); sum(df$Y) #To print both in the same line in the same code run
sum(df$Y)
mean(df$X)
mean(df$Y)
#options(digits=4) To fix the number of significant digits

cor(df$X,df$Y)
cov(df$X,df$Y)
?cor
#plot X & Y
Area=df$X
Sales=df$Y
plot(x=Area, y=Sales, pch=8, type='p', col='green')
plot(y=df$Y,x=df$X,xlab = "Area in square feet", ylab = "Sales Amount", type='p',ylim=c(0,max(df$Y)), main = "Plot of area vs Sales", xlim = c(0,max(df$X)), col='red', pch=15) #xlab and ylab is to set the labesl for the x and y axis wheeas xlim and ylim is to set the limit of the axes height...It takes the arguments as coordinates.
?plot
?ylim
?pch
#draw abline/ prediction line
abline(lm(df$Y~df$X),lty=2,lwd=3 , col='green')#lty is for ltype, lwd is for line width
abline(v=c(3,5,2,1),h=c(6,10),col=c("red","blue","green")) #To draw vertical and horizontal lines VERTICAL-> v=c(x1,x2,x3,..) and HORIZONTAL-> h=c(y1,y2,y3...)
?lty
?lwd
?lm
?abline
#Linear Model fit= lm(Y~X, data=)
fit1=lm(Y~X, data=df)   #Creates Linear Model of Y wrt X
fit1
coef(fit1) #Both intercept and slope
coef(fit1)[1] #Intercept
coef(fit1)[2] #Slope
range(df$X)  # We can only intrapolate when the value of X is between 1.1 and 5.8, and we cant go beyond that. For ex., at X=0, Y=0.96, but it doesnt make sense to have esnse when the total area os shop is 0!

range(df$Y)
summary(fit1)
names(fit1)
coefficients(fit1)
#Print the Model, Summary, Coeff, Residuals


#understand the model values - R2, AdjR2, FStats, Residuals, Coeff p values - IMP STEP

#output of variable Names

#Print Slope value

?fitted
#combine the data with Ypredicted, errors
fitted(fit1)  #Puts the predicted equation for Y to compare it with the original data, used in finding the residuals
cbind(df,fitted(fit1),fitted(fit1)-df$Y,residuals(fit1)) #TO print it as a table with these different columns
residuals(fit1)
# Predictions
#(Y = 0.9645 + 1.6699 * 4)  # Predict Y for X=4 but this cant be used if the data is changed!
(Y = coef(fit1)[1] + coef(fit1)[2] * 4)  # for X=4 using values from output of LM 

fitted(fit1)
cbind(df, fitted(fit1))  # combine data with predicted values
range(df$X)  #min to max value of X: area

# Create a Data Frame with sample values of X
new1=data.frame(X=c(1.5,2,3,4,5))
new1
#X is the independent variable and it must lie within the range of 1.1 and 5.8

#sample data for X for prediction, should be between the range of X values
#prediction data should be in the form of Data Frame

#Predict
predict(fit1, newdata= new1)
predictY=predict(fit1, newdata= new1) # Predict Function for 5 values of X
cbind(new1,predictY)
#fitted(fit1) is equal to predict function using original X values
df$X
predictY = predict(fit1, newdata= data.frame(X=df$X))
cbind(df, predictY, fitted(fit1))

#install.packages('forecast')
library(forecast)
accuracy(fit1)


summary(fit1)
summary(fit1)$sigma  #Residual Std Error SD along the LM Line


#Assumption : Graphical Analysis : IMP STEP
plot(fit1, which=1)
plot(fit1)
plot(df$X, df$Y) # plot of Y & X
plot(x=df$X, y=residuals(fit1)) # Linearity plot of residuals & X # No pattern for assumption that there is linearity betw X & Y
abline(h=0)
plot(residuals(fit1))

#Auto Collinearity : relation between successive values of Y
car::durbinWatsonTest(fit1)
#pvalue > 0 : Do not reject Ho that there is no correlation


#Normality of residuals
hist(residuals(fit1)) #distribution of Residuals

hist(residuals(fit1), freq=F)
lines(density(residuals(fit1)))

#histogram values to show how hist rectangle are created
(h=hist(residuals(fit1)))
names(h)
cbind(h$breaks, h$counts)

#Normality Plot to check assumptions of LM
qqnorm(residuals(fit1))
qqline(residuals(fit1))

#Equal Variance : 4th Assumption : homoscedasticity
plot(y=residuals(fit1), x=df$X)
abline(h=0)  #Variance across all x values is almost constant


#Outlier Analysis

boxplot(residuals(fit1), names=c('Residuals'))
identify(rep(1, length(residuals(fit1))), residuals(fit1), labels = seq_along(residuals(fit1)))

car::outlierTest(fit1)

car::outlierTest(lm(Y ~ X, data=df[-c(14,12),]))

