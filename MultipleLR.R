#Multiple Linear Regression 
#Linear Modeling : DV vs more than 1 IVs
#sales Qty vs price & promotion

#Omni Store
#creating data using Vector
sales= c(4141,3842,3056,3519,4226, 4630,3507,3754, 5000,5120,4011, 5015,1916,675, 3636,3224,2295, 2730,2618,4421, 4113,3746, 3532, 3825,1096, 761,2088,820,2114, 1882,2159,1602,3354,2927)
price = c(59,59,59,59,59,59,59,59,59,59,59,59, 79,79,79,79,79,79,79,79,79, 79,79,79,99,99, 99,99,99,99,99,99,99,99)
promotion= c(200,200,200,200,400,400,400,400, 600,600,600,600,200,200,200,200, 400,400,400,400,600,600,600,600, 200,200,200,200,400,400,400,400,600,600)
#Create a DF from 3 variables
omni1 = data.frame(sales,price,promotion )
omni1
#Check if it has been created


#2nd Method : CSV file
omni3 =read.csv("./data/mlr.csv") #TO import the csv file into R studio-----Way1
str(omni3)
omni3
omni=omni3
#omni2 = read.csv(file.choose())

#3rd Method : gsheet 
library(gsheet)
url = "https://docs.google.com/spreadsheets/d/1h7HU0X_Q4T5h5D1Q36qoK40Tplz94x_HZYHOJJC_edU/edit#gid=1595306231"
omni2 = as.data.frame(gsheet::gsheet2tbl(url))


?lm  #see help of LM
#Simple Linear Model would look like this
slr1 = lm(formula = sales ~ price, data=omni) # sales depend on price of item
slr2 = lm(formula = sales ~ promotion, data=omni) # sales depend on promotion exp
summary(slr1)
summary(slr2)


#MLR  Create Multiple Linear Regression
# we want to see how Sales Qty depend on Price and Promotion Values
mlrmodel1 = lm(formula = sales ~ price + promotion, data=omni)
#how to give parameter values in different sequence, use arguments names if in different order
mlrmodel1 = lm( data=omni, formula = sales ~ price + promotion)

# summary statistics of model IMP STEP
summary(mlrmodel1)
#understand values : R2, AdjR2, Fstats pvalue, Coeff, ***, Residuals

#coefficients b1, b2



#Predicted Values----
fitted(mlrmodel1)
fitted(mlrmodel1)
cbind(sales,fitted(mlrmodel1))
names(omni1)
#create a dataframe of new sample values
(ndata1 = data.frame(price=c(60,70,80,90), promotion=c(300,400,500,600)))
predictY=predict(mlrmodel1, newdata= ndata1) # Predict Function for 5 values of X
cbind(ndata1,predictY)
#cbind the values

coef(mlrmodel1)
coef(mlrmodel1)[1]
coef(mlrmodel1)[2]
coef(mlrmodel1)[3]


#Plots of the Modle
plot(mlrmodel1,1)  # no pattern, equal variance
plot(mlrmodel1,2)  # Residues are normally distributed
plot(mlrmodel1,3)
plot(mlrmodel1,4)  # tells outliers which affect model