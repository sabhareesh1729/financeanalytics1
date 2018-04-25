#Case Study
install.packages('rpart') #CART modelling
install.packages('rpart.plot')
#Students:Gender, buy a product
#Rollnumbers
library(rpart)
library(rpart.plot)
rollno=paste('S',1:1000,sep='')
rollno
#variable gender
set.seed(100)
gender=sample(x=c('Male','Female'),size=1000, replace=T, prob=c(0.5,0.5))
head(gender)
table(gender)

#variable=Buy, Decision
set.seed(3000)
buy=sample(x=c('Buy','NotBuy'),size=1000, replace=T, prob=c(0.5,0.5))
head(buy)
table(buy)
prop.table(table(buy))
?prop.table

#Create a data frame
students1=data.frame(gender,buy)
rownames(students1)=rollno
head(students1)
?table
#Table
table(students1)   #Creats a crosstable
prop.table(table(students1))
addmargins(prop.table(table(students1)))    #Addmargins includes a sum olumn to the table
(t1=table(students1$gender,students1$buy))
addmargins(t1)
addmargins(prop.table(table(students1$gender,students1$buy)))

#model
fit1=rpart(buy~gender, data=students1, minsplit=4, minbucket=2)
fit1

rpart.plot(fit1,main='CLASSIFICATION TREE', nn=T, type=4, extra = 104)
fit1
predict(fit1, newdata=data.frame(gender='Female'))
predict(fit1, newdata=data.frame(gender='Female'),type='class')
predict(fit1, newdata=data.frame(gender='Female','Male','Female'))


#Add another column
set.seed(5000)
married=sample(x=c('Married','Single'),size=1000,replace=T,prob=c(0.5,0.5))
table(married)
students2=data.frame(gender,married,buy)
rownames(students2)=rollno
head(students2)
str(students2)
table(students2)
prop.table(ftable(students2))
?ftable    #FLAT TABLES woth various factors
addmargins(prop.table(ftable(students2)))
fit2=rpart(buy~gender+married, data=students1, minsplit=2)
fit2
rpart.plot(fit2,main='CLASSIFICATION TREE', nn=T, type=2, extra = 104, tweak=1.1, under=T, shadow=c('brown','green','red'))
fit2
