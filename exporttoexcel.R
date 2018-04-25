#Time Series Analysis
# is the price of Johnson and Johnson stock
rollno=paste('S',1:30,sep='-')
rollno
name=paste('Student',1:30,sep='','-',' xyz')
name
(name1=paste0('Student',1:30,sep='','- xyz'))
?paste0
paste('R',1:3)
paste0('R',1:3)
set.seed(100)
gender=sample(c('M','F'),size=30,replace=T,prob=c(0.7,0.3))
gender
#age=sample(c(20:40),size = 30,replace=T, rbinom())
?runif
?rnorm
age=round(runif(30,20,40))
age
marks=round(rnorm(30,mean=50,sd=10))
marks
course=sample(c('BTech','MTech','Phd'),size=30,replace=T,prob=c(0.5,0.3,0.2))
course
married=sample(c('TRUE','FALSE'),size=30,replace=T,prob=c(0.6,0.4))
married
df=data.frame(rollno,name,gender,age,course,marks,married)
df
library(caTools)
set.seed(2000)
split=sample.split(df, SplitRatio = 0.75)  #Make two partitions with 75% in one partition and 25% in another, so that we can have a test set and a training set randomly
training_set=subset(df, split==TRUE)  #Split as we need
test_set=subset(df, split==FALSE)
training_set
test_set
nrow(training_set)
nrow(test_set)
names(training_set)

#Other way
sample(nrow(df),3)  #Gives the row number of three different rows
df1=df[sample(nrow(df),3),]
df1
(df2=subset(df,age>=20&age<27,select=c(rollno,name,age)))  #Subset where age is between 20 and 27 and with only rollno, name and age columns

#dplyr-EASIEST WAY TO MAKE PARTITIONS
library(dplyr)
(df3=sample_n(df,10))   #10 random samples
(df4=sample_frac(df,0.5))  #Half of the original size is taken as a random sample

df5=df[which(gender=='F'&age>25),]   #TO see all data with female and age > 25
df5

#Now let us save this into the excel file
write.csv(df,'./data/students3.csv')
#install.packages('xlsx')
library(xlsx)
Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre1.8.0_144")
library(xlsx)
write.xlsx(df,'./data/student1.xlsx',sheetName="IITG1", row.names=F, append=T)
write.xlsx(df2,'./data/student1.xlsx',sheetName="IITG2", row.names=F, append=T)

#Open the file in Excel