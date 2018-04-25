#Missing values


v1=c(1,2,NA,NA,5)
df=data.frame(x=c(1,NA,3,4,NA),y=c(0,1,NA,4,NA))
df
df1=na.omit(df)
df1
is.na(v1)#TO see \if the element in the vector is missing
sum(is.na(v1)) #To see the total number of missing values
mean(v1)
mean(v1,na.rm='T') #To exclude na values
v1a=na.omit(v1)   #To create a new vector without any missing values
v1a
sum(v1a)
?na.omit
anyNA(v1)
v1
mean(v1,na.rm='T')
v1[c(3,4)]
v1[is.na(v1)]
v1[is.na(v1)]=mean(v1,na.rm='T')    #To replace the missing values by the mean
v1
#install.packages('VIM')
library(VIM)
data(sleep, package='VIM')
head(sleep)
dim(sleep)
complete.cases(sleep) #all complete rows withouth any missing values
sum(complete.cases(sleep))   #Number of complete rows
sum(!complete.cases(sleep))   #Number of incomplete rows
sleep[complete.cases(sleep),]  #display all complete rows
sleep[!complete.cases(sleep),]  #display all incomplete rows

is.na(sleep$Dream)
sum(is.na(sleep$Dream))  #Number of missing dream =12
mean(is.na(sleep$Dream))   #12/62
sum(is.na(sleep))
names(sleep)
colSums(is.na(sleep)) #Number of missing values column wise
rowSums(is.na(sleep))  #Number of missing values in the differernt rows
head(sleep)

install.packages('mice')
library(mice)
mice::md.pattern(sleep)
#42 rows without any missing values #2rows with 1 NA Span
#Visualistion
VIM::aggr(sleep,prop='F', numbers='T')
VIM::aggr(sleep, prop='T') #prop is for proportion
VIM::matrixplot(sleep)
VIM::matrixplot(sleep[c])
