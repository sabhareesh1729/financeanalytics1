log1 =read.csv("./data/logr.csv") #TO import the csv file into R studio-----Way1
str(log1)
log1
log=log1
names(log)
summary(log)

#install.packages('caTools')
library(caTools) 
set.seed(2000)
split=sample.split(log$purchased, SplitRatio = 0.75)  #Make two partitions with 75% in one partition and 25% in another, so that we can have a test set and a training set randomly
training_set=subset(log, split==TRUE)  #Split as we need
test_set=subset(log, split==FALSE)
dim(log)
length(log)  #Columns number
length(log$userid)   #Rows number
nrow(training_set)   #Rows number in ttarining set
dim(test_set)    #Dimensions
names(log)
class(log$gender)
names(training_set)

#Fitting logistical regression into the training test
logitmodel1=glm(purchased~gender+age+salary, family=binomial, data=training_set)
summary(logitmodel1)

#Now, we drop the insignificant parameters and make a new model
logitmodel2=glm(purchased~age+salary, family=binomial, data=training_set)
summary(logitmodel2)$coefficient
coefficients(logitmodel2)
test_set2=data.frame(age=c(40,65), salary=c(40000,50000))
test_set2
prob_pred2= predict(logitmodel2, type= 'response', newdata=test_set2)
prob_pred2
cbind(test_set2,prob_pred2)
#Now, we decide on a boundary line to decide if they will buy or not, and ideally it is Probability = 0.5 

#Predicting test set results from the test set
test_set
prob_pred=predict(logitmodel2, type='response', newdata=test_set)
df_prob_pred=as.data.frame(prob_pred)
summary(df_prob_pred)
head(df_prob_pred)
#If probability is > 0.5 make it 1, else make it 0=>similar to the ternary operator in c, c++
y_pred=ifelse(prob_pred>0.5,1,0)
y_pred
cbind(test_set$purchased,y_pred)

#Confusion Matrix
#install.packages('caret')
cm=table(test_set[,5], y_pred)
cm
caret::confusionMatrix(cm)
?confusionmatrix
