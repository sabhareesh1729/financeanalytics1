#Outlier Analysis
marks=c(1:80,100,1000)
marks2=c(1:80,100)
marks
par(mfrow=c(1,2)) #To plot two graphs in the same row

boxplot(marks); boxplot(marks2)
#par(mfrow=c(2,2))
boxplot(marks); boxplot(marks2); boxplot(marks); boxplot(marks2)
#summary(marks); summary(marks2)     #We see that the mean has been adversely affected but not the median...
mean(marks); mean(marks2)
median(marks); median(marks2)

abline(h=c(1,21.25,41.50,52.93,61.75,1000))
par(mfrow=c(1,1))
boxplot(marks)
fit2=lm()

#Identify----
set.seed(482)
y=rnorm(100)
median(y)
boxplot(y)
identify(rep(1,length(y)),y,labels=seq_along(y))   #Interactive-click on the graph
y[86]; y[52]; y[55]; y[45]; y[4]; y[91]
max(y)
min(y)
