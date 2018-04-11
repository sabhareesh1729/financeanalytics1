#case study-denco

sales1=read.csv("./data/denco.csv") #TO import the csv file into R studio-----Way1
str(sales1)

sales2= read.csv(file.choose())   #If we dont know where the file is, and we want to browse....A popup window opens and we can import ----WAY 2
str(sales2)

#install.packages('gsheet')

#Directly get it from the Googlesheet
library(gsheet)      #We just installed the library and made it a comment
url="https://docs.google.com/spreadsheets/d/1h7HU0X_Q4T5h5D1Q36qoK40Tplz94x_HZYHOJJC_edU/edit#gid=216113907"
sales3=as.data.frame(gsheet2tbl(url))    #Command to import from googlesheets
str(sales3)
?gsheet2tbl

sales=sales3  # keeping a backup
#head(sales1,n=7)
#names(sales1)

class(sales)
?summary
summary(sales)   #Summary of sales

str(sales)   #TO see the structure of sales
dim(sales)    #To see the number of rows-column i.e.dimensions
unique(sales$custname) #TO print a list of all the distinct customer names
length(unique(sales$custname)) #To find the total number of distinct custnames
length(unique(sales$region ))   #To find the total number of distinct regions

aggregate(sales$revenue , by=list(sales$custname), FUN=sum) #its is like groupby customer name 
df1 = aggregate(sales$revenue , by=list(sales$custname), FUN=sum)  #make the above data as a dataframe for sorting
head(df1)    #Display the top (first) 5 values Tail command is for the last 5 values
str(df1)

df1=df1[order(df1$x, decreasing=TRUE),]   #Arrange it in decreasing order ORDER
head(df1,5)
head(df1,10)
head(df1[order(df1$x, decreasing=TRUE),], 5) #Incorporating both into a single line

#Aggregate Formula
(df2 = aggregate(revenue ~ custname + region, data=sales, FUN=sum))
head(df2[order(df2$revenue,decreasing=T),],10)

#List
list1= tapply(sales$revenue, sales$custname, FUN=sum)
head(list1)
list1
head(sort(list1, decreasing=T))
summary(df3)
str(df3)

#dplyr
names(sales)
#install.packages('dplyr')  #Dplyr is a very handy library to use in data manipulation
library(dplyr)

sales %>% filter(margin > 1000000) %>% arrange(region, desc(revenue)) #Piping symbol %>% for data, by default arrange-ascending First it arranges according to the region then it arranges it according to the revenue.
filter(sales, margin > 1000000)

sales %>% filter(region == '01-East' & revenue > 400000) %>% select(partnum, region, revenue) #Filter according to our needs and display only the columns required & for AND and | for OR

names(sales)
?names
sales %>% group_by(region) %>% 
  summarize(Revenue = sum(revenue)) %>% arrange(desc(Revenue))

#install.packages('data.table')
library(data.table)
dt1 = as.data.table(sales)
dt2 = dt1[, sum(revenue), by=custname]
names(dt2)
dt1[, dt1[, sum(revenue), by=custname]]

dt1[, order(, decreasing = T)]

# Select
library(sqldf)
df5 =sqldf("Select custname, sum(revenue) from sales Group By custname order by sum(revenue) desc ")
head(df5)

# Freqency --------
names(sales)   #NAmes of column names
table(sales$custname) #How many times does a custmer name occurs in sales
T1=table(sales$custname)  #Make a tables according to customer name
head(T1)
T2=sort( T1,decreasing=T)  #Make a new tablr but in decreasing order 
head(T2)
head(sort(table(sales$custname), decreasing=T), n=10) #Most repeating customers list
tail(sort(table(sales$custname), decreasing=T), n=10)


#xtab
#
head(sort(xtabs(~ custname, sales), decreasing=T))
#
#
library(dplyr)
sales %>% dplyr::count(custname, sort=TRUE)   #Count the customer names and sorts

sales %>% dplyr::group_by(custname) %>% dplyr::summarise(n = n()) %>% dplyr::arrange(desc(n))



#plyr
df2a= plyr::count(sales, c('custname'))
str(df2a); names(df2a)
head(df2a[order(-df2a$freq),])



# Summarise by Part Num
df_=aggregate(sales$revenue , by=list(sales$partnum), FUN=sum) #First make a new data frame using total revenue grouped by the part number
head(df_[order(df_$x, decreasing=TRUE),], 5) #Now order the data from in descending order and display the top 5

df3a= aggregate(sales$revenue, by=list(sales$partnum), FUN=sum)
df3a
df3a[order(-df3a$x),][1:5,]

df3b = aggregate(revenue ~ partnum, data=sales, FUN=sum)
head(df3b)
head(df3b[order(df3b$revenue, decreasing=T),])

sales %>% dplyr::group_by(partnum) %>% dplyr::summarise(n = n()) %>% dplyr::arrange(desc(n))


# which parts have highest Profit : partno - sum(profit)
names(sales)
df4a = aggregate(margin ~ partnum, data=sales, FUN=sum)
head(df4a)

sales %>% group_by (partnum) %>% select(partnum,margin) %>% arrange(desc(margin))









