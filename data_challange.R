#Loading the raw data
setwd("~/")
data <- read.csv("data.csv")
colnames(data)[1] <- "InvoiceNo"
#install.packages("sqldf")
library("sqldf")
list_words <- strsplit(as.character(data[,3]), " ")

#for removing characters in StockCode
#data1 <- data[!is.na(as.numeric(as.character(data$StockCode))),]
data1 <- data
data1$StockCode <- as.character(data1$StockCode) 
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

n <- substrRight(data1$StockCode,1)
data1 <- cbind(data1,n)
data1 <- data1[which(!is.na(as.numeric(as.character(data1$n)))=='TRUE'),]
#for cleaning Quantity Column
#for Cleaning CustomerID column removing NAs
attach(data1)
data2 <- data1[which(data1$Quantity >= 0 & is.na(data1$CustomerID) == FALSE),]
detach(data1)

#Fixing date
data2$Date <- as.Date(data2$InvoiceDate)
data2$Date <- strptime(as.character(data2$InvoiceDate), "%m/%d/%Y")
data2$Date <- format(data2$Date, "%Y-%m-%d")
#data2$Date <- as.Date(data2$InvoiceDate,format = "%m-%d-%Y")
#data2$InvoiceDate <- format(data2$InvoiceDate, "%Y-%m-%d")

#Imputing same StockCode to similar products
attach(data2)
data3 <- data2[order(data2$StockCode),]
detach(data2)

data3$code <- data3$StockCode
data3$code <- as.numeric(data3$code)
data3 <- data3[which(is.na(data3$code)==FALSE),]
l <- nrow(data3)
for(i in 1:(l-1))
{
   j <- data3[i,11] 
  if(abs(data3[i+1,11] - data3[i,11]) <= 2)
  {
    data3[i+1,11] <- j
  }
}

#data3 <- data3[1:363119,]

# for revenue ranking countrywise
DF <- sqldf("select Country,sum(Quantity*UnitPrice) as Revenue from data3 group by 1 order by Revenue desc") 

#install.packages("xlsx")
library("xlsx")
#write.xlsx(DF, "C:/Users/akmeh/Documents/New Folder/Revenue_ranking_country_wise.xlsx")

# for count of individual transcations and ranking countrywise

DF2 <- sqldf("select Country,count(distinct InvoiceNo) as Total_transaction_count from data3 group by 1 order by 2 desc") 
#write.xlsx(DF2, "C:/Users/akmeh/Documents/New Folder/Transaction_count_ranking_country_wise.xlsx")

# for count of customers and ranking countrywise

DF3 <- sqldf("select Country,count(distinct CustomerID) as Total_customer_count from data3 group by 1 order by 2 desc") 
#write.xlsx(DF3, "C:/Users/akmeh/Documents/New Folder/Customer_count_ranking_country_wise.xlsx")

# for transaction count month and yearwise in UK

#DF4 <- sqldf("select extract(month from Date) as Month, extract(year from Date) as Year,count(distinct InvoiceNo) as Total_transaction_count from data3 where Country in ('United Kingdom') group by 1,2 order by 1,2") 
data3$Month_Yr <- format(as.Date(data3$Date), "%Y-%m")
DF4 <- sqldf("select Month_Yr,count(distinct InvoiceNo) as Total_transaction_count from data3 where Country in ('United Kingdom') group by 1 order by 1") 

# Quantity bought across time 

#install.packages("reshape2")
library(reshape2)
y3 <- sqldf("select Month_Yr,Country,sum(Quantity) as total_quantity from  data3 group by 1,2")
w <- reshape(y3, 
             timevar = "Month_Yr",
             idvar = c("Country"),
             direction = "wide")
#write.xlsx(w, "C:/Users/akmeh/Documents/New Folder/Country_group_overall_quantity.xlsx")

library(reshape2)
y3 <- sqldf("select Month_Yr,Country,sum(Quantity*UnitPrice) as total_quantity from  data3 group by 1,2")
w <- reshape(y3, 
             timevar = "Month_Yr",
             idvar = c("Country"),
             direction = "wide")
write.xlsx(w, "C:/Users/akmeh/Documents/New Folder/Country_group_overall_rev.xlsx")

y4 <- sqldf("select Month_Yr,StockCode,sum(Quantity) as q from data3 where Country ='United Kingdom' group by 1,2 order by 2,1")
w2 <- reshape(y4, 
             timevar = "Month_Yr",
             idvar = c("StockCode"),
             direction = "wide")
#write.xlsx(w2, "C:/Users/akmeh/Documents/New Folder/StockCode_over_year.xlsx")

y5 <- sqldf("select Month_Yr,code,sum(Quantity) as q from data3 where Country ='United Kingdom' group by 1,2 order by 2,1")
w3 <- reshape(y5, 
              timevar = "Month_Yr",
              idvar = c("code"),
              direction = "wide")
#write.xlsx(w3, "C:/Users/akmeh/Documents/New Folder/StockCode_over_year_PC.xlsx")

y6 <- sqldf("select distinct code,Description from data3")
#write.xlsx(y6, "C:/Users/akmeh/Documents/New Folder/Code_Description.xlsx")

y7 <- sqldf("select distinct StockCode,Description from data3")
#write.xlsx(y7, "C:/Users/akmeh/Documents/New Folder/StockCode_Description.xlsx")

#For calcualting moving average in 3 months window for Demand Variability
l <- w3[,c(1,2,3,4,5,6,13,7,8,9,10,11,12,14)]
var <- NA
count1 <- NA
for(i in 1:1161)
{ var[i] = 0
count1[i] = 0
avg <- NA
for (j in 2:11)
{ f <- 0
s<- 0
t <- 0
p <- 0

f <- ifelse((is.na(l[i,j]) == 'FALSE'),l[i,j],0)
s <- ifelse((is.na(l[i,j+1]) == 'FALSE'),l[i,j+1],0)
t <- ifelse((is.na(l[i,j+2]) == 'FALSE'),l[i,j+2],0)

avg[j] <- (f+s+t)/3

a <- ifelse((is.na(l[i,j+1]) == 'FALSE'),l[i,j+1],0)
b <- ifelse((is.na(l[i,j+2]) == 'FALSE'),l[i,j+2],0)
c <- ifelse((is.na(l[i,j+3]) == 'FALSE'),l[i,j+3],0)

avg[j+1] <- (a+b+c)/3

if (avg[j] != 0 ) 
{
  p <- abs(avg[j+1]-avg[j])/avg[j]
  #print(p)
  var[i] <- var[i] + p
  #print(var[i])
  count1[i] <- count1[i] + 1
}
}
}
change <- NA
for(i in 1:1161)
{
  change[i] <- var[i]/(count1[i])
}
m <- cbind(l,change)
#write.xlsx(m, "C:/Users/akmeh/Documents/New Folder/demand_variability_mov_avg.xlsx")

y8 <- sqldf("select count(distinct CustomerID)  from data3 where Country ='United Kingdom'")

y9 <- sqldf("select Month_Yr,CustomerID,sum(UnitPrice*Quantity) as r from data3 where Country ='United Kingdom' group by 1,2 order by 2,1")
w5 <- reshape(y9, 
              timevar = "Month_Yr",
              idvar = c("CustomerID"),
              direction = "wide")
#write.xlsx(w5, "C:/Users/akmeh/Documents/New Folder/Cust_cn_over_year_PC.xlsx")

y10 <- sqldf("select CustomerID,count(distinct InvoiceNo) as i from data3 where Country ='United Kingdom' group by 1")
#write.xlsx(y10, "C:/Users/akmeh/Documents/New Folder/Cust_no_of_bills.xlsx")

y11 <- sqldf("select count(distinct StockCode)  from data3 where Country ='United Kingdom'")

# For analysing revenue trend across differently-priced items
y12 <- sqldf("select Month_Yr,StockCode,UnitPrice,sum(UnitPrice*Quantity) as r from data3 where Country ='United Kingdom' group by 1,2 order by 2,1")
w6 <- reshape(y12, 
              timevar = "Month_Yr",
              idvar = c("StockCode","UnitPrice"),
              direction = "wide")
write.xlsx(w6, "C:/Users/akmeh/Documents/New Folder/goods_rev_over_year_PC.xlsx")