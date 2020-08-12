library(qqplotr)
library(ggplot2)
library(data.table)
library(car)
library(plyr)
library(tidyverse)
library(viridis)
library(forcats)
library(dplyr)
library(hrbrthemes)
library(ggpubr)
install.packages("gtools")
library(gtools)
install.packages("lubridate")
library(lubridate)
file<- "/home//qam/return_set15_19.csv"
returns <- fread(file)

file2<- "/home//qam/citrix/gender_label.csv"
gender<- fread(file2)


load("/home/qam/utrans2_2001_2019_reduced.rda")
setnames(utrans2, "date_trans", "close_date")
setnames(utrans2, "cur", "currency")
utrans2[,close_date := as.character(close_date,format="%Y-%m-%d")]

### data on buy 

#from 2015 to 2018

test1<- unique(utrans2[utrans2$action == "B" & utrans2$close_date >= "2015-01-01",])

test1.na<- na.omit(test1)
dt_merge<- merge(test1.na, gender, by = c("client"), all.x = T)
unique(dt_merge)
dt_mergeNew<-na.omit(dt_merge)

test2<-returns[returns$close_date <="2018-02-01",]


##### 
dt_mergeNew[,isin_code:=substr(isin,1,12)]
dt_mergeNew[,stock_key:=paste(isin_code,currency,sep='_')]
gc()

tmp2 <- merge(dt_mergeNew,test2, by = c("stock_key", "close_date"), all.y= TRUE)
tmp2.no.na <- drop_na(tmp2, title_label)

## from 2018

test3<- unique(utrans2[utrans2$action == "B" & utrans2$close_date >= "2018-02-01" ,])
test3.na<- na.omit(test3)
dt_merge2<- merge(test3.na, gender, by = c("client"), all.x = T)
unique(dt_merge2)
dt_mergeNew2<-na.omit(dt_merge2)
test4<-returns[returns$close_date >="2018-02-01 ",]
tmp3 <- merge(dt_mergeNew2,  test4, by = c("security_key", "close_date"), all.y= TRUE)
tmp3.no.na <- na.omit(tmp3)

#total set

total_data <-rbind.fill(tmp2.no.na, tmp3.no.na)

# regression on future returns over a day
# tmp2= 2015-2018
#tmp2 = 2018-2019
#total_data = 2015-2019

model1<- lm(fut_ret_measure_1 ~ factor(title_label) + ret_measure_250 + factor(title_label)*ret_measure_250, data = tmp2.no.na)
model2<- lm(fut_ret_measure_1 ~ factor(title_label) + ret_measure_250 + factor(title_label)*ret_measure_250, data = tmp3.no.na)
model3<- lm(fut_ret_measure_1 ~ factor(title_label) + ret_measure_250 + factor(title_label)* ret_measure_250, data = total_data)
summary(model1)
summary(model2)
summary(model3)
model1<- lm(fut_ret_measure_1 ~ factor(title_label) + ret_measure_70 + factor(title_label)*ret_measure_70, data = tmp2.no.na)
model2<- lm(fut_ret_measure_1 ~ factor(title_label) + ret_measure_70 + factor(title_label)*ret_measure_70, data = tmp3.no.na)
model3<- lm(fut_ret_measure_1 ~ factor(title_label) + ret_measure_70 + factor(title_label)* ret_measure_70, data = total_data)
summary(model1)
summary(model2)
summary(model3)

# regression on future returns over 5 days

model4<- lm(fut_ret_measure_5 ~ factor(title_label) + ret_measure_250 + factor(title_label)*ret_measure_250, data = tmp2.no.na)
model5<- lm(fut_ret_measure_5 ~ factor(title_label) + ret_measure_250 + factor(title_label)*ret_measure_250, data = tmp3.no.na)
model6<- lm(fut_ret_measure_5 ~ factor(title_label) + ret_measure_250 + factor(title_label)* ret_measure_250, data = total_data)
summary(model4)
summary(model5)
summary(model6)
model4<- lm(fut_ret_measure_1 ~ factor(title_label) + ret_measure_70 + factor(title_label)*ret_measure_70, data = tmp2.no.na)
model5<- lm(fut_ret_measure_1 ~ factor(title_label) + ret_measure_70 + factor(title_label)*ret_measure_70, data = tmp3.no.na)
model6<- lm(fut_ret_measure_1 ~ factor(title_label) + ret_measure_70 + factor(title_label)* ret_measure_70, data = total_data)
summary(model4)
summary(model5)
summary(model6)

# regression on future returns over 25 days

model7<- lm(fut_ret_measure_25 ~ factor(title_label) + ret_measure_250 + factor(title_label)* ret_measure_250, data = tmp2.no.na)
model8<- lm(fut_ret_measure_25 ~ factor(title_label) + ret_measure_250 + factor(title_label)*ret_measure_250, data = tmp3.no.na)
model9<- lm(fut_ret_measure_25 ~ factor(title_label) + ret_measure_250 + factor(title_label)* ret_measure_250, data = total_data)
summary(model7)
summary(model8)
summary(model9)
model7<- lm(fut_ret_measure_1 ~ factor(title_label) + ret_measure_70 + factor(title_label)*ret_measure_70, data = tmp2.no.na)
model8<- lm(fut_ret_measure_1 ~ factor(title_label) + ret_measure_70 + factor(title_label)*ret_measure_70, data = tmp3.no.na)
model9<- lm(fut_ret_measure_1 ~ factor(title_label) + ret_measure_70 + factor(title_label)* ret_measure_70, data = total_data)
summary(model7)
summary(model8)
summary(model9)


#regression on future retunrs over a 70 day period

model10<- lm(fut_ret_measure_70~ factor(title_label) + ret_measure_250 + factor(title_label) * ret_measure_250, data = tmp2.no.na)
model11<- lm(fut_ret_measure_70 ~ factor(title_label) + ret_measure_250 + factor(title_label)*ret_measure_250, data = tmp3.no.na)
model12<- lm(fut_ret_measure_70 ~ factor(title_label) + ret_measure_250 + factor(title_label)* ret_measure_250, data = total_data)
summary(model10)
summary(model11)
summary(model12)

model10<- lm(fut_ret_measure_70~ factor(title_label) + ret_measure_70 + factor(title_label) * ret_measure_70, data = tmp2.no.na)
model11<- lm(fut_ret_measure_70 ~ factor(title_label) + ret_measure_70 + factor(title_label)*ret_measure_70, data = tmp3.no.na)
model12<- lm(fut_ret_measure_70 ~ factor(title_label) + ret_measure_70 + factor(title_label)* ret_measure_70, data = total_data)
summary(model10)
summary(model11)
summary(model12)
#regression on future returns over a 250 day period

model13<-lm(fut_ret_measure_250 ~ factor(title_label) + ret_measure_250 + factor(title_label) * ret_measure_250, data = tmp2.no.na)
model14<- lm(fut_ret_measure_1 ~ factor(title_label) + ret_measure_250 + factor(title_label)*ret_measure_250, data = tmp3.no.na)
model15<- lm(fut_ret_measure_1 ~ factor(title_label) + ret_measure_250 + factor(title_label)* ret_measure_250, data = total_data)
summary(model13)
summary(model14)
summary(model15)
model13<-lm(fut_ret_measure_250 ~ factor(title_label) + ret_measure_70 + factor(title_label) * ret_measure_70, data = tmp2.no.na)
model14<- lm(fut_ret_measure_1 ~ factor(title_label) + ret_measure_70 + factor(title_label)*ret_measure_70, data = tmp3.no.na)
model15<- lm(fut_ret_measure_1 ~ factor(title_label) + ret_measure_70 + factor(title_label)* ret_measure_70, data = total_data)
summary(model13)
summary(model14)
summary(model15)

#graphs on returns 25

p<- ggplot(tmp2.no.na, aes(x = tmp2.no.na$fut_ret_measure_25, fill = tmp2.no.na$title_label, color = tmp2.no.na$title_label))+
  geom_histogram(aes(y=..density..), position = "identity", alpha = 0.5)

#Student T test
testvect<- unique(tmp2.no.na[title_label == "Mrs"])
volFemme<-c(testvect$fut_ret_measure_25)
testvect2<- unique(tmp2.no.na[title_label== "Mr"])
volHomme<-c(testvect2$fut_ret_measure_25)
t.test(volFemme,volHomme)
t.test(volFemme, volHomme,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)
median(volFemme)
median(volHomme)

# graphs on returns 5

p<- ggplot(tmp2.no.na, aes(x = tmp2.no.na$fut_ret_measure_5, fill = tmp2.no.na$title_label, color = tmp2.no.na$title_label))+
  geom_histogram(aes(y=..density..), position = "identity", alpha = 0.5)

#Student T test
testvect<- unique(tmp2.no.na[title_label == "Mrs"])
volFemme<-c(testvect$fut_ret_measure_5)
testvect2<- unique(tmp2.no.na[title_label== "Mr"])
volHomme<-c(testvect2$fut_ret_measure_5)
t.test(volFemme,volHomme)
t.test(volFemme, volHomme,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)

median(volFemme)
median(volHomme)
# graph on returns 70

p<-ggplot(tmp2.no.na, aes(x = tmp2.no.na$fut_ret_measure_70, color = tmp2.no.na$title_label, fill = tmp2.no.na$title_label))+
  geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5)

#student t test

testvect<- unique(tmp2.no.na[title_label == "Mrs"])
comFemme <- c(testvect$fut_ret_measure_70)
testvect2<- unique(tmp2.no.na[title_label == "Mr"])
comHomme<- c(testvect2$fut_ret_measure_70)
t.test(volFemme,volHomme)

median(comFemme)
median(comHomme)

#graph on returns 250

p<- ggplot(tmp2.no.na, aes( x = tmp2.no.na$fut_ret_measure_250, color = tmp2.no.na$title_label, fill = tmp2.no.na$title_label))+
  geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5)

#student t test

testvect<- unique(tmp2.no.na[title_label == "Mrs"])
comFemme<- c(testvect$fut_ret_measure_250)
testvect2<- unique(tmp2.no.na[title_label == "Mr"])
comHomme<- c(testvect2$fut_ret_measure_250)
t.test(comFemme, comHomme,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)
median(comFemme)
median(comHomme)

##### 
utrans2[,isin_code:=substr(isin,1,12)]
utrans2[,stock_key:=paste(isin_code,currency,sep='_')]
merge(utrans2_temp,returns,by.x )

### testing regressions on disposition effect 

# regression on past returns over a day

model1<- lm(ret_measure_1 ~ factor(title_label) + ret_measure_250 + factor(title_label)*ret_measure_250, data = tmp2.no.na)
model2<- lm(ret_measure_1 ~ factor(title_label) + ret_measure_250 + factor(title_label)*ret_measure_250, data = tmp3.no.na)
model3<- lm(ret_measure_1 ~ factor(title_label) + ret_measure_250 + factor(title_label)* ret_measure_250, data = total_data)
summary(model1)
summary(model2)
summary(model3)

# regression on past returns over 5 days

model4<- lm(ret_measure_5 ~ factor(title_label) + ret_measure_250 + factor(title_label)*ret_measure_250, data = tmp2.no.na)
model5<- lm(ret_measure_5 ~ factor(title_label) + ret_measure_250 + factor(title_label)*ret_measure_250, data = tmp3.no.na)
model6<- lm(ret_measure_5 ~ factor(title_label) + ret_measure_250 + factor(title_label)* ret_measure_250, data = total_data)
summary(model4)
summary(model5)
summary(model6)

# regression on past returns over 25 days

model7<- lm(ret_measure_25 ~ factor(title_label) + ret_measure_250 + factor(title_label)* ret_measure_250, data = total_data)
model8<- lm(ret_measure_25 ~ factor(title_label) + ret_measure_250 + factor(title_label)*ret_measure_250, data = tmp3.no.na)
model9<- lm(ret_measure_25 ~ factor(title_label) + ret_measure_250 + factor(title_label)* ret_measure_250, data = total_data)
summary(model7)
summary(model8)
summary(model9)
#regression on past retunrs over a 70 day period

model10<- lm(fut_ret_measure_70~ factor(title_label) + ret_measure_250 + factor(title_label) * ret_measure_250, data = tmp2.no.na)
model11<- lm(fut_ret_measure_70 ~ factor(title_label) + ret_measure_250 + factor(title_label)*ret_measure_250, data = tmp3.no.na)
model12<- lm(fut_ret_measure_70 ~ factor(title_label) + ret_measure_250 + factor(title_label)* ret_measure_250, data = total_data)
summary(model10)
summary(model11)
summary(model12)

### data on sales

test2<- unique(utrans2[utrans2$action == "S" & utrans2$close_date >= "2015-01-01",])

test2.na<- na.omit(test2)
dt_merge<- merge(test2.na, gender, by = c("client"), all.x = T)
unique(dt_merge)
dt_mergeNew<-na.omit(dt_merge)

test3<-returns[returns$close_date <="2018-02-01",]


##### 
dt_mergeNew[,isin_code:=substr(isin,1,12)]
dt_mergeNew[,stock_key:=paste(isin_code,currency,sep='_')]
gc()

tmp2 <- merge(dt_mergeNew,test3, by = c("stock_key", "close_date"), all.y= TRUE)
tmp2.no.na <- drop_na(tmp2, title_label)

## from 2018

test4<- unique(utrans2[utrans2$action == "S" & utrans2$close_date >= "2018-02-01" ,])
test4.na<- na.omit(test4)
dt_merge2<- merge(test4.na, gender, by = c("client"), all.x = T)
unique(dt_merge2)
dt_mergeNew2<-na.omit(dt_merge2)
test5<-returns[returns$close_date >="2018-02-01 ",]
tmp3 <- merge(dt_mergeNew2,  test5, by = c("security_key", "close_date"), all.y= TRUE)
tmp3.no.na <- na.omit(tmp3)

#total set

total_data3 <-rbind.fill(tmp2.no.na, tmp3.no.na)

### testing regressions on sales

# regression on past returns over a day

model1<- lm(ret_measure_1 ~ factor(title_label) + ret_measure_250 + factor(title_label)*ret_measure_250, data = tmp2.no.na)
model2<- lm(ret_measure_1 ~ factor(title_label) + ret_measure_250 + factor(title_label)*ret_measure_250, data = tmp3.no.na)
model3<- lm(ret_measure_1 ~ factor(title_label) + ret_measure_250 + factor(title_label)* ret_measure_250, data = total_data3)
summary(model1)
summary(model2)
summary(model3)

# regression on past returns over 5 days

model4<- lm(ret_measure_5 ~ factor(title_label) + ret_measure_250 + factor(title_label)*ret_measure_250, data = tmp2.no.na)
model5<- lm(ret_measure_5 ~ factor(title_label) + ret_measure_250 + factor(title_label)*ret_measure_250, data = tmp3.no.na)
model6<- lm(ret_measure_5 ~ factor(title_label) + ret_measure_250 + factor(title_label)* ret_measure_250, data = total_data3)
summary(model4)
summary(model5)
summary(model6)

# regression on past returns over 25 days

model7<- lm(ret_measure_25 ~ factor(title_label) + ret_measure_250 + factor(title_label)* ret_measure_250, data = tmp2.no.na)
model8<- lm(ret_measure_25 ~ factor(title_label) + ret_measure_250 + factor(title_label)*ret_measure_250, data = tmp3.no.na)
model9<- lm(ret_measure_25 ~ factor(title_label) + ret_measure_250 + factor(title_label)* ret_measure_250, data = total_data3)
summary(model7)
summary(model8)
summary(model9)
#regression on past retunrs over a 70 day period

model10<- lm(fut_ret_measure_70~ factor(title_label) + ret_measure_250 + factor(title_label) * ret_measure_250, data = tmp2.no.na)
model11<- lm(fut_ret_measure_70 ~ factor(title_label) + ret_measure_250 + factor(title_label)*ret_measure_250, data = tmp3.no.na)
model12<- lm(fut_ret_measure_70 ~ factor(title_label) + ret_measure_250 + factor(title_label)* ret_measure_250, data = total_data3)
summary(model10)
summary(model11)
summary(model12)


#### regressing using only the sign of returns for buys


testR<-returns[returns$close_date <="2018-02-01",]

##### 
dt_mergeNew[,isin_code:=substr(isin,1,12)]
dt_mergeNew[,stock_key:=paste(isin_code,currency,sep='_')]
gc()

tmpR <- merge(dt_mergeNew,testR, by = c("stock_key", "close_date"), all.y= TRUE)
tmpR.no.na <- drop_na(tmpR, title_label)

## from 2018

test3<- unique(utrans2[utrans2$action == "B" & utrans2$close_date >= "2018-02-01" ,])
test3.na<- na.omit(test3)
dt_merge2<- merge(test3.na, gender, by = c("client"), all.x = T)
unique(dt_merge2)
dt_mergeNew2<-na.omit(dt_merge2)
testR2<-returns[returns$close_date >="2018-02-01 ",]
tmpR3 <- merge(dt_mergeNew2,  testR2, by = c("security_key", "close_date"), all.y= TRUE)
tmpR3.no.na <- na.omit(tmpR3)

#total set

total_data4 <-rbind.fill(tmpR.no.na, tmpR3.no.na)
newTotalData<- copy(total_data4)

# changing values to 0's and one's

 x = c(newTotalData$ret_measure_250)
 x[x>0] <-1
 x[x<=0] <-0

 x<-x
 
 z = c(newTotalData$ret_measure_70)
 z[z>0] <-1
 z[z<=0]<-0
 
 z<- z
 
 ### adding these vectors to the data set
 
 new.df <- data.frame(
   +  x,
   + z)
 
 dataSet <- (cbind(newTotalData, new.df))
 
 
 ### regressions over 1 day
 
model3<- lm(fut_ret_measure_1 ~ factor(title_label) + x + factor(title_label)* x, data = dataSet)
summary(model3)
model3<- lm(fut_ret_measure_1 ~ factor(title_label) + z + factor(title_label)* z, data = dataSet)
summary(model3)

# regression on future returns over 5 days


model6<- lm(fut_ret_measure_5 ~ factor(title_label) + x + factor(title_label)* x, data = dataSet)
summary(model6)

model6<- lm(fut_ret_measure_5~ factor(title_label) + z+factor(title_label)* z, data = dataSet)
summary(model6)

# regression on future returns over 25 days


model9<- lm(fut_ret_measure_25 ~ factor(title_label) + x+ factor(title_label)*x, data = dataSet)
summary(model9)

model9<- lm(fut_ret_measure_25~ factor(title_label) + z+ factor(title_label)* z, data = dataSet)
summary(model9)

#regression on future retunrs over a 70 day period


model12<- lm(fut_ret_measure_70 ~ factor(title_label) + x+ factor(title_label)* x,data = dataSet)
summary(model12)
model12<- lm(fut_ret_measure_70 ~ factor(title_label) + z+ factor(title_label)* z,data = dataSet)
summary(model12)

#regression on future returns over a 250 day period


model15<- lm(fut_ret_measure_250~factor(title_label) + x+ factor(title_label)* x,data = dataSet)
summary(model15)
model15<- lm(fut_ret_measure_250~factor(title_label) + z+ factor(title_label)* z,data = dataSet)
summary(model15)


### Creating a dollar neutral portfolio 

#grouping data by months

D<-data.table(total_data)

months<-setDT(D)[, months_years := format(as.Date(c(D$close_date)), "%Y-%m") ]

totalData <- (cbind(D, months))
as.data.table(totalData)
table1<- unique(totalData[,. (client,title_label, months_years, isin)])

table1[,sumWomen:= .N , by = .(months_years,title_label)][title_label == "Mrs"]
table1[,sumMen:= .N , by = .(months_years,title_label)][title_label == "Mr"]

tmp = table1[,.(client, title_label, months_years, isin )]
tmp[, cnt:= .N , by = .(months_years,title_label, isin)]
tmp = tmp[,.(months_years, isin, title_label, cnt)]%>%unique()
tmp2 = dcast(tmp, months_years  + isin ~ title_label, value.var = 'cnt')
tmp2[is.na(tmp2)] <- 0

table2<- table1[,.(client, title_label, months_years, isin )]
table2[, count:= .N , by = .(months_years,title_label)]
table2 = table2[,.(months_years,title_label, count)]%>%unique()
newTable2 = dcast(table2, months_years   ~ title_label, value.var = 'count')
newTable2[is.na(newTable2)] <- 0

as.data.table(newTable2)


