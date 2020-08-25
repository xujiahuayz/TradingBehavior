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


rm(list=ls())
load("/home/qam/Sofiya/Data/computed_returns_2010_2020.rda")
load("/home/qam/utrans2_2001_2019_reduced.rda")
file2<- "/home/qam/citrix/gender_label.csv"
gender<- fread(file2)

setnames(utrans2, "date_trans", "close_date")
setnames(utrans2, "cur", "currency")

test1<- unique(utrans2[utrans2$action == "B" & utrans2$close_date >= "2010-01-01",])

test1.na<- na.omit(test1)
dt_merge<- merge(test1.na, gender, by = c("client"), all.x = T)
unique(dt_merge)
dt_mergeNew<-na.omit(dt_merge)
dt_mergeNew[,isin_code:=substr(security_key,1,12)]
dt_mergeNew[,stock_key:=paste(isin_code,currency,sep='_')]

return_data<- na.omit(Return_data)

setnames(return_data, "security_key", "stock_key")

tot_data<- merge(dt_mergeNew,  return_data, by = c("stock_key", "close_date"), all.y= TRUE)
total_data<- na.omit(tot_data)
data<-total_data[!ret_measure_250 %in% c(-Inf, Inf)]
dataTot<-data[!fut_ret_measure_250 %in% c(-Inf, Inf)]

## Do women like momentum

model2 <- lm(ret_measure_25~ factor(title_label), data = dataTot)

model4<- lm(ret_measure_70~factor(title_label), data = dataTot)

model6<- lm(ret_measure_250~factor(title_label), data =dataTot)


## Regressing future returns using 70 day momentum

model1<- lm(fut_ret_measure_1 ~ factor(title_label) + ret_measure_70 + factor(title_label)*ret_measure_70, data = dataTot)
model2<- lm(fut_ret_measure_5~ factor(title_label) + ret_measure_70 + factor(title_label)*ret_measure_70, data = dataTot)
model3<- lm(fut_ret_measure_25~ factor(title_label) + ret_measure_70 + factor(title_label)*ret_measure_70, data = dataTot)
model4<- lm(fut_ret_measure_70~ factor(title_label) + ret_measure_70 + factor(title_label)*ret_measure_70, data = dataTot)
model5<- lm(fut_ret_measure_250~ factor(title_label) + ret_measure_70 + factor(title_label)*ret_measure_70, data = dataTot)


##Regressing future returns using 250 day momentum

model6<- lm(fut_ret_measure_1 ~ factor(title_label) + ret_measure_250 + factor(title_label)*ret_measure_250, data = dataTot)
model7<- lm(fut_ret_measure_5~ factor(title_label) + ret_measure_250 + factor(title_label)* ret_measure_250, data = dataTot)
model8<- lm(fut_ret_measure_25 ~ factor(title_label) + ret_measure_250 + factor(title_label)* ret_measure_250, data = dataTot)
model9<- lm(fut_ret_measure_70 ~ factor(title_label) + ret_measure_250 + factor(title_label)* ret_measure_250, data = dataTot)
model10<- lm(fut_ret_measure_250~ factor(title_label) + ret_measure_250 + factor(title_label)* ret_measure_250, data = dataTot)


##Regressing on sales

test2<- unique(utrans2[utrans2$action == "S" & utrans2$close_date >= "2010-01-01",])

test2.na<- na.omit(test2)
dt_merge2<- merge(test2.na, gender, by = c("client"), all.x = T)
unique(dt_merge2)
dt_mergeNew2<-na.omit(dt_merge2)
dt_mergeNew2[,isin_code:=substr(security_key,1,12)]
dt_mergeNew2[,stock_key:=paste(isin_code,currency,sep='_')]


tot_data2<- merge(dt_mergeNew,  return_data, by = c("stock_key", "close_date"), all.y= TRUE)
total_data2<- na.omit(tot_data)
data2<-total_data2[!ret_measure_250 %in% c(-Inf, Inf)]
dataTot2<-data2[!fut_ret_measure_250 %in% c(-Inf, Inf)]


## Do women sell winners

model1 <- lm(ret_measure_25~ factor(title_label), data = dataTot2)
model2<- lm(ret_measure_70 ~ factor(title_label), data = dataTot2)
model3<- lm(ret_measure_250~ factor(title_label), data = dataTot2)


### Regressing on future returns only using past return signs

newTotalData<- copy(dataTot)


# changing values to 0's and one's


ret_250 = c(newTotalData$ret_measure_250)
ret_250[ret_250>0] <-1
ret_250[ret_250<=0] <-0


ret_250<-ret_250

ret_70 = c(newTotalData$ret_measure_70)
ret_70[ret_70>0] <-1
ret_70[ret_70<=0]<-0

ret_70<- ret_70

### adding these vectors to the data set

new.df <- data.frame(
  +  ret_250,
  + ret_70)

dataSet <- (cbind(newTotalData, new.df))

##regressing using 250 day momentum on buys

model1<- glm(fut_ret_measure_1 ~ factor(title_label) + ret_250 + factor(title_label)* ret_250, data = dataSet)
model2<- glm(fut_ret_measure_5 ~ factor(title_label) + ret_250 + factor(title_label)* ret_250, data = dataSet)
model3<- glm(fut_ret_measure_25 ~ factor(title_label) + ret_250 + factor(title_label)* ret_250, data = dataSet)
model4<- glm(fut_ret_measure_70 ~ factor(title_label) + ret_250 + factor(title_label)*ret_250, data = dataSet)
model5<- glm(fut_ret_measure_250 ~ factor(title_label) + ret_250 + factor(title_label)*ret_250, data = dataSet)

## regressing using 70 day momentum on sells

model6<- glm(fut_ret_measure_1 ~ factor(title_label) + ret_70 + factor(title_label)*ret_70, data = dataSet)
model7<- glm(fut_ret_measure_5 ~ factor(title_label) + ret_70 + factor(title_label)*ret_70, data = dataSet)
model8<- glm(fut_ret_measure_25~ factor(title_label) + ret_70 + factor(title_label)*ret_70, data = dataSet)
model9<- glm(fut_ret_measure_70 ~ factor(title_label) + ret_70 + factor(title_label)* ret_70, data = dataSet)
model10<- glm(fut_ret_measure_250 ~ factor(title_label) + ret_70 + factor(title_label)*ret_70, data = dataSet)


