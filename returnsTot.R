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


##heatmap

##creating a vector with the coefficient estimates for 250 past momentum

coefficient1<- coef(model6)
coefficient2<- coef(model7)
coefficient3<- coef(model8)
coefficient4<-coef(model9)
coefficient5<- coef(model10)

coeffs<- c(coefficient1[4], coefficient2[4], coefficient3[4], coefficient4[4], coefficient5[4])

###creating a vector with the coefficient estimates for 70 day momentum

coeff1<- coef(model1)
coeff2<- coef(model2)
coeff3<- coef(model3)
coeff4<- coef(model4)
coeff5<- coef(model5)

coefficients<- c(coeff1[4], coeff2[4], coeff3[4], coeff4[4], coeff5[4])


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
ret_250[ret_250<=0] <- -1


ret_250<-ret_250

ret_70 = c(newTotalData$ret_measure_70)
ret_70[ret_70>0] <-1
ret_70[ret_70<=0]<- -1

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

D<-data.table(dataTot)

months<-setDT(D)[, months_years := format(as.Date(c(D$close_date)), "%Y-%m") ]

totalData <- (cbind(D, months))
as.data.table(totalData)
table1<- unique(totalData[,. (client,title_label, months_years, isin, ret_measure_1, ret_measure_5, fut_ret_measure_5, ret_measure_25, fut_ret_measure_25, ret_measure_70, ret_measure_250, fut_ret_measure_250,fut_ret_measure_1,fut_ret_measure_70)])

table1[,sumWomen:= .N , by = .(months_years,title_label)][title_label == "Mrs"]
table1[,sumMen:= .N , by = .(months_years,title_label)][title_label == "Mr"]

tmp = unique(table1[,.(client, title_label, months_years, isin)])
tmp[, cnt:= .N , by = .(months_years,title_label, isin)]


tmp = tmp[,.(months_years, isin, title_label, cnt)]%>%unique()
tmp2 = dcast(tmp, months_years  + isin ~ title_label, value.var = c('cnt'))
setnames(tmp2, "Mr", "count_Mr")
setnames(tmp2, "Mrs", "count_Mrs")


table2<- unique(table1[,.(client, title_label, months_years, ret_measure_1, ret_measure_5, fut_ret_measure_5, ret_measure_25, fut_ret_measure_25, ret_measure_70, ret_measure_250, fut_ret_measure_250,fut_ret_measure_1,fut_ret_measure_70)])
table2[, count:= .N , by = .(months_years,title_label)]
table2 = table2[,.(months_years,title_label, count, return_1, return_5, return_25, return_70, return_250, fut_return_1, fut_return_5, fut_return_25, fut_return_70, fut_return_250)]%>%unique()
newTable2 = dcast(table2, months_years   ~ title_label, value.var = ('count'))


as.data.table(newTable2)

#data table with future and past returns 
totalTable<- merge(tmp2, newTable2, by= ("months_years"), all.x = T)
datTotalData<- data.table(totalData)
#au lieu d'unique conditionner sur la date sur laquelle on prend le retour
#max date by years_months ans close date = max date
returns_fut_past<- datTotalData[,. (fut_ret_measure_25, ret_measure_25,months_years, isin)]
#setnames(returns_fut_past, "isin_code", "isin")
totalTable2 <-merge(totalTable, returns_fut_past, by = c("isin", "months_years"), all.x = T)

totalTable2[, division_fem := (count_Mrs/Mrs), by=. (months_years)]
totalTable2[, division_hom := (count_Mr/Mr), by = .(months_years)]

totalTable2[, demeaned_fraction_Mr := division_hom - mean(division_hom, na.rm=TRUE)]
totalTable2[, demeaned_fraction_Mrs := division_fem - mean (division_fem, na.rm = TRUE)]

#calculate quantiles by months
totalTable2[, q1_Mr := quantile(division_hom,0.2, na.rm =TRUE), by=.(months_years) ]
totalTable2[, q2_Mr := quantile(division_hom,0.8, na.rm =TRUE), by=.(months_years) ]
totalTable2[, q1_Mrs := quantile(division_fem,0.2, na.rm =TRUE), by=.(months_years) ]
totalTable2[, q2_Mrs := quantile(division_fem,0.8, na.rm =TRUE), by=.(months_years) ]

totalTable2[, indicator1_hom:= ifelse(division_hom<q1_Mr,1,0)]
totalTable2[, indicator2_hom:= ifelse(division_hom>q2_Mr,1,0)]
totalTable2[, indicator1_fem:= ifelse(division_fem<q1_Mrs,1,0)]
totalTable2[, indicator2_fem:= ifelse(division_fem>q2_Mrs,1,0)]
totalTable3.na <- drop_na(totalTable2, c(indicator1_hom, indicator1_fem, indicator2_hom, indicator2_fem,fut_ret_measure_25,ret_measure_25))

totalTable3.na[, portf_simple_1_hom:= sum(fut_ret_measure_25*demeaned_fraction_Mr), by =.(months_years)]
totalTable3.na[, portf_simple_1_fem:= sum(demeaned_fraction_Mrs*fut_ret_measure_25), by =.(months_years)]
totalTable3.na[, portf_ret_1_hom:= sum((division_hom*indicator2_hom*fut_ret_measure_25-(division_hom*indicator1_hom*fut_ret_measure_25))), by =.(months_years)]
totalTable3.na[, portf_ret_1_fem:= sum((division_fem*indicator2_fem*fut_ret_measure_25 -(division_fem*indicator1_fem*fut_ret_measure_25))), by =.(months_years)]
totalTable3.na[, portf_ret_2_hom:= sum((indicator2_hom*division_hom *(ret_measure_25 >0)-(division_hom*indicator1_hom *(ret_measure_25<0)*fut_ret_measure_25))), by =.(months_years)]
totalTable3.na[, portf_ret_2_fem:= sum((indicator2_fem*division_fem *(ret_measure_25 >0)-(division_fem*indicator1_fem *(ret_measure_25<0)*fut_ret_measure_25))), by =.(months_years)]


