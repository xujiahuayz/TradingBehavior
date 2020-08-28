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
library(stargazer)


rm(list=ls())
load("/home/qam/computed_returns_at_utrans.rda")
load("/home/qam/utrans2_2001_2019_reduced.rda")
file2<- "/home/qam/citrix/gender_label.csv"
gender<- fread(file2)

setnames(utrans2, "cur", "currency")

test1<- unique(utrans2[utrans2$action == "B" & utrans2$date_trans >= "2010-01-01",])

test1.na<- na.omit(test1)
dt_merge<- merge(test1.na, gender, by = c("client"), all.x = T)
unique(dt_merge)
dt_mergeNew<-na.omit(dt_merge)
dt_mergeNew[,isin_code:=substr(security_key,1,12)]
dt_mergeNew[,stock_key:=paste(isin_code,currency,sep='_')]

return_data<- na.omit(Return_data)

tot_data<- merge(dt_mergeNew,  return_data, by = c("stock_key", "date_trans"), all.y= TRUE)
total_data<- na.omit(tot_data)
data<-total_data[!ret_measure_1_week %in% c(-Inf, Inf,NA,NaN)]
dataTot<-data[!fut_ret_measure_1_week %in% c(-Inf, Inf,NA,NaN)]






u<- colnames(dataTot%>% select(contains("measure")))

l1<- sprintf(paste0('ret_measure_', c(1:52), '_week'))
l2<- sprintf(paste0('fut_ret_measure_', c(1:52), '_week'))

table<- expand.grid(l1,l2)
table<- as.data.table(table)

setnames(table, "Var1", "past")
setnames(table, "Var2", "fut")

table2<- data.frame(lapply(table, as.character), stringsAsFactors=FALSE)
table<- as.data.table(table2)


get_regression_results <- function(x){
  reg = lm(x, data=dt_test)
  coeff_list = as.numeric(coefficients(reg))
  smr_rez = data.table(summary(reg)$coefficients)
  rez_list_1 = paste0('(', paste(round(smr_rez$Estimate,4), collapse = ','), ')')
  rez_list_2 = paste0('(', paste(round(smr_rez$`t value`, 4), collapse = ','), ')')
  return (paste0(rez_list_1, ',', rez_list_2))
}


dt_test = dataTot[1:100]

table[, formula:= paste0(fut, '~', past, ' + factor(title_label) + factor(title_label) * ',past )]

table[, rez:= apply(table[,.(formula)], 1, function(x){get_regression_results(x)  })] 
table

table$estimate1= as.character(lapply(strsplit(as.character(table$rez), split=","), "[", 1))
table$estimate2= as.character(lapply(strsplit(as.character(table$rez), split=","), "[", 2))
table$estimate3= as.character(lapply(strsplit(as.character(table$rez), split=","), "[", 3))
table$estimate4= as.character(lapply(strsplit(as.character(table$rez), split=","), "[", 4))
table$tvalue1 = as.character(lapply(strsplit(as.character(table$rez), split=","), "[", 5))
table$tvalue2 = as.character(lapply(strsplit(as.character(table$rez), split=","), "[", 6))
table$tvalue3 = as.character(lapply(strsplit(as.character(table$rez), split=","), "[", 7))
table$tvalue4 = as.character(lapply(strsplit(as.character(table$rez), split=","), "[", 8))
table$estimate4 = substr(table$estimate4,1,nchar(table$estimate4)-1)
table$tvalue4 = substr(table$tvalue4,1,nchar(table$tvalue4)-1)
table$estimate1 <- substring(table$estimate1, 2)
table$tvalue1 <- substring(table$tvalue1, 2 )

 transform(table, estimate1 = as.numeric(estimate1),
           estimate2 = as.numeric(estimate2), 
          estimate3 = as.numeric(estimate3),
           estimate4 = as.numeric(estimate4), 
           tvalue1 = as.numeric(tvalue1),
           tvalue2 = as.numeric(tvalue2),
           tvalue3 = as.numeric(tvalue3),
           tvalue4 = as.numeric(tvalue4)
           )
