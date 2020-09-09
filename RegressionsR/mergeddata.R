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
load("/home/qam/Sofiya/price_ret_sd_1year.rda")

file<- "/home//qam/Sofiya/Data/utrans_2019.csv"
utrans_2019 <- fread(file)

file<- "/home//qam/sell_action_properties18-19.csv"
sell_action <- fread(file)

test1<-unique(utrans_2019[aclass == "Shares",.(security_key,client, gender,trade_id,isin,aclass,asset_type,date_trans,action)])

file<- "/home//qam/buy_action_properties18-19.csv"
buy_action <- fread(file)

setnames(PRSD_data,"close_date","date_trans")
PRSD_data[,date_trans := as.character(date_trans,format="%Y-%m-%d")]
dt_merge<- merge(PRSD_data,test1, by= c("security_key","date_trans"), all.y=T)

data<- unique(buy_action[,. (security_key, date_trans, ret_1,ret_25)])
dt_mergeTot<- merge(dt_merge,data, by =c("security_key", "date_trans"), all.y =T)

data2<- unique(sell_action[,. (security_key, date_trans, ret_1,ret_25)])
dt_mergeTot2<- merge(dt_merge,data2, by =c("security_key", "date_trans"), all.y =T)


#create the data table

test<- unique(dt_mergeTot[,. (client, security_key, date_trans,ret_measure_252, ret_1,gender , ret_25)])
test2<- unique(dt_mergeTot2[,. (client, security_key,date_trans, ret_25, gender)])

#remove NA's

test_purged <- na.omit(test) 
test_purged2<- na.omit(test2)
model<- lm(ret_1 ~ factor(gender) + ret_measure_252 + factor(gender)*ret_measure_252, data = test_purged)

model2 <- lm(ret_25~ factor(gender), data = test_purged)

model3<- lm(ret_25~factor(gender),data = test_purged2 )

