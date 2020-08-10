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

file<- "/home//qam/return_set15_19.csv"
returns <- fread(file)

file2<- "/home//qam/citrix/gender_label.csv"
gender<- fread(file2)

unique(returns)
load("/home/qam/utrans2_2001_2019_reduced.rda")
setnames(utrans2, "date_trans", "close_date")
setnames(utrans2, "cur", "currency")
utrans2[,close_date := as.character(close_date,format="%Y-%m-%d")]
test1<- unique(utrans2[utrans2$close_date >= "2015-01-01",])

test1.na<- na.omit(test1)
dt_merge<- merge(test1.na, gender, by = c("client"), all.y = T)
unique(dt_merge)
dt_mergeNew<-na.omit(dt_merge)
test2<- unique(returns[returns$close_date <= "2019-09-27",])
test3<-na.omit(test2)

dt_mergeTot<- merge(dt_mergeNew, test3, by= c("close_date",  "stock_exchange"), all.y=TRUE)

gc()

tmp2 <- merge(dt_mergeNew,  test3, by = c("security_key", "close_date"), all.x = TRUE)
tmp2.no.na<- na.omit(tmp2)

# regression on future returns over a day

model<- lm(fut_ret_measure_1 ~ factor(title_label) + ret_measure_250 + factor(title_label)*ret_measure_250, data = tmp2.no.na)
summary(model)

# regression on future returns over 5 days

model2<- lm(fut_ret_measure_5 ~ factor(title_label) + ret_measure_250 + factor(title_label)*ret_measure_250, data = tmp2.no.na)
summary(model2)

# regression on future returns over 25 days

model3<- lm(fut_ret_measure_25 ~ factor(title_label) + ret_measure_250 + factor(title_label)* ret_measure_250, data = tmp2.no.na)
summary(model3)

#regression on future retunrs over a 70 day period

model4<- lm(fut_ret_measure_70~ factor(title_label) + ret_measure_250 + factor(title_label) * ret_measure_250, data = tmp2.no.na)
summary(model4)

#regression on future returns over a 250 day period

model5<-lm(fut_ret_measure_250 ~ factor(title_label) + ret_measure_250 + factor(title_label) * ret_measure_250, data = tmp2.no.na)
summary(model5)

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
