# load library
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
library(tidyr)

# import data
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


################################################
##                                            ##  
##      creaing a dollar neutral portfolio    ##
##      using buy data                        ##
################################################

### Creating a data set containing all the necessary data by combining data sets


D<-data.table(dataTot)

months<-setDT(D)[, months_years := format(as.Date(c(D$close_date)), "%Y-%m") ]

totalData <- (cbind(D, months))
as.data.table(totalData)
table1<- unique(totalData[,. (vol_chf_trans,client,title_label, months_years, isin, ret_measure_1, ret_measure_5, fut_ret_measure_5, ret_measure_25, fut_ret_measure_25, ret_measure_70, ret_measure_250, fut_ret_measure_250,fut_ret_measure_1,fut_ret_measure_70)])

### Creating columns containing the number of buys per gender and month
### Creating column containing number of buys per month for all clients

# @param 
# @sumWomen
# @sumMen
# @sumClients

### Creating columns containing the number of buys per gender, month and isin
# @param Mr = number of male buys per month
# @param Mrs = number of female buys per month
# @param count_Mr = number of male buys per month and isin
# @param count_Mrs = number of female buys per month and isin
# @param sumClients = total number of buys per month and isin

tmp = unique(table1[,.(client, title_label, months_years, isin, vol_chf_trans)])
tmp[, cnt:= .N , by = .(months_years,title_label, isin,client)]

tmp = tmp[,.(months_years, isin, title_label, cnt,client,vol_chf_trans)]%>%unique()
tmp4 = dcast(tmp, months_years  + isin ~ title_label, value.var = c('cnt'))
setnames(tmp4, "Mr", "count_mr_transactions_per_isin_date")
setnames(tmp4,"Mrs", "count_mrs_transactions_per_isin_date")
tmp4[, sumClients:= (count_mr_transactions_per_isin_date+count_mrs_transactions_per_isin_date) , by = .(months_years, isin)]
tmp6<- merge(tmp4, tmp, by =c("months_years","isin"), all.x =T)


###Combining old data and new data containing the new columns 

table2<- unique(table1[,.(client, title_label, months_years, ret_measure_1, ret_measure_5, fut_ret_measure_5, ret_measure_25, fut_ret_measure_25, ret_measure_70, ret_measure_250, fut_ret_measure_250,fut_ret_measure_1,fut_ret_measure_70)])
table2[, count:= .N , by = .(months_years,title_label)]
table2 = table2[,.(months_years,title_label, count, ret_measure_1, ret_measure_5, fut_ret_measure_5, ret_measure_25, fut_ret_measure_25, ret_measure_70, ret_measure_250, fut_ret_measure_250,fut_ret_measure_1,fut_ret_measure_70)]%>%unique()
newTable2 = dcast(table2, months_years ~ title_label, value.var = ('count'))
setnames(newTable2, "Mr", "count_mr_transactions_per_date")
setnames(newTable2,"Mrs", "count_mrs_transactions_per_date")

as.data.table(newTable2)

#data table with future and past returns 
totalTable<- unique(merge(tmp6, newTable2, by= ("months_years"), all.x = T))
datTotalData<- data.table(totalData)

returns_fut_past<-datTotalData[,. (ret_measure_1, ret_measure_5, fut_ret_measure_5, ret_measure_25, fut_ret_measure_25, ret_measure_70, ret_measure_250, fut_ret_measure_250,fut_ret_measure_1,fut_ret_measure_70,months_years, isin, client, vol_chf_trans)]
#setnames(returns_fut_past, "isin_code", "isin")
totalTable2 <-merge(totalTable, returns_fut_past, by = c("isin", "months_years", "client", "vol_chf_trans"), all.x = T)

###Creating new columns to compute future indicators

# @param division_fem = number of buys per isin per month divided by the number of buys per month, for female clients
# @param division_hom = number of buys per isin per month divided by the number of buys per month, for male clients
# @param division_fem_by_clients = number of buys per month for female clients divided by the number of buys per month for all clients
# @param division_hom_by_clients = number of buys per month for male clients divided by the number of buys per month for all clients


totalTable2[, division_fem := (count_mrs_transactions_per_isin_date/count_mrs_transactions_per_date), by=. (months_years)]
totalTable2[, division_hom := (count_mr_transactions_per_isin_date/count_mr_transactions_per_date), by = .(months_years)]
totalTable2[, division_fem_by_clients := (count_mrs_transactions_per_isin_date/sumClients), by = .(months_years,isin)]
totalTable2[, division_hom_by_clients := (count_mr_transactions_per_isin_date/sumClients), by = .(months_years,isin)]
finalTable<- na.omit(totalTable2)

dollar.neutral.portfolio <- drop_na(finalTable, c(ret_measure_1, ret_measure_5, fut_ret_measure_5, ret_measure_25, fut_ret_measure_25, ret_measure_70, ret_measure_250, fut_ret_measure_250,fut_ret_measure_1,fut_ret_measure_70))

####Creating a dollar neutral portfolio containing only unique combinations of dates and isins

merged_colmns_isin_months_years<- cbind(dollar.neutral.portfolio$isin, dollar.neutral.portfolio$months_years)
merged_table<-as.data.table(merged_colmns_isin_months_years)

setnames(merged_table, "V2", "months_years")
merged_table$Seq_modif <- paste(merged_table$V1, merged_table$months_years)
setnames(merged_table, "V1", "isin")
setnames(merged_table, "Seq_modif","combined_dates_months")

merged_table1<- unique(merged_table)


dt_merge1<- merge(dollar.neutral.portfolio,merged_table1 , by = c("isin", "months_years"), all.x =T)
unique_dollar.neutral.portfolio <- dt_merge1[match(unique(dt_merge1$combined_dates_months), dt_merge1$combined_dates_months),]

#compute quantiles by months

d <-list('division_hom', 'division_fem', 'division_hom_by_clients','division_fem_by_clients' )

# We take the lowest and highest twenty percent 
c_of_quantiles = c(0.2, 0.8)
quant<-"quant"

# Returns quantiles with respect to division indicators per month

for (col in d){
  
  for (quantile1 in c_of_quantiles) {
    
    name = col
    
    unique_dollar.neutral.portfolio[,  paste(quant, name,quantile1,sep='_')  := quantile(get(col), quantile1), by=.(months_years)]
    
  }
}

### Create indicators based on the computed quantiles
# indicators represent values bigger ans smaller than the previosly computed quantiles

high_indicators <- list('quant_division_hom_0.8', 'quant_division_fem_0.8', 'quant_division_hom_by_clients_0.8', 'quant_division_fem_by_clients_0.8')
low_indicators <- list('quant_division_hom_0.2', 'quant_division_fem_0.2', 'quant_division_hom_by_clients_0.2', 'quant_division_fem_by_clients_0.2')

high_ind = "high_indicator"
low_ind = "low_indicator"
list_length = length( high_indicators)

# Returns whether a division indicator is higher than it's quantile 
# during the month it is computed for 

computing_high_indicators <- function(division, quantile){
  
  ifelse(division >= quantile ,1,0)
}

# Returns whether a division indicator is lower than it's quantile 
# during the month it is computed for 

computing_low_indicators <- function(division, quantile){
  
  ifelse(division <= quantile ,1,0)
}

# Returns new columns with indicators 
# representing if a division indicator is higher or lower
# than the computed quantiles

for(i in 1: list_length){
  
  high_indicator = high_indicators[[i]]
  low_indicator = low_indicators[[i]]
  col = d[[i]]
  name = col
  
  unique_dollar.neutral.portfolio[,  paste(high_ind, name,sep='_')  := computing_high_indicators(get(col), get(high_indicator))]
  unique_dollar.neutral.portfolio[, paste(low_ind, name,sep='_') := computing_low_indicators(get(col), get(low_indicator)) ]
  
}

unique_dollar.neutral.portfolio[, indicator_return_pos := ifelse(ret_measure_25>0, 1,0)]
unique_dollar.neutral.portfolio[, indicator_return_neg := ifelse(ret_measure_25<0, 1,0)]

### Find misatke in demeaned fractions by clients

### Demeaning the previous @param

## @param
# @ mean_mr/mean_mrs = mean division_hom/mean division_fem
# @ mean_clients_mr/mean_clients_mrs = mean division_hom_by_clients/mean division_fem_by_clients

mean = "mean"
demeaned = "demeaned"

for (col in d){
  
  name = col
  
  unique_dollar.neutral.portfolio[,  paste(mean, name,sep='_')  := mean(get(col)), by=.(months_years)]
  unique_dollar.neutral.portfolio[, paste(demeaned, name, sep = '_') := get(col) - mean(get(col)), by =.(months_years)]
  
}

# We build this to be able to see the interaction effect between gender and return signals

computing_types <- function(division, indicator){
  type = division * unique_dollar.neutral.portfolio$ret_measure_25 * indicator
  return(type)
}

type_list_fem <- list('high_indicator_division_fem', 'low_indicator_division_fem')
type_list_hom <- list('high_indicator_division_hom', 'low_indicator_division_hom')
type_returns_list <- list('indicator_return_pos', 'indicator_return_neg')
type = "type"


# Returns new columns containing information on
# interaction effect between gender and return signals

for (i in 1:length(type_list_fem)){
  
  type_fem = type_list_fem[[i]]
  type_hom = type_list_hom[[i]]
  type_returns = type_returns_list[[i]]
  
  unique_dollar.neutral.portfolio[,  paste(type,type_fem ,sep = "_")  := computing_types(division_fem, get(type_fem))]
  unique_dollar.neutral.portfolio[,  paste(type,type_hom, sep = "_")  := computing_types(division_hom, get(type_hom))]
  unique_dollar.neutral.portfolio[, paste(type,type_returns,"fem", sep = ''):= computing_types(mean_division_fem, get(type_returns))]
  unique_dollar.neutral.portfolio[,  paste(type,type_returns,"hom", sep = '')  := computing_types(mean_division_hom, get(type_returns))]
  
}


### Building the dollar neutral portfolio

portf_ret <- "portf_ret"

creating_column_names <- function(name){
  
  column_name = paste(portf_ret, name, sep = '_')
  
  return(column_name)
}

f <-  list('demeaned_division_hom', 'demeaned_division_fem', 'demeaned_division_hom_by_clients','demeaned_division_fem_by_clients')

for (col in f){
  
  name = col
  
  unique_dollar.neutral.portfolio[, creating_column_names(name) := sum(get(col)* fut_ret_measure_25 ), by = .(months_years)]
  
}

portfolio_returns <- function(positions, returns)
{
  sum(positions * returns)
}

good_portfolio_returns <- function(indicator1, indicator2, returns){
  position = (indicator1 - indicator2)
  return(portfolio_returns(position, returns))
}

great_portfolio_returns <- function (indicator, timing_factor, returns){
  positions = indicator * timing_factor
  return(portfolio_returns(positions, returns))
}


all_stuff_high <- list('high_indicator_division_hom', 'high_indicator_division_fem', 'high_indicator_division_hom_by_clients', 'high_indicator_division_fem_by_clients')
all_stuff_low <- list('low_indicator_division_hom', 'low_indicator_division_fem', 'low_indicator_division_hom_by_clients', 'low_indicator_division_fem_by_clients')
indicators <- list('indicator_return_pos', 'indicator_return_neg')

length_list <- length(all_stuff_high)

great = "great"

future_returns_to_be_traded = "fut_ret_measure_25"

portf_ret = "portf_ret"

names_for_portfolio_columns <- list()

for (i in 1:length_list) {
  
  # produce name_ for this category
  # get the analogous low_indicator 
  
  high_name = all_stuff_high[[i]]
  low_name = all_stuff_low[[i]]
  name = d[[i]]
  
  unique_dollar.neutral.portfolio[, paste(creating_column_names(i),name, sep ='_') := good_portfolio_returns(get(high_name), get(low_name), get(future_returns_to_be_traded)), by =. (months_years)]
  
  for (timing_factor in indicators){
    
    unique_dollar.neutral.portfolio[, paste(portf_ret,name, timing_factor, sep = '_') := great_portfolio_returns(get(high_name), get(timing_factor), get(future_returns_to_be_traded)), by =. (months_years)]
    
    unique_dollar.neutral.portfolio[, paste(portf_ret,name, timing_factor, sep = '_') := great_portfolio_returns(get(low_name), get(timing_factor), get(future_returns_to_be_traded)), by =. (months_years)]
    
  }
}  

unique_dollar.neutral.portfolio[,years:=substr(months_years,1,4)]

###Function for computing excess returns

computing_excess_returns <- function(portfolio_returns, risk_free_rate)
  
{
  
  excess_returns <- Return.excess(portfolio_returns, Rf = risk_free_rate)
  
}

new_list <- list(colnames(unique_dollar.neutral.portfolio))

unique_dollar.neutral.portfolio[,years:=substr(months_years,1,4)]

# @param
# @portf_ret_demeaned_division_fem portfolio returns using demeaned female "attractivness score"
# @portf_ret_demeaned_division_hom portfolio returns using demeaned male "attractivness score"
# @portf_ret_demeaned_division_fem_by_clients portfolio returns using demeaned female "attractivness score" compared to the total number of clients
# @portf_ret_demeaned_division_hom portfolio_by_clients returns using male demeaned "attractivness score" compared to the total number of clients
# @portf_ret_2_division_fem portfolio returns using female "attractivness score"
# @portf_ret_1_division_hom portfolio returns using male "attractivness score"
# @portf_ret_4_division_fem_by_clients portfolio returns using female "attractivness score" compared to the total number of clients
# @portf_ret_3_division_hom portfolio_by_clients returns using male "attractivness score" compared to the total number of clients
# @portf_ret_division_fem_indicator_return_neg portfolio returns using female "attractivness score" multiplied a timing factor( here past returns in the lowest quantile)
# @portf_ret_division_hom_indicator_return_neg portfolio returns using male "attractivness score" multiplied a timing factor( here past returns in the lowest quantile)
# @portf_ret_division_fem_by_clients_indicator_return_neg portfolio returns using female "attractivness score" multiplied a timing factor( here past returns in the lowest quantile)
# @portf_ret_division_hom_by_clients_indicator_return_neg portfolio returns using male "attractivness score" multiplied a timing factor( here past returns in the lowest quantile)
# @portf_ret_division_fem_indicator_return_neg portfolio returns using female "attractivness score" multiplied a timing factor( here past returns in the highest quantile)
# @portf_ret_division_hom_indicator_return_neg portfolio returns using male "attractivness score" multiplied a timing factor( here past returns in the highest quantile)
# @portf_ret_division_fem_by_clients_indicator_return_neg portfolio returns using female "attractivness score" multiplied a timing factor( here past returns in the highest quantile)
# @portf_ret_division_hom_by_clients_indicator_return_neg portfolio returns using male "attractivness score" multiplied a timing factor( here past returns in the highest quantile)


l <-
  list(
    "portf_ret_demeaned_division_fem",
    "portf_ret_demeaned_division_hom",
    "portf_ret_demeaned_division_fem_by_clients",
    "portf_ret_demeaned_division_hom_by_clients",
    "portf_ret_2_division_fem",
    "portf_ret_1_division_hom",
    "portf_ret_4_division_fem_by_clients",
    "portf_ret_3_division_hom_by_clients",
    'portf_ret_division_fem_by_clients_indicator_return_neg',
    "portf_ret_division_hom_by_clients_indicator_return_neg",
    "portf_ret_division_fem_indicator_return_neg",
    "portf_ret_division_hom_indicator_return_neg",
    "portf_ret_division_fem_by_clients_indicator_return_pos",
    "portf_ret_division_hom_by_clients_indicator_return_pos",
    "portf_ret_division_fem_indicator_return_pos",
    "portf_ret_division_hom_indicator_return_pos"
  )


return_excess = "return_excess"
std_dev = "std_dev"
mannual_annualized_sharpe_ratio = "mannual_annualized_sharpe_ratio"

computing_sharpe_ratios <- function(column) {
  
  result = sqrt(12) * mean(column)/sd(column)
  
}

# Computes annualized means, standart deviations and sharpe ratios using the previsous @param

for (col in l) {
  
  name = col
  
  unique_dollar.neutral.portfolio[, paste(return_excess,name, sep = '_') :=  computing_excess_returns(get(col), 0.0003)]
  unique_dollar.neutral.portfolio[, paste(mean, name, sep = '_') :=  mean(get(col)), by =.(years)]
  unique_dollar.neutral.portfolio[, paste(std_dev, name, sep = '_') := sd(get(col)), by =. (years)]
  unique_dollar.neutral.portfolio[, paste(mannual_annualized_sharpe_ratio, name, sep = '_') := computing_sharpe_ratios(get(col)), by =.(years)]
  
}

# Better visualization of sharpe ratios

visualized_sharpes <- unique_dollar.neutral.portfolio[, . (years, mannual_annualized_sharpe_ratio_portf_ret_demeaned_division_fem,mannual_annualized_sharpe_ratio_portf_ret_demeaned_division_hom, mannual_annualized_sharpe_ratio_portf_ret_demeaned_division_fem_by_clients, 
                                                           mannual_annualized_sharpe_ratio_portf_ret_demeaned_division_hom_by_clients,mannual_annualized_sharpe_ratio_portf_ret_2_division_fem, mannual_annualized_sharpe_ratio_portf_ret_1_division_hom,
                                                           mannual_annualized_sharpe_ratio_portf_ret_4_division_fem_by_clients, mannual_annualized_sharpe_ratio_portf_ret_3_division_hom_by_clients,mannual_annualized_sharpe_ratio_portf_ret_division_fem_by_clients_indicator_return_neg,
                                                           mannual_annualized_sharpe_ratio_portf_ret_division_hom_by_clients_indicator_return_neg, mannual_annualized_sharpe_ratio_portf_ret_division_fem_indicator_return_neg, mannual_annualized_sharpe_ratio_portf_ret_division_hom_indicator_return_neg,
                                                           mannual_annualized_sharpe_ratio_portf_ret_division_fem_by_clients_indicator_return_pos,mannual_annualized_sharpe_ratio_portf_ret_division_hom_by_clients_indicator_return_pos,
                                                           mannual_annualized_sharpe_ratio_portf_ret_division_fem_indicator_return_pos,mannual_annualized_sharpe_ratio_portf_ret_division_hom_indicator_return_pos)]%>%unique



# Import Fama French 3 factor data 

file2 <- "/home/qam/F-F_Research_Data_Factors.CSV"

fama_french_data1 <- fread(file2)

setnames(fama_french_data1, "V1", "months_years")

fama_french_data1[, years := substr(months_years, 1, 4)]

fama_french_data1[, months := substr(months_years, 5, 6)]

fama_french <-
  fama_french_data1[fama_french_data1$months_years >= "201001",]

fama_french[, months_years := paste(years, months, sep = '-')]

for (i in 2:5) {
  fama_french[[i]] = fama_french[[i]] / 100
  
}

# Keeping only relevant infornmation from the unique_dollar.neutral.portfolio for further analysis

portfolio <-
  unique(unique_dollar.neutral.portfolio[, .(
    portf_ret_demeaned_division_fem,
    portf_ret_demeaned_division_hom,
    portf_ret_demeaned_division_fem_by_clients,
    portf_ret_demeaned_division_hom_by_clients,
    portf_ret_2_division_fem,
    portf_ret_1_division_hom,
    portf_ret_4_division_fem_by_clients,
    portf_ret_3_division_hom_by_clients,
    portf_ret_division_fem_by_clients_indicator_return_neg,
    portf_ret_division_hom_by_clients_indicator_return_neg,
    portf_ret_division_fem_indicator_return_neg,
    portf_ret_division_hom_indicator_return_neg,
    portf_ret_division_fem_by_clients_indicator_return_pos,
    portf_ret_division_hom_by_clients_indicator_return_pos,
    portf_ret_division_fem_indicator_return_pos,
    portf_ret_division_hom_indicator_return_pos,
    months_years
  )])

# Merging data

fama_french_dollar_neutral_portfolio <-
  merge(portfolio,
        fama_french,
        by = c("months_years"),
        all.x = T)

# Creates names for regressions

regression = "regression"
names <- colnames(fama_french_dollar_neutral_portfolio)

naming_regressions <- function(name) {
  paste(regression, name, sep = "_")
}

# Converting returns from francs to dollars
for (i in 2:17) {
  fama_french_dollar_neutral_portfolio[[i]] = fama_french_dollar_neutral_portfolio[[i]] *
    1.12
  #Substracting the risk free Rate
  fama_french_dollar_neutral_portfolio[[i]] = (
    fama_french_dollar_neutral_portfolio[[i]] - fama_french_dollar_neutral_portfolio$RF
  )
  #Regressing to find alpha 
  alphas <-
    regressing_to_compute_alpha(fama_french_dollar_neutral_portfolio[[i]],
                                fama_french_dollar_neutral_portfolio)
  print(summary(alphas))
  
}

# portf_ret_demeaned_division_fem, portf_ret_demeaned_division_hom ,portf_ret_demeaned_division_fem_by_clients ,portf_ret_demeaned_division_hom_by_clients ,portf_ret_2_division_fem ,portf_ret_1_division_hom, portf_ret_4_division_fem_by_clients
# portf_ret_3_division_hom_by_clients portf_ret_division_fem_by_clients_indicator_return_neg portf_ret_division_hom_by_clients_indicator_return_neg portf_ret_division_fem_indicator_return_neg portf_ret_division_hom_indicator_return_neg
# portf_ret_division_fem_by_clients_indicator_return_pos portf_ret_division_hom_by_clients_indicator_return_pos portf_ret_division_fem_indicator_return_pos portf_ret_division_hom_indicator_return_pos

# Regression that computes alpha according to Fama French method

regressing_to_compute_alpha <- function(returns, dataa) {
  alpha <-
    lm(
      returns ~ fama_french_dollar_neutral_portfolio$SMB + fama_french_dollar_neutral_portfolio$HML + fama_french_dollar_neutral_portfolio$Mkt -
        RF,
      data = dataa
    )
  
}
