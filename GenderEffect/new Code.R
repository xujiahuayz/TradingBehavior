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

# Constants ----

## indicators
d <-
  list(
    'division_hom',
    'division_fem',
    'division_hom_by_clients',
    'division_fem_by_clients'
  )


g <-
  list(
    'mean_division_hom',
    'mean_division_fem',
    'mean_division_hom_by_clients',
    'mean_division_fem_by_clients'
  )

# Import data ----
rm(list = ls())
load("/home/qam/Sofiya/Data/computed_returns_2010_2020.rda")
load("/home/qam/utrans2_2001_2019_reduced.rda")
file2 <- "/home/qam/citrix/gender_label.csv"
gender <- fread(file2)

setnames(utrans2, "date_trans", "close_date")
setnames(utrans2, "cur", "currency")

test1 <-
  unique(utrans2[utrans2$action == "B" &
                   utrans2$close_date >= "2010-01-01",])

test1.na <- na.omit(test1)
dt_merge <- merge(test1.na, gender, by = c("client"), all.x = T)
unique(dt_merge)
dt_mergeNew <- na.omit(dt_merge)
dt_mergeNew[, isin_code := substr(security_key, 1, 12)]
dt_mergeNew[, stock_key := paste(isin_code, currency, sep = '_')]

return_data <- na.omit(Return_data)

setnames(return_data, "security_key", "stock_key")

tot_data <-
  merge(
    dt_mergeNew,
    return_data,
    by = c("stock_key", "close_date"),
    all.y = TRUE
  )
total_data <- na.omit(tot_data)
data <- total_data[!ret_measure_250 %in% c(-Inf, Inf)]
dataTot <- data[!fut_ret_measure_250 %in% c(-Inf, Inf)]

dataTot2 <- dataTot[1:100000,]
save(dataTot2, file = "dataTot4.Rdata")


save(dataTot, file = "dataTot.Rdata")


# Creating a dollar neutral portfolio using buy data ----

### Creating a data set containing all the necessary data by combining data sets

D <- data.table(dataTot)

months <-
  setDT(D)[, months_years := format(as.Date(c(D$close_date)), "%Y-%m")]

totalData <- cbind(D, months)
# %>% as.data.table
# totalData <- as.data.table(totalData)
table1 <-
  unique(totalData[, . (
    vol_chf_trans,
    client,
    title_label,
    months_years,
    isin,
    ret_measure_1,
    ret_measure_5,
    fut_ret_measure_5,
    ret_measure_25,
    fut_ret_measure_25,
    ret_measure_70,
    ret_measure_250,
    fut_ret_measure_250,
    fut_ret_measure_1,
    fut_ret_measure_70
  )])

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

tmp = unique(table1[, .(client, title_label, months_years, isin, vol_chf_trans)])
tmp[, cnt := .N , by = .(months_years, title_label, isin
                         # , client
                         )]
tmp[, sum_vol_trans_by_isin := sum(vol_chf_trans), by = . (months_years, title_label, isin)]
tmp[, sum_vol_trans := sum(vol_chf_trans), by = . (months_years, title_label)]
tmp2 <-
  tmp[, . (title_label, months_years, sum_vol_trans)] %>% unique %>% dcast(., months_years ~ title_label, value.var = c('sum_vol_trans'))

tmp = tmp[, .(
  months_years,
  isin,
  title_label,
  cnt,
  client,
  vol_chf_trans,
  sum_vol_trans,
  sum_vol_trans_by_isin
)] %>% unique()
tmp4 = dcast(tmp, months_years  + isin ~ title_label, value.var = c('cnt'))
tmp5 = dcast(tmp, months_years ~ title_label, value.var = c('sum_vol_trans'))
setnames(tmp5, "Mr", "sum_vol_trans_mr")
setnames(tmp5, "Mrs", "sum_vol_trans_mrs")
setnames(tmp4, "Mr", "count_mr_transactions_per_isin_date")
setnames(tmp4, "Mrs", "count_mrs_transactions_per_isin_date")
tmp4[, sumClients := (count_mr_transactions_per_isin_date + count_mrs_transactions_per_isin_date) , by = .(months_years, isin)]
tmp5[, sum_vol_trans := (sum_vol_trans_mr + sum_vol_trans_mrs) , by = .(months_years)]
tmp6 <- merge(tmp4,
              tmp,
              by = c("months_years", "isin"),
              all.x = T)
tmp7 <- merge(tmp5, tmp6, by = c("months_years"), all.x = T)



###Combining old data and new data containing the new columns

table2 <-
  unique(table1[, .(
    client,
    title_label,
    months_years,
    ret_measure_1,
    ret_measure_5,
    fut_ret_measure_5,
    ret_measure_25,
    fut_ret_measure_25,
    ret_measure_70,
    ret_measure_250,
    fut_ret_measure_250,
    fut_ret_measure_1,
    fut_ret_measure_70
  )])
table2[, count := .N , by = .(months_years, title_label)]
table2 = table2[, .(
  months_years,
  title_label,
  count,
  ret_measure_1,
  ret_measure_5,
  fut_ret_measure_5,
  ret_measure_25,
  fut_ret_measure_25,
  ret_measure_70,
  ret_measure_250,
  fut_ret_measure_250,
  fut_ret_measure_1,
  fut_ret_measure_70
)] %>% unique()
newTable2 = dcast(table2, months_years ~ title_label, value.var = ('count'))
setnames(newTable2, "Mr", "count_mr_transactions_per_date")
setnames(newTable2, "Mrs", "count_mrs_transactions_per_date")

as.data.table(newTable2)

#data table with future and past returns
totalTable <-
  unique(merge(
    tmp7,
    newTable2,
    by = ("months_years"),
    all.x = T
  ))
datTotalData <- data.table(totalData)

returns_fut_past <-
  datTotalData[, . (
    ret_measure_1,
    ret_measure_5,
    fut_ret_measure_5,
    ret_measure_25,
    fut_ret_measure_25,
    ret_measure_70,
    ret_measure_250,
    fut_ret_measure_250,
    fut_ret_measure_1,
    fut_ret_measure_70,
    months_years,
    isin,
    client,
    vol_chf_trans
  )]
#setnames(returns_fut_past, "isin_code", "isin")
totalTable2 <-
  merge(
    totalTable,
    returns_fut_past,
    by = c("isin", "months_years", "client", "vol_chf_trans"),
    all.x = T
  )

###Creating new columns to compute future indicators

# @param division_fem = number of buys per isin per month divided by the number of buys per month, for female clients
# @param division_hom = number of buys per isin per month divided by the number of buys per month, for male clients
# @param division_fem_by_clients = number of buys per month for female clients divided by the number of buys per month for all clients
# @param division_hom_by_clients = number of buys per month for male clients divided by the number of buys per month for all clients


totalTable2[, division_fem := (count_mrs_transactions_per_isin_date /
                                 count_mrs_transactions_per_date), by = . (months_years)]
totalTable2[, division_hom := (count_mr_transactions_per_isin_date /
                                 count_mr_transactions_per_date), by = .(months_years)]
totalTable2[, division_fem_by_clients := (count_mrs_transactions_per_isin_date /
                                            sumClients), by = .(months_years, isin)]
totalTable2[, division_hom_by_clients := (count_mr_transactions_per_isin_date /
                                            sumClients), by = .(months_years, isin)]
finalTable <- na.omit(totalTable2)

dollar.neutral.portfolio <-
  drop_na(
    finalTable,
    c(
      ret_measure_1,
      ret_measure_5,
      fut_ret_measure_5,
      ret_measure_25,
      fut_ret_measure_25,
      ret_measure_70,
      ret_measure_250,
      fut_ret_measure_250,
      fut_ret_measure_1,
      fut_ret_measure_70
    )
  )


## Function used to compute regressions

regressing_on_indicators <-
  function(future_returns, indicator, dataa) {
    returns_on_indicator <- lm(future_returns ~ indicator, data = dataa)
    return(returns_on_indicator)
  }

##calling the function for different types of future returns

## Regressing using indicators computed for women

# @param division_fem = number of buys per isin per month divided by the number of buys per month, for female clients
# @param division_fem_by_clients = number of buys per month for female clients divided by the number of buys per month for all clients

returns_on_indicators_25_fem <-
  regressing_on_indicators(
    dollar.neutral.portfolio$fut_ret_measure_25,
    dollar.neutral.portfolio$division_fem,
    dollar.neutral.portfolio
  )
returns_on_indicators_70_fem <-
  regressing_on_indicators(
    dollar.neutral.portfolio$fut_ret_measure_70,
    dollar.neutral.portfolio$division_fem,
    dollar.neutral.portfolio
  )
returns_on_indicators_5_fem <-
  regressing_on_indicators(
    dollar.neutral.portfolio$fut_ret_measure_5,
    dollar.neutral.portfolio$division_fem,
    dollar.neutral.portfolio
  )
returns_on_indicators_1_fem <-
  regressing_on_indicators(
    dollar.neutral.portfolio$fut_ret_measure_1,
    dollar.neutral.portfolio$division_fem,
    dollar.neutral.portfolio
  )

new_returns_on_indicators_25_fem <-
  regressing_on_indicators(
    dollar.neutral.portfolio$fut_ret_measure_25,
    dollar.neutral.portfolio$division_fem_by_clients,
    dollar.neutral.portfolio
  )
new_returns_on_indicators_70_fem <-
  regressing_on_indicators(
    dollar.neutral.portfolio$fut_ret_measure_70,
    dollar.neutral.portfolio$division_fem_by_clients,
    dollar.neutral.portfolio
  )
new_returns_on_indicators_5_fem <-
  regressing_on_indicators(
    dollar.neutral.portfolio$fut_ret_measure_5,
    dollar.neutral.portfolio$division_fem_by_clients,
    dollar.neutral.portfolio
  )
new_returns_on_indicators_1_fem <-
  regressing_on_indicators(
    dollar.neutral.portfolio$fut_ret_measure_1,
    dollar.neutral.portfolio$division_fem_by_clients,
    dollar.neutral.portfolio
  )

### Regressing using indicators computed for men

# @param division_hom= number of buys per isin per month divided by the number of buys per month, for male clients
# @param division_hom_by_clients = number of buys per month for male clients divided by the number of buys per month for all clients

returns_on_indicators_25_hom <-
  regressing_on_indicators(
    dollar.neutral.portfolio$fut_ret_measure_25,
    dollar.neutral.portfolio$division_hom,
    dollar.neutral.portfolio
  )
returns_on_indicators_70_hom <-
  regressing_on_indicators(
    dollar.neutral.portfolio$fut_ret_measure_70,
    dollar.neutral.portfolio$division_hom,
    dollar.neutral.portfolio
  )
returns_on_indicators_5_hom <-
  regressing_on_indicators(
    dollar.neutral.portfolio$fut_ret_measure_5,
    dollar.neutral.portfolio$division_hom,
    dollar.neutral.portfolio
  )
returns_on_indicators_1_hom <-
  regressing_on_indicators(
    dollar.neutral.portfolio$fut_ret_measure_1,
    dollar.neutral.portfolio$division_hom,
    dollar.neutral.portfolio
  )

new_returns_on_indicators_25_hom <-
  regressing_on_indicators(
    dollar.neutral.portfolio$fut_ret_measure_25,
    dollar.neutral.portfolio$division_hom_by_clients,
    dollar.neutral.portfolio
  )
new_returns_on_indicators_70_hom <-
  regressing_on_indicators(
    dollar.neutral.portfolio$fut_ret_measure_70,
    dollar.neutral.portfolio$division_hom_by_clients,
    dollar.neutral.portfolio
  )
new_returns_on_indicators_5_hom <-
  regressing_on_indicators(
    dollar.neutral.portfolio$fut_ret_measure_5,
    dollar.neutral.portfolio$division_hom_by_clients,
    dollar.neutral.portfolio
  )
new_returns_on_indicators_1_hom <-
  regressing_on_indicators(
    dollar.neutral.portfolio$fut_ret_measure_1,
    dollar.neutral.portfolio$division_hom_by_clients,
    dollar.neutral.portfolio
  )

####Creating a dollar neutral portfolio containing only unique combinations of dates and isins

merged_colmns_isin_months_years <-
  cbind(dollar.neutral.portfolio$isin,
        dollar.neutral.portfolio$months_years)
merged_table <- as.data.table(merged_colmns_isin_months_years)

setnames(merged_table, "V2", "months_years")
merged_table$Seq_modif <-
  paste(merged_table$V1, merged_table$months_years)
setnames(merged_table, "V1", "isin")
setnames(merged_table, "Seq_modif", "combined_dates_months")

merged_table1 <- unique(merged_table)


dt_merge1 <-
  merge(
    dollar.neutral.portfolio,
    merged_table1 ,
    by = c("isin", "months_years"),
    all.x = T
  )
unique_dollar.neutral.portfolio <-
  dt_merge1[match(unique(dt_merge1$combined_dates_months),
                  dt_merge1$combined_dates_months),]

#compute quantiles by months

### names = keys
c_of_quantiles = c(0.2, 0.8)
quant <- "quant"


for (col in d) {
  for (quantile1 in c_of_quantiles) {
    name = col
    
    unique_dollar.neutral.portfolio[,  paste(quant, name, quantile1, sep =
                                               '_')  := quantile(get(col), quantile1), by = .(months_years)]
    
  }
}

### Create indicators based on the computed quantiles
# indicators represent values bigger ans smaller than the previosly computed quantiles

high_indicators <-
  list(
    'quant_division_hom_0.8',
    'quant_division_fem_0.8',
    'quant_division_hom_by_clients_0.8',
    'quant_division_fem_by_clients_0.8'
  )
low_indicators <-
  list(
    'quant_division_hom_0.2',
    'quant_division_fem_0.2',
    'quant_division_hom_by_clients_0.2',
    'quant_division_fem_by_clients_0.2'
  )

high_ind = "high_indicator"
low_ind = "low_indicator"
list_length = length(high_indicators)

computing_high_indicators <- function(division, quantile) {
  ifelse(division > quantile , 1, 0)
}

computing_low_indicators <- function(division, quantile) {
  ifelse(division < quantile , 1, 0)
}

for (i in 1:list_length) {
  high_indicator = high_indicators[[i]]
  low_indicator = low_indicators[[i]]
  col = d[[i]]
  name = col
  
  unique_dollar.neutral.portfolio[,  paste(high_ind, name, sep = '_')  := computing_high_indicators(get(col), get(high_indicator))]
  unique_dollar.neutral.portfolio[, paste(low_ind, name, sep = '_') := computing_low_indicators(get(col), get(low_indicator))]
  
}

unique_dollar.neutral.portfolio[, indicator_return_pos := ifelse(ret_measure_25 >
                                                                   0, 1, 0)]
unique_dollar.neutral.portfolio[, indicator_return_neg := ifelse(ret_measure_25 <
                                                                   0, 1, 0)]

### Find misatke in demeaned fractions by clients

# Demeaning the previous @param ----

## @param
# @ mean_mr/mean_mrs = mean division_hom/mean division_fem
# @ mean_clients_mr/mean_clients_mrs = mean division_hom_by_clients/mean division_fem_by_clients

mean = "mean"
demeaned = "demeaned"

for (col in d) {
  name = col
  unique_dollar.neutral.portfolio[,  paste(mean, name, sep = '_')  := mean(get(col)), by =
                                    .(months_years)]
}

g <-
  list(
    'mean_division_hom',
    'mean_division_fem',
    'mean_division_hom_by_clients',
    'mean_division_fem_by_clients'
  )

for (i in 1:length(g)) {
  col = d[[i]]
  col2 = g[[i]]
  name = col
  
  unique_dollar.neutral.portfolio[,  paste(demeaned, name, sep = '_')  := (get(col) - mean(get(col2))), by =
                                    .(months_years)]
  
}
### We build this to be able to see the interaction effect between gender and return signals

computing_types <- function(division, indicator) {
  type = division * unique_dollar.neutral.portfolio$ret_measure_25 * indicator
  return(type)
}

type_list_fem <-
  list('high_indicator_division_fem', 'low_indicator_division_fem')
type_list_hom <-
  list('high_indicator_division_hom', 'low_indicator_division_hom')
type_returns_list <-
  list('indicator_return_pos', 'indicator_return_neg')
type = "type"

for (i in 1:length(type_list_fem)) {
  type_fem = type_list_fem[[i]]
  type_hom = type_list_hom[[i]]
  type_returns = type_returns_list[[i]]
  
  unique_dollar.neutral.portfolio[,  paste(type, type_fem , sep = "_")  := computing_types(division_fem, get(type_fem))]
  unique_dollar.neutral.portfolio[,  paste(type, type_hom, sep = "_")  := computing_types(division_hom, get(type_hom))]
  unique_dollar.neutral.portfolio[, paste(type, type_returns, "fem", sep = '') := computing_types(mean_division_fem, get(type_returns))]
  unique_dollar.neutral.portfolio[,  paste(type, type_returns, "hom", sep = '')  := computing_types(mean_division_hom, get(type_returns))]
  
}

### Building the dollar neutral portfolio

portf_ret <- "portf_ret"

future_returns_to_be_traded = "fut_ret_measure_25"

creating_column_names <- function(name) {
  column_name = paste(portf_ret, name, sep = '_')
  
  return(column_name)
}

f <-
  list(
    'demeaned_division_hom',
    'demeaned_division_fem',
    'demeaned_division_hom_by_clients',
    'demeaned_division_fem_by_clients'
  )

for (col in f) {
  name = col
  
  unique_dollar.neutral.portfolio[, creating_column_names(name) := sum(get(col) * get(future_returns_to_be_traded)), by = .(months_years)]
  
}

portfolio_returns <- function(positions, returns)
{
  sum(positions * returns)
}

good_portfolio_returns <-
  function(indicator1, indicator2, returns) {
    position <- (indicator1 - indicator2)
    return(portfolio_returns(position, returns))
  }

great_portfolio_returns <-
  function (indicator, timing_factor, returns) {
    positions <- (indicator * timing_factor)
    return(portfolio_returns(positions, returns))
  }


all_stuff_high <-
  list(
    'high_indicator_division_hom',
    'high_indicator_division_fem',
    'high_indicator_division_hom_by_clients',
    'high_indicator_division_fem_by_clients'
  )
all_stuff_low <-
  list(
    'low_indicator_division_hom',
    'low_indicator_division_fem',
    'low_indicator_division_hom_by_clients',
    'low_indicator_division_fem_by_clients'
  )
indicators <-
  list('indicator_return_pos', 'indicator_return_neg')

length_list <- length(all_stuff_high)

great = "great"


portf_ret = "portf_ret"

names_for_portfolio_columns <- list()

for (i in 1:length_list) {
  # produce name_ for this caregory
  # get the analogous low_indicator
  high_name = all_stuff_high[[i]]
  low_name = all_stuff_low[[i]]
  name = d[[i]]
  
  unique_dollar.neutral.portfolio[, paste(creating_column_names(i), name, sep =
                                            '_') := good_portfolio_returns(get(high_name),
                                                                           get(low_name),
                                                                           get(future_returns_to_be_traded)), by = . (months_years)]
  
  for (timing_factor in indicators) {
    unique_dollar.neutral.portfolio[, paste(portf_ret, name, timing_factor, sep = '_') := great_portfolio_returns(get(high_name),
                                                                                                                  get(timing_factor),
                                                                                                                  get(future_returns_to_be_traded)), by = . (months_years)]
    
    unique_dollar.neutral.portfolio[, paste(portf_ret, name, timing_factor, sep = '_') := great_portfolio_returns(get(low_name),
                                                                                                                  get(timing_factor),
                                                                                                                  get(future_returns_to_be_traded)), by = . (months_years)]
    
  }
}

unique_dollar.neutral.portfolio[, years := substr(months_years, 1, 4)]

###Function for computing excess returns

computing_excess_returns <-
  function(portfolio_returns, risk_free_rate)
    
  {
    excess_returns <-
      Return.excess(portfolio_returns, Rf = risk_free_rate)
    
  }


unique_dollar.neutral.portfolio[, years := substr(months_years, 1, 4)]

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
    'portf_ret_division_hom_by_clients_indicator_return_neg',
    "portf_ret_division_fem_by_clients_indicator_return_neg",
    "portf_ret_division_fem_indicator_return_neg",
    "portf_ret_division_hom_indicator_return_neg",
    "portf_ret_division_fem_by_clients_indicator_return_pos",
    "portf_ret_division_hom_by_clients_indicator_return_pos",
    "portf_ret_division_hom_indicator_return_pos",
    "portf_ret_division_fem_indicator_return_pos"
  )

return_excess = "return_excess"
std_dev = "std_dev"
mannual_annualized_sharpe_ratio = "mannual_annualized_sharpe_ratio"

computing_sharpe_ratios <- function(column) {
  result = sqrt(12) * mean(column) / sd(column)
  
}

for (col in l) {
  name = col
  
  unique_dollar.neutral.portfolio[, paste(return_excess, name, sep = '_') :=  computing_excess_returns(get(col), 0.0003)]
  unique_dollar.neutral.portfolio[, paste(mean, name, sep = '_') :=  mean(get(col)), by =
                                    .(years)]
  unique_dollar.neutral.portfolio[, paste(std_dev, name, sep = '_') := sd(get(col)), by =
                                    . (years)]
  unique_dollar.neutral.portfolio[, paste(mannual_annualized_sharpe_ratio, name, sep = '_') := computing_sharpe_ratios(get(col)), by =
                                    .(years)]
  
}


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
    portf_ret_division_hom_by_clients_indicator_return_neg,
    portf_ret_division_fem_by_clients_indicator_return_neg,
    portf_ret_division_fem_indicator_return_neg,
    portf_ret_division_hom_indicator_return_neg,
    portf_ret_division_fem_by_clients_indicator_return_pos,
    portf_ret_division_hom_by_clients_indicator_return_pos,
    portf_ret_division_hom_indicator_return_pos,
    portf_ret_division_fem_indicator_return_pos,
    months_years
  )])


fama_french_dollar_neutral_portfolio <-
  merge(portfolio,
        fama_french,
        by = c("months_years"),
        all.x = T)

### Converting returns from francs to dollars

regression = "regression"
names <- colnames(fama_french_dollar_neutral_portfolio)

naming_regressions <- function(name) {
  paste(regression, name, sep = "_")
}

for (i in 2:17) {
  fama_french_dollar_neutral_portfolio[[i]] = fama_french_dollar_neutral_portfolio[[i]] *
    1.12
  fama_french_dollar_neutral_portfolio[[i]] = (
    fama_french_dollar_neutral_portfolio[[i]] - fama_french_dollar_neutral_portfolio$RF
  )
  
  alphas <-
    regressing_to_compute_alpha(fama_french_dollar_neutral_portfolio[[i]],
                                fama_french_dollar_neutral_portfolio)
  print(summary(alphas))
  
}

regressing_to_compute_alpha <- function(returns, dataa) {
  alpha <-
    lm(
      returns ~ fama_french_dollar_neutral_portfolio$SMB + fama_french_dollar_neutral_portfolio$HML + fama_french_dollar_neutral_portfolio$Mkt -
        RF,
      data = dataa
    )
  
}


################################################
##                                            ##
##      creaing a dollar neutral portfolio    ##
##      using sell data                       ##
################################################


D2 <- data.table(dataTot2)

months <-
  setDT(D2)[, months_years := format(as.Date(c(D$close_date)), "%Y-%m")]

totalData2 <- (cbind(D2, months))
as.data.table(totalData2)
table1.0 <-
  unique(totalData2[, . (
    vol_chf_trans,
    client,
    title_label,
    months_years,
    isin,
    ret_measure_1,
    ret_measure_5,
    fut_ret_measure_5,
    ret_measure_25,
    fut_ret_measure_25,
    ret_measure_70,
    ret_measure_250,
    fut_ret_measure_250,
    fut_ret_measure_1,
    fut_ret_measure_70
  )])

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

tmp2 = unique(table1.0[, .(client, title_label, months_years, isin, vol_chf_trans)])
tmp2[, cnt := .N , by = .(months_years, title_label, isin, client)]

tmp2 = tmp2[, .(months_years, isin, title_label, cnt, client, vol_chf_trans)] %>%
  unique()
tmp5 = dcast(tmp, months_years  + isin ~ title_label, value.var = c('cnt'))
setnames(tmp5, "Mr", "count_Mr")
setnames(tmp5, "Mrs", "count_Mrs")
tmp5[, sumClients := (count_Mr + count_Mrs) , by = .(months_years, isin)]
tmp7 <- merge(tmp5,
              tmp2,
              by = c("months_years", "isin"),
              all.x = T)


###Combining old data and new data containing the new columns

table2.0 <-
  unique(table1.0[, .(
    client,
    title_label,
    months_years,
    ret_measure_1,
    ret_measure_5,
    fut_ret_measure_5,
    ret_measure_25,
    fut_ret_measure_25,
    ret_measure_70,
    ret_measure_250,
    fut_ret_measure_250,
    fut_ret_measure_1,
    fut_ret_measure_70
  )])
table2.0[, count := .N , by = .(months_years, title_label)]
table2.0 = table2.0[, .(
  months_years,
  title_label,
  count,
  ret_measure_1,
  ret_measure_5,
  fut_ret_measure_5,
  ret_measure_25,
  fut_ret_measure_25,
  ret_measure_70,
  ret_measure_250,
  fut_ret_measure_250,
  fut_ret_measure_1,
  fut_ret_measure_70
)] %>% unique()
newTable2.0 = dcast(table2, months_years ~ title_label, value.var = ('count'))

as.data.table(newTable2.0)

#data table with future and past returns
totalTable.0 <-
  unique(merge(
    tmp7,
    newTable2.0,
    by = ("months_years"),
    all.x = T
  ))
datTotalData2 <- data.table(totalData2)

returns_fut_past2 <-
  datTotalData2[, . (
    ret_measure_1,
    ret_measure_5,
    fut_ret_measure_5,
    ret_measure_25,
    fut_ret_measure_25,
    ret_measure_70,
    ret_measure_250,
    fut_ret_measure_250,
    fut_ret_measure_1,
    fut_ret_measure_70,
    months_years,
    isin,
    client,
    vol_chf_trans
  )]
#setnames(returns_fut_past, "isin_code", "isin")
totalTable2.0 <-
  merge(
    totalTable.0,
    returns_fut_past2,
    by = c("isin", "months_years", "client", "vol_chf_trans"),
    all.x = T
  )

###Creating new columns to compute future indicators

# @param division_fem = number of buys per isin per month divided by the number of buys per month, for female clients
# @param division_hom = number of buys per isin per month divided by the number of buys per month, for male clients
# @param division_fem_by_clients = number of buys per month for female clients divided by the number of buys per month for all clients
# @param division_hom_by_clients = number of buys per month for male clients divided by the number of buys per month for all clients


totalTable2.0[, division_fem := (count_Mrs / Mrs), by = . (months_years)]
totalTable2.0[, division_hom := (count_Mr / Mr), by = .(months_years)]
totalTable2.0[, division_fem_by_clients := (count_Mrs / sumClients), by = .(months_years, isin)]
totalTable2.0[, division_hom_by_clients := (count_Mr / sumClients), by = .(months_years, isin)]
finalTable2 <- na.omit(totalTable2.0)


### Demeaning the previous @param

finalTable2[, demeaned_fraction_Mr := (division_hom - mean(division_hom)), by =
              . (months_years)]
finalTable2[, demeaned_fraction_Mrs := (division_fem - mean(division_fem)), by =
              . (months_years)]
finalTable2[, demeaned_fraction_Mr_by_clients := (division_hom_by_clients - mean(division_hom_by_clients)), by =
              . (months_years)]
finalTable2[, demeaned_fraction_Mrs_by_clients := division_fem_by_clients - mean(division_fem_by_clients), by =
              . (months_years)]


#compute quantiles by months

##"test" <- function(genre,quant){
##  newColumn <- paste0("q",quant,"_",genre)
##  existingCol <- paste0("division_",genre)
##  return(c(newColumn,existingCol))
##}
##a <- test("hom",0.2)
##finalTable[, a[1] := quantile(a[2],0.2, na.rm =TRUE), by=.(months_years) ]

finalTable2[, q1_Mr := quantile(division_hom, 0.2, na.rm = TRUE), by =
              .(months_years)]
finalTable2[, q2_Mr := quantile(division_hom, 0.8, na.rm = TRUE), by =
              .(months_years)]
finalTable2[, q1_Mrs := quantile(division_fem, 0.2, na.rm = TRUE), by =
              .(months_years)]
finalTable2[, q2_Mrs := quantile(division_fem, 0.8, na.rm = TRUE), by =
              .(months_years)]

finalTable2[, q3_Mr := quantile(division_hom_by_clients, 0.2, na.rm =
                                  TRUE), by = .(months_years)]
finalTable2[, q4_Mr := quantile(division_hom_by_clients, 0.8, na.rm =
                                  TRUE), by = .(months_years)]
finalTable2[, q3_Mrs := quantile(division_fem_by_clients, 0.2, na.rm =
                                   TRUE), by = .(months_years)]
finalTable2[, q4_Mrs := quantile(division_fem_by_clients, 0.8, na.rm =
                                   TRUE), by = .(months_years)]

### Create indicators based on the computed quantiles
# indicators represent values bigger ans smaller than the previosly computed quantiles

finalTable2[, indicator1_hom := ifelse(division_hom < q1_Mr, 1, 0)]
finalTable2[, indicator2_hom := ifelse(division_hom > q2_Mr, 1, 0)]
finalTable2[, indicator1_fem := ifelse(division_fem < q1_Mrs, 1, 0)]
finalTable2[, indicator2_fem := ifelse(division_fem > q2_Mrs, 1, 0)]

finalTable2[, indicator3_hom := ifelse(division_hom_by_clients < q1_Mr, 1, 0)]
finalTable2[, indicator4_hom := ifelse(division_hom_by_clients > q2_Mr, 1, 0)]
finalTable2[, indicator3_fem := ifelse(division_fem_by_clients < q1_Mrs, 1, 0)]
finalTable2[, indicator4_fem := ifelse(division_fem_by_clients > q2_Mrs, 1, 0)]

dollar.neutral.portfolio2 <-
  drop_na(
    finalTable2,
    c(
      indicator1_hom,
      indicator1_fem,
      indicator2_hom,
      indicator2_fem,
      indicator3_hom,
      indicator3_fem,
      indicator4_hom,
      indicator4_fem,
      ret_measure_1,
      ret_measure_5,
      fut_ret_measure_5,
      ret_measure_25,
      fut_ret_measure_25,
      ret_measure_70,
      ret_measure_250,
      fut_ret_measure_250,
      fut_ret_measure_1,
      fut_ret_measure_70
    )
  )



## Function used to compute regressions

regressing_on_indicators <-
  function(future_returns, indicator, dataa) {
    returns_on_indicator <- lm(future_returns ~ indicator, data = dataa)
    return(returns_on_indicator)
  }

##calling the function for different types of future returns

## Regressing using indicators computed for women

# @param division_fem = number of buys per isin per month divided by the number of buys per month, for female clients
# @param division_fem_by_clients = number of buys per month for female clients divided by the number of buys per month for all clients


returns_on_indicators_25.2_fem <-
  regressing_on_indicators(
    dollar.neutral.portfolio2$fut_ret_measure_25,
    dollar.neutral.portfolio2$division_fem,
    dollar.neutral.portfolio2
  )
returns_on_indicators_70.2_fem <-
  regressing_on_indicators(
    dollar.neutral.portfolio2$fut_ret_measure_70,
    dollar.neutral.portfolio2$division_fem,
    dollar.neutral.portfolio2
  )
returns_on_indicators_5.2_fem <-
  regressing_on_indicators(
    dollar.neutral.portfolio2$fut_ret_measure_5,
    dollar.neutral.portfolio2$division_fem,
    dollar.neutral.portfolio2
  )
returns_on_indicators_1.2_fem <-
  regressing_on_indicators(
    dollar.neutral.portfolio2$fut_ret_measure_1,
    dollar.neutral.portfolio2$division_fem,
    dollar.neutral.portfolio2
  )


new_returns_on_indicators_25.2_fem <-
  regressing_on_indicators(
    dollar.neutral.portfolio2$fut_ret_measure_25,
    dollar.neutral.portfolio2$division_fem_by_clients,
    dollar.neutral.portfolio2
  )
new_returns_on_indicators_70.2_fem <-
  regressing_on_indicators(
    dollar.neutral.portfolio2$fut_ret_measure_70,
    dollar.neutral.portfolio2$division_fem_by_clients,
    dollar.neutral.portfolio2
  )
new_returns_on_indicators_5.2_fem <-
  regressing_on_indicators(
    dollar.neutral.portfolio2$fut_ret_measure_5,
    dollar.neutral.portfolio2$division_fem_by_clients,
    dollar.neutral.portfolio2
  )
new_returns_on_indicators_1.2_fem <-
  regressing_on_indicators(
    dollar.neutral.portfolio2$fut_ret_measure_1,
    dollar.neutral.portfolio2$division_fem_by_clients,
    dollar.neutral.portfolio2
  )

### Regressing using indicators computed for men

# @param division_fem = number of buys per isin per month divided by the number of buys per month, for male clients
# @param division_hom_by_clients = number of buys per month for male clients divided by the number of buys per month for all clients

returns_on_indicators_25_hom.2 <-
  regressing_on_indicators(
    dollar.neutral.portfolio2$fut_ret_measure_25,
    dollar.neutral.portfolio2$division_hom,
    dollar.neutral.portfolio2
  )
returns_on_indicators_70_hom.2 <-
  regressing_on_indicators(
    dollar.neutral.portfolio2$fut_ret_measure_70,
    dollar.neutral.portfolio2$division_hom,
    dollar.neutral.portfolio2
  )
returns_on_indicators_5_hom.2 <-
  regressing_on_indicators(
    dollar.neutral.portfolio2$fut_ret_measure_5,
    dollar.neutral.portfolio2$division_hom,
    dollar.neutral.portfolio2
  )
returns_on_indicators_1_hom.2 <-
  regressing_on_indicators(
    dollar.neutral.portfolio2$fut_ret_measure_1,
    dollar.neutral.portfolio2$division_hom,
    dollar.neutral.portfolio2
  )

new_returns_on_indicators_25_hom.2 <-
  regressing_on_indicators(
    dollar.neutral.portfolio2$fut_ret_measure_25,
    dollar.neutral.portfolio2$division_hom_by_clients,
    dollar.neutral.portfolio2
  )
new_returns_on_indicators_70_hom.2 <-
  regressing_on_indicators(
    dollar.neutral.portfolio2$fut_ret_measure_70,
    dollar.neutral.portfolio2$division_hom_by_clients,
    dollar.neutral.portfolio2
  )
new_returns_on_indicators_5_hom.2 <-
  regressing_on_indicators(
    dollar.neutral.portfolio2$fut_ret_measure_5,
    dollar.neutral.portfolio2$division_hom_by_clients,
    dollar.neutral.portfolio2
  )
new_returns_on_indicators_1_hom.2 <-
  regressing_on_indicators(
    dollar.neutral.portfolio2$fut_ret_measure_1,
    dollar.neutral.portfolio2$division_hom_by_cleints,
    dollar.neutral.portfolio2
  )

## computing positive and negative signals with past returns over a 25 day period (still need to eliminate all 0 values)

dollar.neutral.portfolio[, indicator1_return := ifelse(ret_measure_25 >
                                                         0, 1, 0)]
dollar.neutral.portfolio[, indicator2_return := ifelse(ret_measure_25 <
                                                         0, 1, 0)]


dollar.neutral.portfolio[, meanHom := mean(division_hom), by = . (months_years)]
dollar.neutral.portfolio[, meanfem := mean(division_fem), by = .(months_years)]

### We build this to be able to see the interaction effect between gender and return signals

dollar.neutral.portfolio[, type_pos_fem := (division_fem * ret_measure_25 *
                                              indicator1_return), by = .(isin)]
dollar.neutral.portfolio[, type_neg_fem := (division_fem * ret_measure_25 *
                                              indicator2_return), by = .(isin)]
dollar.neutral.portfolio[, type_pos_men := (division_hom * ret_measure_25 *
                                              indicator1_return), by = .(isin)]
dollar.neutral.portfolio[, type_neg_men := (division_hom * ret_measure_25 *
                                              indicator2_return), by = . (isin)]

dollar.neutral.portfolio[, type_pos_using_meanfem := (meanfem * ret_measure_25 *
                                                        indicator1_return), by = .(isin)]
dollar.neutral.portfolio[, type_neg_using_meanfem := (meanfem * ret_measure_25 *
                                                        indicator2_return), by = .(isin)]
dollar.neutral.portfolio[, type_pos_using_meanHom := (meanHom * ret_measure_25 *
                                                        indicator1_return), by = . (isin)]
dollar.neutral.portfolio[, type_neg_using_meanHom := (meanHom * ret_measure_25 *
                                                        indicator2_return), by = . (isin)]

### Building the dollar neutral portfolio

## find mistakes, results are clearly incorrect

dollar.neutral.portfolio[, portf_simple_1_hom := sum(fut_ret_measure_25 *
                                                       demeaned_fraction_Mr), by = .(months_years)]
dollar.neutral.portfolio[, portf_simple_1_fem := sum(demeaned_fraction_Mrs *
                                                       fut_ret_measure_25), by = .(months_years)]
dollar.neutral.portfolio[, portf_ret_1_hom := sum((
  division_hom * indicator2_hom * fut_ret_measure_25 - (division_hom * indicator1_hom *
                                                          fut_ret_measure_25)
)), by = .(months_years)]
dollar.neutral.portfolio[, portf_ret_1_fem := sum((
  division_fem * indicator2_fem * fut_ret_measure_25 - (division_fem * indicator1_fem *
                                                          fut_ret_measure_25)
)), by = .(months_years)]
dollar.neutral.portfolio[, portf_ret_2_hom := sum((
  indicator2_hom * division_hom * (ret_measure_25 > 0) - (
    division_hom * indicator1_hom * (ret_measure_25 < 0) * fut_ret_measure_25
  )
)), by = .(months_years)]
dollar.neutral.portfolio[, portf_ret_2_fem := sum((
  indicator2_fem * division_fem * (ret_measure_25 > 0) - (
    division_fem * indicator1_fem * (ret_measure_25 < 0) * fut_ret_measure_25
  )
)), by = .(months_years)]

dollar.neutral.portfolio[, portf_simple_2_hom := sum(fut_ret_measure_25 *
                                                       demeaned_fraction_Mr_by_clients), by = .(months_years)]
dollar.neutral.portfolio[, portf_simple_2_fem := sum(demeaned_fraction_Mrs_by_clients *
                                                       fut_ret_measure_25), by = .(months_years)]
dollar.neutral.portfolio[, portf_ret_3_hom := sum((
  division_hom_by_clients * indicator4_hom * fut_ret_measure_25 - (division_hom_by_clients *
                                                                     indicator3_hom * fut_ret_measure_25)
)), by = .(months_years)]
dollar.neutral.portfolio.first[, portf_ret_3_fem := sum((
  division_fem_by_clients * indicator4_fem * fut_ret_measure_25 - (division_fem_by_clients *
                                                                     indicator3_fem * fut_ret_measure_25)
)), by = .(months_years)]
dollar.neutral.portfolio.first[, portf_ret_4_hom := sum((
  indicator4_hom * division_hom_by_clients * (ret_measure_25 > 0) - (
    division_hom_by_clients * indicator3_hom * (ret_measure_25 < 0) * fut_ret_measure_25
  )
)), by = .(months_years)]
dollar.neutral.portfolio.first[, portf_ret_4_fem := sum((
  indicator4_fem * division_fem_by_clients * (ret_measure_25 > 0) - (
    division_fem_by_clients * indicator3_fem * (ret_measure_25 < 0) * fut_ret_measure_25
  )
)), by = .(months_years)]


# Recycle bin ----

## Do women like momentum

model_do_women_like_momentum <-
  glm(
    formula = I(title_label == "Mrs") ~ ret_measure_250 + ret_measure_70 + ret_measure_25,
    family = "binomial",
    data = dataTot
  )

model2 <-
  lm(ret_measure_25 ~ factor(title_label), data = dataTot)

model4 <- lm(ret_measure_70 ~ factor(title_label), data = dataTot)

model6 <- lm(ret_measure_250 ~ factor(title_label), data = dataTot)


## Regressing future returns using 70 day momentum

model1 <-
  lm(
    fut_ret_measure_1 ~ factor(title_label) + ret_measure_70 + factor(title_label) *
      ret_measure_70,
    data = dataTot
  )
model2 <-
  lm(
    fut_ret_measure_5 ~ factor(title_label) + ret_measure_70 + factor(title_label) *
      ret_measure_70,
    data = dataTot
  )
model3 <-
  lm(
    fut_ret_measure_25 ~ factor(title_label) + ret_measure_70 + factor(title_label) *
      ret_measure_70,
    data = dataTot
  )
model4 <-
  lm(
    fut_ret_measure_70 ~ factor(title_label) + ret_measure_70 + factor(title_label) *
      ret_measure_70,
    data = dataTot
  )
model5 <-
  lm(
    fut_ret_measure_250 ~ factor(title_label) + ret_measure_70 + factor(title_label) *
      ret_measure_70,
    data = dataTot
  )


##Regressing future returns using 250 day momentum

model6 <-
  lm(
    fut_ret_measure_1 ~ factor(title_label) + ret_measure_250 + factor(title_label) *
      ret_measure_250,
    data = dataTot
  )

model7 <-
  lm(
    fut_ret_measure_5 ~ factor(title_label) + ret_measure_250 + factor(title_label) * ret_measure_250,
    data = dataTot
  )

model8 <-
  lm(
    fut_ret_measure_25 ~ factor(title_label) + ret_measure_250 + factor(title_label) * ret_measure_250,
    data = dataTot
  )

model9 <-
  lm(
    fut_ret_measure_70 ~ factor(title_label) + ret_measure_250 + factor(title_label) * ret_measure_250,
    data = dataTot
  )

model10 <-
  lm(
    fut_ret_measure_250 ~ factor(title_label) + ret_measure_250 + factor(title_label) * ret_measure_250,
    data = dataTot
  )


##heatmap

##creating a vector with the coefficient estimates for 250 past momentum

coefficient1 <- coef(model6)
coefficient2 <- coef(model7)
coefficient3 <- coef(model8)
coefficient4 <- coef(model9)
coefficient5 <- coef(model10)

coeffs <-
  c(coefficient1[4],
    coefficient2[4],
    coefficient3[4],
    coefficient4[4],
    coefficient5[4])

###creating a vector with the coefficient estimates for 70 day momentum

coeff1 <- coef(model1)
coeff2 <- coef(model2)
coeff3 <- coef(model3)
coeff4 <- coef(model4)
coeff5 <- coef(model5)

coefficients <-
  c(coeff1[4], coeff2[4], coeff3[4], coeff4[4], coeff5[4])


##Regressing on sales

test2 <-
  unique(utrans2[utrans2$action == "S" &
                   utrans2$close_date >= "2010-01-01",])

test2.na <- na.omit(test2)
dt_merge2 <- merge(test2.na, gender, by = c("client"), all.x = T)
unique(dt_merge2)
dt_mergeNew2 <- na.omit(dt_merge2)
dt_mergeNew2[, isin_code := substr(security_key, 1, 12)]
dt_mergeNew2[, stock_key := paste(isin_code, currency, sep = '_')]


tot_data2 <-
  merge(
    dt_mergeNew2,
    return_data,
    by = c("stock_key", "close_date"),
    all.y = TRUE
  )
total_data2 <- na.omit(tot_data2)
data2 <- total_data2[!ret_measure_250 %in% c(-Inf, Inf)]
dataTot2 <- data2[!fut_ret_measure_250 %in% c(-Inf, Inf)]


## Do women sell winners

model1 <-
  lm(ret_measure_25 ~ factor(title_label), data = dataTot2)
model2 <-
  lm(ret_measure_70 ~ factor(title_label), data = dataTot2)
model3 <-
  lm(ret_measure_250 ~ factor(title_label), data = dataTot2)

model_do_women_sell_winners <-
  glm(
    formula = I(title_label == "Mrs") ~ ret_measure_250 + ret_measure_70 + ret_measure_25,
    family = "binomial",
    data = dataTot2
  )

model_do_women_like_momentum <-
  glm(
    formula = I(title_label == "Mrs") ~ ret_measure_250 + ret_measure_70 + ret_measure_25,
    family = "binomial",
    data = dataTot
  )



### Regressing on future returns only using past return signs

newTotalData <- copy(dataTot)


# changing values to 0's and one's


ret_250 = c(newTotalData$ret_measure_250)
ret_250[ret_250 > 0] <- 1
ret_250[ret_250 <= 0] <- -1


ret_250 <- ret_250

ret_70 = c(newTotalData$ret_measure_70)
ret_70[ret_70 > 0] <- 1
ret_70[ret_70 <= 0] <- -1

ret_70 <- ret_70

### adding these vectors to the data set

new.df <- data.frame(+ret_250, +ret_70)

dataSet <- (cbind(newTotalData, new.df))

##regressing using 250 day momentum on buys

model1 <-
  glm(
    fut_ret_measure_1 ~ factor(title_label) + ret_250 + factor(title_label) * ret_250,
    data = dataSet
  )
model2 <-
  glm(
    fut_ret_measure_5 ~ factor(title_label) + ret_250 + factor(title_label) * ret_250,
    data = dataSet
  )
model3 <-
  glm(
    fut_ret_measure_25 ~ factor(title_label) + ret_250 + factor(title_label) * ret_250,
    data = dataSet
  )
model4 <-
  glm(
    fut_ret_measure_70 ~ factor(title_label) + ret_250 + factor(title_label) *
      ret_250,
    data = dataSet
  )
model5 <-
  glm(
    fut_ret_measure_250 ~ factor(title_label) + ret_250 + factor(title_label) *
      ret_250,
    data = dataSet
  )

## regressing using 70 day momentum on sells

model6 <-
  glm(fut_ret_measure_1 ~ factor(title_label) + ret_70 + factor(title_label) *
        ret_70,
      data = dataSet)
model7 <-
  glm(fut_ret_measure_5 ~ factor(title_label) + ret_70 + factor(title_label) *
        ret_70,
      data = dataSet)
model8 <-
  glm(fut_ret_measure_25 ~ factor(title_label) + ret_70 + factor(title_label) *
        ret_70,
      data = dataSet)
model9 <-
  glm(fut_ret_measure_70 ~ factor(title_label) + ret_70 + factor(title_label) * ret_70,
      data = dataSet)
model10 <-
  glm(
    fut_ret_measure_250 ~ factor(title_label) + ret_70 + factor(title_label) *
      ret_70,
    data = dataSet
  )



                                     
