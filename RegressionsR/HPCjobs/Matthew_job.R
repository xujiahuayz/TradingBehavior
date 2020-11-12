library(data.table)
library(plm)
library(magrittr)
library(stats)

getwd()

funcmeanconf = function(series){
  list(mret = mean(series, na.rm = T),
       conf1 = t.test(series)$conf.int[1],
       conf2 = t.test(series)$conf.int[2])
}

filerootpath = '/home/jxu/TradingBehavior/'
# filerootpath = '../'
filepath = paste0(filerootpath, 'data/')

load(paste0(filepath,'regtable.rda'))
load(paste0(filepath,'tsregressreg.rda'))
xx[, ':='(firstobs.y = NULL)]

trselscum = list()

temp0 = xx[is.finite(rreturn365),
           list(
             client= client,
             account_date = account_date,
             ret =  rreturn365,
             age = age)][, ':='(
               # retpr = ecdf(ret)(ret) # rank of return
               retpr = scale(ret) %>% as.numeric()
             ), by = account_date]


# nbin = 4
# pickage = seq(0, length.out = nbin, by = 1.5) * 365.2425 # pick an age
# pickage = c(1, 1.5, 2, 3.5, 5) * 365.2425
pickage = c(1, 2, 3, 4) * 365.2425

trselsplm = list()
ns = c()
print(0 %>% paste0('====================================================='))


for (k in 1:(length(pickage)-1)){
  popu = regtable[lifetime >= pickage[k+1] & firstobs >= '2009-01-01', c('lifetime', 'client')]
  
  # popu[, ':='(cohort = floor(as.numeric(lifetime/(182.62125/6))))] # cohorting in lifetime in half year
  
  # select only observations where clients are between that age bin
  # temp = merge(
  #   temp0[age > pickage[k] & age <= pickage[k+1]], popu[, c('cohort', 'client')], by = 'client'
  #   )[ , list(
  #     retpr = mean(retpr, na.rm = T),
  #     account_date = mean(account_date, na.rm = T),
  #     n = length(retpr)
  #     #have to do a synthetic group, otherwise `pvcm` does not run
  #     ), by = c('cohort', 'age')][n > 200]
  
  temp = merge(
    temp0[age > pickage[k] & age <= pickage[k+1]], popu[, c('lifetime', 'client')], by = 'client'
  )
  temp[, agediff := as.numeric(age - pickage[k])]
  temp %>% head %>% print
  
  gw <- lm(retpr ~ lifetime + agediff + I(lifetime * agediff) , data = temp
           # , index = c("cohort", "account_date")
           )
  # gw[['coefficients']] %>% print
  gw %>% summary %>% print
  # 
  # cor.test(gw[['coefficients']][,1], gw[['coefficients']][,2]) %>% print
  # cor.test(gw[['coefficients']][,1], gw[['coefficients']] %>% row.names() %>% as.numeric()) %>% print
  # cor.test(gw[['coefficients']] %>% row.names() %>% as.numeric(), gw[['coefficients']][,2]) %>% print
  # 
  print(k %>% paste0('====================================================='))
}





