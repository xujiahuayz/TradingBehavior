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


filepath = '/home/jxu/TradingBehavior/data/'
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
               # retpr_rank = ecdf(ret)(ret), # rank of return
               retpr = scale(ret) %>% as.numeric()
             ), by = account_date]


# nbin = 4
# pickage = seq(0, length.out = nbin, by = 1.5) * 364.2425 # pick an age
pickage = c(1, 1.5, 2, 3.5, 5) * 364.2425

trselsplm = list()
ns = c()
for (k in 1:(length(pickage)-1)){
  popu = regtable[lifetime >= pickage[k+1] & firstobs >= '2009-01-01', c('lifetime', 'client')]
  
  popu[, ':='(cohort = floor(lifetime/7))] # cohorting in lifetime in weeks
  
  # select only observations where clients are between that age bin
  temp = merge(
    temp0[age > pickage[k] & age <= pickage[k+1]], popu[, c('cohort', 'client')], by = 'client'
    )[ , list(
      retpr = mean(retpr, na.rm = T),
      account_date = mean(account_date, na.rm = T) #have to do a synthetic group, otherwise pvcm does not run
      ), by = c('cohort', 'age')]
  
  temp %>% head %>% print
  
  
  gw <- pvcm(retpr ~ I(age - pickage[k]), data = temp, index = c("cohort", "account_date"))
  gw[['coefficients']] %>% head %>% print
  gw[['coefficients']] %>% summary %>% print
  
  cor.test(gw[['coefficients']][,1], gw[['coefficients']][,2]) %>% print
  
  print(k %>% paste0('====================================================='))
}





