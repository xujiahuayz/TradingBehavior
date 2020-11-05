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
               retpr = ecdf(ret)(ret) # rank of return
             ), by = account_date]


nbin = 4
pickage = seq(0, length.out = nbin, by = 1.5) * 364.2425 # pick an age

trselsplm = list()
ns = c()
for (k in 1:nbin){
  popu = regtable[lifetime >= pickage[k+1] & firstobs >= '2009-01-01']$client
  
  # select only observations where clients are between tha age bin
  temp = temp0[age > pickage[k] & age <= pickage[k+1] & client %in% popu]
  
  gw <- pvcm(retpr ~ age, data = temp, index = c("client", "account_date"))
  cor.test(gw[['coefficients']][,1], gw[['coefficients']][,2]) %>% print
  
  print(k)
}





