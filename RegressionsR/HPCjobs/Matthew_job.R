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

temp0 = xx[is.finite(rreturn365),
                    list(
                      client= client,
                      account_date = account_date,
                      ret =  rreturn365,
                      age = age)][, ':='(
                        retpr = ecdf(ret)(ret) # rank of return
                        ), by = account_date]
temp0 %>% head
temp0[, .N, by = client] %>% head

gw <- pvcm(retpr ~ age, data = temp0, index = c("client", "account_date"))
cor.test(gw[['coefficients']][,1], gw[['coefficients']][,2])
