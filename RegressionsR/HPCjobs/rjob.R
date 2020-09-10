library(data.table)
library(plm)
filepath = '/home/jxu/learnr/'
# load(paste0(filepath,'regtable.rda'))
# load(paste0(filepath,'cl_transferin.rda'))

load(paste0(filepath,'tsregressreg.rda'))

xx[, ':='(firstobs.y = NULL)]
gc()
time = Sys.time()
xxplmg = plm(rreturn365 ~ age + enterage,
data = xx, index = c('client', 'account_date'), effect = 'time')
Sys.time() - time
summary(xxplmg)

rm(xxplmg)
gc()
time = Sys.time()
xxplmg = plm(rreturn365 ~ age + gender + enterage,
data = xx, index = c('client', 'account_date'), effect = 'time')
Sys.time() - time
summary(xxplmg)


rm(xxplmg)
gc()
time = Sys.time()
xxplmg = plm(rreturn365 ~ age + TradesOptions,
data = xx, index = c('client', 'account_date'), effect = 'time')
Sys.time() - time
summary(xxplmg)

rm(xxplmg)
gc()
time = Sys.time()
xxplmg = plm(rreturn365 ~ age + TradesOptions + nsec + ntrade,
data = xx, index = c('client', 'account_date'), effect = 'time')
Sys.time() - time
summary(xxplmg)


rm(xxplmg)
gc()
time = Sys.time()
xxplmg = plm(rreturn365 ~ age + gender + enterage + TradesOptions + nsec + ntrade,
data = xx, index = c('client', 'account_date'), effect = 'time')
Sys.time() - time
summary(xxplmg)

