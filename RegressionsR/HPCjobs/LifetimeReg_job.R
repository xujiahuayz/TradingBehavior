library(data.table)
library(plm)
library(magrittr)

getwd()

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
                        retpr = ecdf(ret)(ret)), by = account_date]


pickage = c(2, 3.5, 5) * 364.2425 # pick an age

trselsplm = list()
ns = c()
for (k in 1:length(pickage)){
  
  i = pickage[k]
  popu = regtable[lifetime >= i & firstobs >= '2009-01-01']$client
  temp = temp0[age <= i & client %in% popu]
  ns = c(ns, temp$client %>% unique() %>% length())
  gc()
  
  trselsplm[[k]] = pdata.frame(temp[, c('client', 'account_date', 'age', 'retpr')], index=c('client', 'account_date'), row.names = F)
  trsel = temp[, c(n=.N, funcmeanconf(retpr)), by = age][order(age)]
  trselscum[[k]] = trsel
  
  print(k)
}


#exited after two years and before 5
popu = regtable[lifetime >= 2 * 364.2425 & lifetime <= 5 * 364.2425 & 
                  is.finite(closing_date) & firstobs >= '2009-01-01']$client
temp = temp0[client %in% popu]

trselsplm[[k+1]] = pdata.frame(
  temp[, c('client', 'account_date', 'age', 'retpr')], 
  index=c('client', 'account_date'), 
  row.names = F
  )

gc()
trsel = temp[age < 2 * 364.2425, c(n=.N,funcmeanconf(retpr)), by = age][order(age)]
trselscum[[k+1]] = trsel
ns = c(ns, temp$client %>% unique() %>% length())



# when age is aligned, using plm/lm seems not make a difference 
xx11t = plm(retpr~age, data = trselsplm[[1]][trselsplm[[1]]$age < 365.2425*1.5,], effect = 'time')
xx11t %>% fixef %>% t.test
summary(xx11t)

xx21t = plm(retpr~age, data = trselsplm[[2]][trselsplm[[2]]$age < 365.2425*1.5,], effect = 'time')
xx21t %>% fixef %>% t.test
summary(xx21t)

xx31t = plm(retpr~age, data = trselsplm[[3]][trselsplm[[3]]$age < 365.2425*1.5,], effect = 'time')
xx31t %>% fixef %>% t.test
summary(xx31t)

xx41t = plm(retpr~age, data = trselsplm[[4]][trselsplm[[4]]$age < 365.2425*1.5,], effect = 'time')
xx41t %>% fixef %>% t.test
summary(xx41t)

xx12t = plm(retpr~age, data = trselsplm[[1]][trselsplm[[1]]$age >= 365.2425*1.5 & trselsplm[[1]]$age < 365.2425*2,], effect = 'time')
xx12t %>% fixef %>% t.test
summary(xx12t)

xx22t = plm(retpr~age, data = trselsplm[[2]][trselsplm[[2]]$age >= 365.2425*1.5 & trselsplm[[2]]$age < 365.2425*2,], effect = 'time')
xx22t %>% fixef %>% t.test
summary(xx22t)

xx32t = plm(retpr~age, data = trselsplm[[3]][trselsplm[[3]]$age >= 365.2425*1.5 & trselsplm[[3]]$age < 365.2425*2,], effect = 'time')
xx32t %>% fixef %>% t.test
summary(xx32t)

xx42t = plm(retpr~age, data = trselsplm[[4]][trselsplm[[4]]$age >= 365.2425*1.5 & trselsplm[[4]]$age < 365.2425*2,], effect = 'time')
xx42t %>% fixef %>% t.test
summary(xx42t)

xx23t = plm(retpr~age, data = trselsplm[[2]][trselsplm[[2]]$age >= 365.2425*2 & trselsplm[[2]]$age < 365.2425*3.5,], effect = 'time')
xx23t %>% fixef %>% t.test
summary(xx23t)

xx33t = plm(retpr~age, data = trselsplm[[3]][trselsplm[[3]]$age >= 365.2425*2 & trselsplm[[3]]$age < 365.2425*3.5,], effect = 'time')
xx33t %>% fixef %>% t.test
summary(xx33t)

xx34t = plm(retpr~age, data = trselsplm[[3]][trselsplm[[3]]$age >= 365.2425*3.5 & trselsplm[[3]]$age < 365.2425*5,], effect = 'time')
xx34t %>% fixef %>% t.test
summary(xx34t)

