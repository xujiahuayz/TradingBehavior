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


agerange = list(
  c(1, 2),
  c(2, 3.5),
  c(3.5, 5)
)

trselsplm = list()
ns = c()
for (k in 1:length(agerange)){
  pickage = agerange[[k]] * 365.2425
  i = pickage[2]
  popu = regtable[lifetime >= i & firstobs >= '2009-01-01']$client
  temp = temp0[age <= i & client %in% popu]
  ns = c(ns, temp$client %>% unique() %>% length())
  gc()
  temp[, agediff = age - pickage[1]]
  trselsplm[[k]] = pdata.frame(
    temp[, c('client', 'account_date', 'agediff', 'retpr')], 
    index=c('client', 'account_date'), 
    row.names = F
    )
  trsel = temp[, c(n=.N, funcmeanconf(retpr)), by = agediff][order(agediff)]
  trselscum[[k]] = trsel
  
  print(k)
}


#exited after two years and before 5
popu = regtable[lifetime >= 2 * 365.2425 & lifetime <= 5 * 365.2425 & 
                  is.finite(closing_date) & firstobs >= '2009-01-01']$client
temp = temp0[client %in% popu]
temp[, agediff = age - i]

trselsplm[[k+1]] = pdata.frame(
  temp[, c('client', 'account_date', 'agediff', 'retpr')], 
  index=c('client', 'account_date'), 
  row.names = F
  )

gc()
trsel = temp[age < 2 * 365.2425, c(n=.N,funcmeanconf(retpr)), by = agediff][order(agediff)]
trselscum[[k+1]] = trsel
ns = c(ns, temp$client %>% unique() %>% length())


time = Sys.time()
# when age is aligned, using plm/lm seems not make a difference 
xx11t = plm(retpr~age, data = trselsplm[[1]][trselsplm[[1]]$agediff < 365.2425*1.5,], effect = 'individual')
xx11t %>% fixef %>% t.test
summary(xx11t)
Sys.time() - time

time = Sys.time()
xx21t = plm(retpr~age, data = trselsplm[[2]][trselsplm[[2]]$agediff < 365.2425*1.5,], effect = 'individual')
xx21t %>% fixef %>% t.test
summary(xx21t)
Sys.time() - time

xx31t = plm(retpr~age, data = trselsplm[[3]][trselsplm[[3]]$agediff < 365.2425*1.5,], effect = 'individual')
xx31t %>% fixef %>% t.test
summary(xx31t)

xx41t = plm(retpr~age, data = trselsplm[[4]][trselsplm[[4]]$agediff < 365.2425*1.5,], effect = 'individual')
xx41t %>% fixef %>% t.test
summary(xx41t)

xx12t = plm(retpr~age, data = trselsplm[[1]][trselsplm[[1]]$agediff >= 365.2425*1.5 & trselsplm[[1]]$age < 365.2425*2,], effect = 'individual')
xx12t %>% fixef %>% t.test
summary(xx12t)

xx22t = plm(retpr~age, data = trselsplm[[2]][trselsplm[[2]]$agediff >= 365.2425*1.5 & trselsplm[[2]]$age < 365.2425*2,], effect = 'individual')
xx22t %>% fixef %>% t.test
summary(xx22t)

xx32t = plm(retpr~age, data = trselsplm[[3]][trselsplm[[3]]$agediff >= 365.2425*1.5 & trselsplm[[3]]$age < 365.2425*2,], effect = 'individual')
xx32t %>% fixef %>% t.test
summary(xx32t)

xx42t = plm(retpr~age, data = trselsplm[[4]][trselsplm[[4]]$agediff >= 365.2425*1.5 & trselsplm[[4]]$age < 365.2425*2,], effect = 'individual')
xx42t %>% fixef %>% t.test
summary(xx42t)

# xx23t = plm(retpr~age, data = trselsplm[[2]][trselsplm[[2]]$age >= 365.2425*2 & trselsplm[[2]]$age < 365.2425*3.5,], effect = 'individual')
# xx23t %>% fixef %>% t.test
# summary(xx23t)
# 
# xx33t = plm(retpr~age, data = trselsplm[[3]][trselsplm[[3]]$age >= 365.2425*2 & trselsplm[[3]]$age < 365.2425*3.5,], effect = 'individual')
# xx33t %>% fixef %>% t.test
# summary(xx33t)

xx34t = plm(retpr~age, data = trselsplm[[3]][trselsplm[[3]]$agediff >= 365.2425*2 & trselsplm[[3]]$age < 365.2425*5,], effect = 'individual')
xx34t %>% fixef %>% t.test
summary(xx34t)

