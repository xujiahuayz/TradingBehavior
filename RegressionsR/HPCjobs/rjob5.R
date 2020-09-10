library(data.table)
library(plm)
library(survival)
library(glmmML)


filepath = '/home/jxu/learnr/'
# load(paste0(filepath,'regtable.rda'))
# load(paste0(filepath,'cl_transferin.rda'))

load(paste0(filepath,'tsregressreg.rda'))

xx[, ':='(firstobs.y = NULL)]

yy = xx[, c('client','gender', 'rreturn365', 'TradesOptions', 'account_date' , 'closing_date', 'age', 'enterage')]
rm(xx)
gc()
yy[is.finite(rreturn365), ':='(
    retpr = ecdf(rreturn365)(rreturn365)), by = account_date]

yy[, ':='(ageplus1 = age + 1)][, ':='(
  event = ifelse(is.finite(closing_date), closing_date == account_date, F))]


coxphbase = coxph(Surv(time = age, time2 = ageplus1, event = event) ~  retpr + cluster(client), data = yy)
# baseline never changes irrespective of cluster!!! (is baseline the non-parametric Kaplan-Maier model)?
summary(coxphbase)
coxphbasefit = survfit(coxphbase)

coxphbaseext = coxph(Surv(time = age, time2 = ageplus1, event = event) ~  
        retpr + gender + enterage + TradesOptions +  cluster(client), data = yy)
summary(coxphbaseext)
coxphbasefitextfit = survfit(coxphbaseext)

save(coxphbase, coxphbasefit, coxphbaseext, coxphbasefitextfit, file = 'coxph.Rda')

