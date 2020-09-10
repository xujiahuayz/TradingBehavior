library(data.table)
library(plm)
#library(pglm)
library(glmmML)


filepath = '/home/jxu/learnr/'
# load(paste0(filepath,'regtable.rda'))
# load(paste0(filepath,'cl_transferin.rda'))

load(paste0(filepath,'tsregressreg.rda'))

xx[, ':='(firstobs.y = NULL)]

xx[account_date < '2018-01-01', ':='(
  ExitNxtYr = ifelse(is.finite(closing_date), (closing_date - account_date) < 365.2425, F))]

#gc()
#aa =  glmmboot(ExitNxtYr ~ age + rreturn365, family=binomial(link="logit"), data=xx[age < 365.2425 *3], cluster=account_date, na.action=na.omit)
#aa
#summary(aa)

gc()
aa =  glmmboot(ExitNxtYr ~ age + rreturn365, family=binomial(link="logit"), data=xx[age >= 365.2425 *3], cluster=account_date, na.action=na.omit)
aa
summary(aa)

gc()
aa =  glmmboot(ExitNxtYr ~ age + rreturn365, family=binomial(link="logit"), data=xx, cluster=account_date, na.action=na.omit)
aa
summary(aa)
