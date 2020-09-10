Sys.time()

library(data.table)

datapath = '../../data/'
outputpath = '../../output/'

insiders = fread(paste0(datapath, 'insiders_accounts.txt'),
                 sep = ";", header = T, col.names = c(
                   "client", "attr_id", "attr_val"))

write.csv(insiders, file = paste0(outputpath,"minimumexample.csv"))