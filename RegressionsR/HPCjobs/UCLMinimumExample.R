Sys.time()
getwd()

library(data.table)

datapath = '/home/ucacjxu/TradingBehavior/data/'
outputpath = '/home/ucacjxu/TradingBehavior/output/'

insiders = fread(paste0(datapath, 'insiders_accounts.txt'),
                 sep = ";", header = T, col.names = c(
                   "client", "attr_id", "attr_val"))

write.csv(insiders, file = paste0(outputpath,"UCLminimumexample.csv"))