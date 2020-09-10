Sys.time()
getwd()

library(data.table)

datapath = '/home/jxu/TradingBehavior/data/'
outputpath = '/home/jxu/TradingBehavior/output/'

insiders = fread(paste0(datapath, 'insiders_accounts.txt'),
                 sep = ";", header = T, col.names = c(
                   "client", "attr_id", "attr_val"))

write.csv(insiders, file = paste0(outputpath,"minimumexample.csv"))