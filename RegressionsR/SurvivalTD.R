library(data.table)
library(tidyr)
library(lubridate)
library(lattice)
library(openintro)
library(zoo)
library(pracma)
library(nloptr)
library(speedglm)
library(timeDate)
library(riskRegression)
library(ggplot2)
library(gridExtra)

load('/media/tgodina/HDD/Data_31_03_2018/data/Trading/utrans_2001_2018.rda')
u.trans=u.trans[action %in% c('A','B','S','C','V','W','Y','Z')] #%>% .[!(asset_type %in% c(67,68,69))]
gc()
u.trans=u.trans[!duplicated(u.trans[,c('trade_id','client','date_trans','isin')])]#%>%.[asset_type %in% c(30, 31, 32)]
u.trans=u.trans[order(date_trans)] 
cl_shares=unique(u.trans$client)

get_data= function(str){
  return(get(load(str)))
}

#as.Date(cut(as.Date(cut(Sys.Date(), "quarter")) + 100, "quarter"))


sty = read.csv("/media/tgodina/HDD/Data_31_03_2018/data/securities_26512018.txt", sep = ";", header = TRUE)%>%as.data.table%>%
  setnames(c("sec_id", "cur_id", "prov_id","sec_type_id", "asset_type",
             "stock_exchange", "hash", "isin",      "cur", "symbol",
             "cotation_type", "cotation_factor", "contractsize",   "name", "strike",
             "exp_date", "call_put", "ric_code", "cfi_code", "maturity",
             "deliv_type", "eusipa_code", "strike_cur") )%>%.[,security_key := paste0(substr(isin, 1, 12), "_", stock_exchange, "_", cur)]

Securities=u.trans[!duplicated(security_key),'security_key'] %>% merge(.,sty[,c('sec_id','security_key')],by='security_key',all.x=TRUE)


#last_trade=u.trans[action %in% c('B','S','C','A'),list(last_tr=tail(date_trans,1)),by='client']
last_trade=u.trans[action %in% c('B','S','C','A'),list(last_tr=tail(date_trans,1)),by='client']

Tab4ext = read.csv("/home/godinat/Data_31_03_2018/data/Tab4_extended_09012018.txt", sep = ";", header = TRUE)%>%as.data.table%>%
  setnames(c("account_id", "client",  "mark_camp", "age", "country_res", "client_segm","type","brand_id", "brand_lab",
              "acc_type_id", "acc_type",  "status",  "status_val", "acc_status_date"))%>%setorder("client") 
sapply(Tab4ext, class)
persID = read.csv("/home/godinat/Data_31_03_2018/data/persons_accounts.txt", sep = ";", header = TRUE)%>%as.data.table%>%
  setnames( c("account_id", "person_id", "title_lable", "birth_year", "language", "numbered", "company_flag"))

acc_types = read.csv( paste0("/home/godinat/Data_31_03_2018/data/accounts_types.txt"), sep = ";", header = TRUE)%>%as.data.table()%>%
  setnames(c("client", "attr_id", "attr_val"))

Tab4ext= Tab4ext[,':=' ( acc_status_date= as.Date(acc_status_date))]
first_trade=u.trans[,list(date_trans=head(date_trans,1)),by='client']
Tab4=Tab4ext[status_val=='Open'] %>% .[!duplicated(client)]
Tab4[,acc_status_date:=NULL]
Tab4=merge(Tab4,first_trade,by='client')
colnames(Tab4)[14]='acc_status_date'

Tab4_lt=merge(last_trade,Tab4[,c('client','acc_status_date')],by='client',all.x=TRUE)
Tab4_lt=Tab4_lt[!is.na(acc_status_date)]

last_date=as.Date("2018-01-01")-1
date_of_interest=as.Date('2018-01-01')-1

last_trade_int=u.trans[action %in% c('A','B','S','C'),list(action=tail(action,1),last_tr_int=tail(date_trans[date_trans<=date_of_interest],1)),by='client']
Tab4_lt=merge(Tab4_lt,last_trade_int[,c('client','last_tr_int')],by='client',all.x=TRUE)

Closed_account=Tab4ext[status_val=='Closed' & acc_status_date<=last_date] %>% .[!duplicated(client)]


colnames(Closed_account)[14]='closing_date'
Tab4_lt=merge(Tab4_lt,Closed_account[,c('client','closing_date')],by='client',all.x = TRUE)
Tab4_lt[is.na(closing_date),5]=last_date+1
Tab4_lt=Tab4_lt%>%#.[closing_date>'2010-01-01']  %>% 
      .[client %in% unique(Tab4ext$client[Tab4ext$client_segm %in% c("HNW accounts", "Retail account") & (Tab4ext$brand_lab=='Swissquote')])] %>%
      .[(client %in% acc_types$client[acc_types$attr_val=='Direct client'])]

Date_seq=as.Date(timeLastDayInQuarter(seq(as.Date("2010-03-01"), by = "quarter", length.out = 32)))

Closed_tr=outer(Tab4_lt$last_tr,as.Date(timeLastDayInQuarter(Date_seq-91.25)),FUN='<=') %>% as.data.table(.)
colnames(Closed_tr)=as.character(Date_seq)
rownames(Closed_tr)=as.character(Tab4_lt$client)
Closed_tr[rowSums(Closed_tr[,-7:0+length(Date_seq),with=FALSE])<8,-7:0+length(Date_seq)]=FALSE


Closed_acc=outer(Tab4_lt$closing_date,as.Date(timeLastDayInQuarter(Date_seq-91.25)),FUN='<=') %>% as.data.table(.)
colnames(Closed_acc)=as.character(Date_seq)
rownames(Closed_acc)=as.character(Tab4_lt$client)


##### Balance:
Asset_pos_all=c()
for (i in c(2010:2017)){
  Tab2=get_data(paste0('/home/godinat/Data_31_03_2018/data/Tab2_',i,'.rda')) %>% .[client %in% Tab4_lt$client]
  Dates=unique(Tab2[,c('account_date')])
  Dates=Dates[,date:=as.Date(timeLastDayInQuarter(account_date))]
  Tab2=merge(Tab2,Dates,by='account_date',all.x=TRUE)
  Asset_pos=Tab2[,list(max_asset=max(assets_val)),by=c('client','date')]
  Asset_pos_all=rbind(Asset_pos_all,Asset_pos)
  gc()
}

Positive_assets=dcast(Asset_pos_all, client~date, value.var="max_asset") 
Positive_assets=rbind(Positive_assets,Tab4_lt[!(Tab4_lt$client %in% Asset_pos_all$client),'client'],fill=TRUE) %>% .[order(client)]
Positive_assets[is.na(Positive_assets)]=0


Total_matrix=(Closed_tr | Closed_acc) & (Positive_assets[,-1]<500)  %>% as.data.table(.)#& (Positive_assets[,-1]<500)
colnames(Total_matrix)=as.character(Date_seq)
rownames(Total_matrix)=as.character(Tab4_lt$client)
Total_matrix=t(apply(Total_matrix,1,cumsum))>0




Age=-outer(Tab4_lt$first_tr,Date_seq,FUN='-')# %>% as.data.table(.) 
Age=floor(Age/(91.25))#%>% as.data.table(.) 
Age[Age<1]=NA
colnames(Age)=as.character(Date_seq)
rownames(Age)=as.character(Tab4_lt$client)


Trade_Activity=u.trans[,list(Active=length(action)>0),by=c('client','date_trans')]
Trade_Matrix=Trade_Activity[,list(list(apply(outer(date_trans,Date_seq,FUN=function(x,y)return(as.numeric(y-x)<91.25 & as.numeric(y-x)>=0)),2,any))),
                                  by='client']
Trade_Matrix=Trade_Matrix[order(client)] %>% .[client %in% Tab4_lt$client]

Active_Matrix=do.call(rbind,Trade_Matrix$V1)

colnames(Active_Matrix)=as.character(Date_seq)
rownames(Active_Matrix)=as.character(Tab4_lt$client)

#outer(Age[,1],Age[,1]
store_0=data.frame(age=unique(Age[!is.na(Age)])/4)
store_0_sd=data.frame(age=unique(Age[!is.na(Age)])/4)
store_0_nr=data.frame(age=unique(Age[!is.na(Age)])/4)
store_1=data.frame(age=unique(Age[!is.na(Age)])/4)
store_1_sd=data.frame(age=unique(Age[!is.na(Age)])/4)
store_1_nr=data.frame(age=unique(Age[!is.na(Age)])/4)
for (t in 1:32){
  x=Age[,t]
  store=data.frame(age=as.numeric(),prob=as.numeric())
  store_sd=data.frame(age=as.numeric(),prob=as.numeric())
  store_nr=data.frame(age=as.numeric(),prob=as.numeric())
  colnames(store)[2]=paste0('prob_',t)
  colnames(store_sd)[2]=paste0('prob_',t)
  colnames(store_nr)[2]=paste0('prob_',t)
  for (i in unique(x[order(x)])%>%.[!is.na(.)]){
    store[i,]=c(i/4,mean(Total_matrix[x==i,t]*(x[x==i]>0),na.rm=TRUE))
    store_sd[i,]=c(i/4,sd(Total_matrix[x==i,t]*(x[x==i]>0),na.rm=TRUE))
    store_nr[i,]=c(i/4,sum(!is.na(Total_matrix[x==i,t]*(x[x==i]>0))))
  }      

  #store=store[-dim(store)[1],]
  
  store1=data.frame(age=as.numeric(),prob=as.numeric())
  store1_sd=data.frame(age=as.numeric(),prob=as.numeric())
  store1_nr=data.frame(age=as.numeric(),prob=as.numeric())
  colnames(store1)[2]=paste0('prob_',t)
  colnames(store1_sd)[2]=paste0('prob_',t)
  colnames(store1_nr)[2]=paste0('prob_',t)
  for (i in unique(x[order(x)])%>%.[!is.na(.)]){
    y=sum(x==i,na.rm=TRUE)
    store1[i,]=c(i/4,sum((1-Total_matrix[x==i,t])*Active_Matrix[x==i,t],na.rm=TRUE)/sum(1-Total_matrix[x==i,t],na.rm=TRUE))
    store1_sd[i,]=c(i/4,sd((1-Total_matrix[x==i,t])*Active_Matrix[x==i,t],na.rm=TRUE))
    store1_nr[i,]=c(i/4,sum(1-Total_matrix[x==i,t],na.rm=TRUE))
  }      
  #store1=store1[-dim(store1)[1],]
  store_0=merge(store_0,store,by='age',all=TRUE) 
  store_0=store_0[!is.na(store_0[,1]),]
  store_0_sd=merge(store_0_sd,store_sd,by='age',all=TRUE) 
  store_0_sd=store_0_sd[!is.na(store_0_sd[,1]),]
  store_0_nr=merge(store_0_nr,store_nr,by='age',all=TRUE) 
  store_0_nr=store_0_nr[!is.na(store_0_nr[,1]),]
  
  store_1=merge(store_1,store1,by='age',all=TRUE)
  store_1=store_1[!is.na(store_1[,1]),]
  store_1_sd=merge(store_1_sd,store1_sd,by='age',all=TRUE)
  store_1_sd=store_1_sd[!is.na(store_1_sd[,1]),]
  store_1_nr=merge(store_1_nr,store1_nr,by='age',all=TRUE)
  store_1_nr=store_1_nr[!is.na(store_1_nr[,1]),]
}

######## Poisson surival
#A=8+rowSums(1-Total_matrix,na.rm=TRUE)


par(mfrow=c(2,3))
i=2
plot(x=store_0[!is.na(store_0[,i+3]),1],y=(1-store_0[!is.na(store_0[,i+3]),i])*100,ylim=c(20,100),col=1,type='l',
     main='Survival probability 2010',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_0[,1],y=(1-store_0[,i+1])*100,col=2)
lines(x=store_0[,1],y=(1-store_0[,i+2])*100,col=3)
lines(x=store_0[,1],y=(1-store_0[,i+3])*100,col=4)
legend(7,100,c('Q1','Q2','Q3','Q4'),col=c(1:4),pch=1,cex=0.6)

i=6
plot(x=store_0[!is.na(store_0[,i+3]),1],y=(1-store_0[!is.na(store_0[,i+3]),i])*100,ylim=c(20,100),col=1,type='l',
     main='Survival probability 2011',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_0[,1],y=(1-store_0[,i+1])*100,col=2)
lines(x=store_0[,1],y=(1-store_0[,i+2])*100,col=3)
lines(x=store_0[,1],y=(1-store_0[,i+3])*100,col=4)
legend(8,100,c('Q1','Q2','Q3','Q4'),col=c(1:4),pch=1,cex=0.6)

i=10
plot(x=store_0[!is.na(store_0[,i+3]),1],y=(1-store_0[!is.na(store_0[,i+3]),i])*100,ylim=c(20,100),col=1,type='l',
     main='Survival probability 2012',ylab='Percentage %',xlab='Time in SQ',cex.main=1,cex.lab=1)
lines(x=store_0[,1],y=(1-store_0[,i+1])*100,col=2)
lines(x=store_0[,1],y=(1-store_0[,i+2])*100,col=3)
lines(x=store_0[,1],y=(1-store_0[,i+3])*100,col=4)
legend(9,100,c('Q1','Q2','Q3','Q4'),col=c(1:4),pch=1,cex=0.6)

i=2
plot(x=store_1[!is.na(store_1[,i+3]),1],y=(store_1[!is.na(store_1[,i+3]),i])*100,ylim=c(20,100),col=1,type='l',
     main='Probability to trade 2010',ylab='Percentage %',xlab='Time in SQ',cex.main=1,cex.lab=1)
lines(x=store_1[,1],y=(store_1[,i+1])*100,col=2)
lines(x=store_1[,1],y=(store_1[,i+2])*100,col=3)
lines(x=store_1[,1],y=(store_1[,i+3])*100,col=4)
legend(7,65,c('Q1','Q2','Q3','Q4'),col=c(1:4),pch=1,cex=0.6)

i=6
plot(x=store_1[!is.na(store_1[,i+3]),1],y=(store_1[!is.na(store_1[,i+3]),i])*100,ylim=c(20,100),col=1,type='l',
     main='Probability to trade 2011',ylab='Percentage %',xlab='Time in SQ',cex.main=1,cex.lab=1)
lines(x=store_1[,1],y=(store_1[,i+1])*100,col=2)
lines(x=store_1[,1],y=(store_1[,i+2])*100,col=3)
lines(x=store_1[,1],y=(store_1[,i+3])*100,col=4)
legend(8,100,c('Q1','Q2','Q3','Q4'),col=c(1:4),pch=1,cex=0.6)


i=10
plot(x=store_1[!is.na(store_1[,i+3]),1],y=(store_1[!is.na(store_1[,i+3]),i])*100,ylim=c(20,100),col=1,type='l',
     main='Probability to trade 2012',ylab='Percentage %',xlab='Time in SQ',cex.main=1,cex.lab=1)
lines(x=store_1[,1],y=(store_1[,i+1])*100,col=2)
lines(x=store_1[,1],y=(store_1[,i+2])*100,col=3)
lines(x=store_1[,1],y=(store_1[,i+3])*100,col=4)
legend(9,100,c('Q1','Q2','Q3','Q4'),col=c(1:4),pch=1,cex=0.6)


par(mfrow=c(2,3))
i=14
plot(x=store_0[!is.na(store_0[,i+3]),1],y=(1-store_0[!is.na(store_0[,i+3]),i])*100,ylim=c(20,100),col=1,type='l',
     main='Survival probability 2013',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_0[,1],y=(1-store_0[,i+1])*100,col=2)
lines(x=store_0[,1],y=(1-store_0[,i+2])*100,col=3)
lines(x=store_0[,1],y=(1-store_0[,i+3])*100,col=4)
legend(9,100,c('Q1','Q2','Q3','Q4'),col=c(1:4),pch=1,cex=0.6)

i=18
plot(x=store_0[!is.na(store_0[,i+3]),1],y=(1-store_0[!is.na(store_0[,i+3]),i])*100,ylim=c(20,100),col=1,type='l',
     main='Survival probability 2014',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_0[,1],y=(1-store_0[,i+1])*100,col=2)
lines(x=store_0[,1],y=(1-store_0[,i+2])*100,col=3)
lines(x=store_0[,1],y=(1-store_0[,i+3])*100,col=4)
legend(10,100,c('Q1','Q2','Q3','Q4'),col=c(1:4),pch=1,cex=0.6)

i=22
plot(x=store_0[!is.na(store_0[,i+3]),1],y=(1-store_0[!is.na(store_0[,i+3]),i])*100,ylim=c(20,100),col=1,type='l',
     main='Survival probability 2015',ylab='Percentage %',xlab='Time in SQ',cex.main=1,cex.lab=1)
lines(x=store_0[,1],y=(1-store_0[,i+1])*100,col=2)
lines(x=store_0[,1],y=(1-store_0[,i+2])*100,col=3)
lines(x=store_0[,1],y=(1-store_0[,i+3])*100,col=4)
legend(11,100,c('Q1','Q2','Q3','Q4'),col=c(1:4),pch=1,cex=0.6)

i=14
plot(x=store_1[!is.na(store_1[,i+3]),1],y=(store_1[!is.na(store_1[,i+3]),i])*100,ylim=c(20,100),col=1,type='l',
     main='Probability to trade 2013',ylab='Percentage %',xlab='Time in SQ',cex.main=1,cex.lab=1)
lines(x=store_1[,1],y=(store_1[,i+1])*100,col=2)
lines(x=store_1[,1],y=(store_1[,i+2])*100,col=3)
lines(x=store_1[,1],y=(store_1[,i+3])*100,col=4)
legend(9,100,c('Q1','Q2','Q3','Q4'),col=c(1:4),pch=1,cex=0.6)

i=18
plot(x=store_1[!is.na(store_1[,i+3]),1],y=(store_1[!is.na(store_1[,i+3]),i])*100,ylim=c(20,100),col=1,type='l',
     main='Probability to trade 2014',ylab='Percentage %',xlab='Time in SQ',cex.main=1,cex.lab=1)
lines(x=store_1[,1],y=(store_1[,i+1])*100,col=2)
lines(x=store_1[,1],y=(store_1[,i+2])*100,col=3)
lines(x=store_1[,1],y=(store_1[,i+3])*100,col=4)
legend(10,100,c('Q1','Q2','Q3','Q4'),col=c(1:4),pch=1,cex=0.6)


i=22
plot(x=store_1[!is.na(store_1[,i+3]),1],y=(store_1[!is.na(store_1[,i+3]),i])*100,ylim=c(20,100),col=1,type='l',
     main='Probability to trade 2015',ylab='Percentage %',xlab='Time in SQ',cex.main=1,cex.lab=1)
lines(x=store_1[,1],y=(store_1[,i+1])*100,col=2)
lines(x=store_1[,1],y=(store_1[,i+2])*100,col=3)
lines(x=store_1[,1],y=(store_1[,i+3])*100,col=4)
legend(11,100,c('Q1','Q2','Q3','Q4'),col=c(1:4),pch=1,cex=0.6)


par(mfrow=c(2,3))
i=26
plot(x=store_0[!is.na(store_0[,i+3]),1],y=(1-store_0[!is.na(store_0[,i+3]),i])*100,ylim=c(20,100),col=1,type='l',
     main='Survival probability 2016',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_0[,1],y=(1-store_0[,i+1])*100,col=2)
lines(x=store_0[,1],y=(1-store_0[,i+2])*100,col=3)
lines(x=store_0[,1],y=(1-store_0[,i+3])*100,col=4)
legend(12,100,c('Q1','Q2','Q3','Q4'),col=c(1:4),pch=1,cex=0.6)

i=30
plot(x=store_0[!is.na(store_0[,i+3]),1],y=(1-store_0[!is.na(store_0[,i+3]),i])*100,ylim=c(20,100),col=1,type='l',
     main='Survival probability 2017',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_0[,1],y=(1-store_0[,i+1])*100,col=2)
lines(x=store_0[,1],y=(1-store_0[,i+2])*100,col=3)
lines(x=store_0[,1],y=(1-store_0[,i+3])*100,col=4)
legend(13,100,c('Q1','Q2','Q3','Q4'),col=c(1:4),pch=1,cex=0.6)

#i=34
#plot(x=store_0[!is.na(store_0[,i+1]),1],y=(1-store_0[!is.na(store_0[,i+1]),i])*100,ylim=c(20,100),col=1,type='l',
#     main='Survival probability 2018',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
#lines(x=store_0[,1],y=(1-store_0[,i+1])*100,col=2)
#legend(13,100,c('Q1','Q2'),col=c(1:2),pch=1,cex=0.6)

i=26
plot(x=store_1[!is.na(store_1[,i+3]),1],y=(store_1[!is.na(store_1[,i+3]),i])*100,ylim=c(20,100),col=1,type='l',
     main='Probability to trade 2016',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_1[,1],y=(store_1[,i+1])*100,col=2)
lines(x=store_1[,1],y=(store_1[,i+2])*100,col=3)
lines(x=store_1[,1],y=(store_1[,i+3])*100,col=4)
legend(12,30,c('Q1','Q2','Q3','Q4'),col=c(1:4),pch=1,cex=0.6)

i=30
plot(x=store_1[!is.na(store_1[,i+3]),1],y=(store_1[!is.na(store_1[,i+3]),i])*100,ylim=c(20,100),col=1,type='l',
     main='Probability to trade 2017',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_1[,1],y=(store_1[,i+1])*100,col=2)
lines(x=store_1[,1],y=(store_1[,i+2])*100,col=3)
lines(x=store_1[,1],y=(store_1[,i+3])*100,col=4)
legend(13,30,c('Q1','Q2','Q3','Q4'),col=c(1:4),pch=1,cex=0.6)

#i=34
#plot(x=store_1[!is.na(store_1[,i+1]),1],y=(store_1[!is.na(store_1[,i+1]),i])*100,ylim=c(20,100),col=1,type='l',
#     main='Probability to trade 2018',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
#lines(x=store_1[,1],y=(store_1[,i+1])*100,col=2)
#legend(13,30,c('Q1','Q2'),col=c(1:2),pch=1,cex=0.6)

par(mfrow=c(1,2))
plot(x=store_0[-68,1],y=(1-rowMeans(store_0[-68,-1],na.rm=TRUE))*100,ylim=c(30,100),col=1,type='l',
     main='Aggregated survival probability',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
plot(x=store_1[-68,1],y=(rowMeans(store_1[-68,-1],na.rm=TRUE))*100,ylim=c(35,100),col=1,type='l',
     main='Aggregated probability to trade quaterly',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
#####################################################
##################### Confidence intervals ###########
par(mfrow=c(2,3))
i=27
plot(x=store_0[!is.na(store_0[,i]),1],y=(1-store_0[!is.na(store_0[,i]),i])*100,ylim=c(20,100),col=1,type='l',
     main='Survival probability 2016 Q2',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_0[!is.na(store_0[,i]),1],y=(1-store_0[!is.na(store_0[,i]),i])*100+100*1.96*(store_0_sd[!is.na(store_0[,i]),i])/
        sqrt(store_0_nr[!is.na(store_0[,i]),i]),col=2,lty=2)
lines(x=store_0[!is.na(store_0[,i]),1],y=(1-store_0[!is.na(store_0[,i]),i])*100-100*1.96*(store_0_sd[!is.na(store_0[,i]),i])/
        sqrt(store_0_nr[!is.na(store_0[,i]),i]),col=3,lty=2)
legend(6.5,100,c('Survival Probability','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

i=31
plot(x=store_0[!is.na(store_0[,i]),1],y=(1-store_0[!is.na(store_0[,i]),i])*100,ylim=c(20,100),col=1,type='l',
     main='Survival probability 2017 Q2',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_0[!is.na(store_0[,i]),1],y=(1-store_0[!is.na(store_0[,i]),i])*100+100*1.96*(store_0_sd[!is.na(store_0[,i]),i])/
        sqrt(store_0_nr[!is.na(store_0[,i]),i]),col=2,lty=2)
lines(x=store_0[!is.na(store_0[,i]),1],y=(1-store_0[!is.na(store_0[,i]),i])*100-100*1.96*(store_0_sd[!is.na(store_0[,i]),i])/
        sqrt(store_0_nr[!is.na(store_0[,i]),i]),col=3,lty=2)
legend(7,100,c('Survival Probability','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

#i=35
#plot(x=store_0[!is.na(store_0[,i]),1],y=(1-store_0[!is.na(store_0[,i]),i])*100,ylim=c(20,100),col=1,type='l',
#     main='Survival probability 2018 Q2',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
#lines(x=store_0[!is.na(store_0[,i]),1],y=(1-store_0[!is.na(store_0[,i]),i])*100+100*1.96*(store_0_sd[!is.na(store_0[,i]),i])/
#        sqrt(store_0_nr[!is.na(store_0[,i]),i]),col=2,lty=2)
#lines(x=store_0[!is.na(store_0[,i]),1],y=(1-store_0[!is.na(store_0[,i]),i])*100-100*1.96*(store_0_sd[!is.na(store_0[,i]),i])/
#        sqrt(store_0_nr[!is.na(store_0[,i]),i]),col=3,lty=2)
#legend(7.5,100,c('Survival Probability','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

i=27
plot(x=store_1[!is.na(store_1[,i]),1],y=(store_1[!is.na(store_1[,i]),i])*100,ylim=c(20,100),col=1,type='l',
     main='Probability to trade 2016 Q2',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_1[!is.na(store_1[,i]),1],y=(store_1[!is.na(store_0[,i]),i])*100+100*1.96*(store_1_sd[!is.na(store_1[,i]),i])/
        sqrt(store_1_nr[!is.na(store_1[,i]),i]),col=2,lty=2)
lines(x=store_1[!is.na(store_1[,i]),1],y=(store_1[!is.na(store_0[,i]),i])*100-100*1.96*(store_1_sd[!is.na(store_1[,i]),i])/
        sqrt(store_1_nr[!is.na(store_1[,i]),i]),col=3,lty=2)
legend(6.5,100,c('Probability to trade','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

i=31
plot(x=store_1[!is.na(store_1[,i]),1],y=(store_1[!is.na(store_1[,i]),i])*100,ylim=c(20,100),col=1,type='l',
     main='Probability to trade 2017 Q2',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_1[!is.na(store_1[,i]),1],y=(store_1[!is.na(store_0[,i]),i])*100+100*1.96*(store_1_sd[!is.na(store_1[,i]),i])/
        sqrt(store_1_nr[!is.na(store_1[,i]),i]),col=2,lty=2)
lines(x=store_1[!is.na(store_1[,i]),1],y=(store_1[!is.na(store_0[,i]),i])*100-100*1.96*(store_1_sd[!is.na(store_1[,i]),i])/
        sqrt(store_1_nr[!is.na(store_1[,i]),i]),col=3,lty=2)
legend(7,100,c('Probability to trade','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

#i=35
#plot(x=store_1[!is.na(store_1[,i]),1],y=(store_1[!is.na(store_1[,i]),i])*100,ylim=c(20,100),col=1,type='l',
#     main='Probability to trade 2018 Q2',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
#lines(x=store_1[!is.na(store_1[,i]),1],y=(store_1[!is.na(store_0[,i]),i])*100+100*1.96*(store_1_sd[!is.na(store_1[,i]),i])/
#        sqrt(store_1_nr[!is.na(store_1[,i]),i]),col=2,lty=2)
#lines(x=store_1[!is.na(store_1[,i]),1],y=(store_1[!is.na(store_0[,i]),i])*100-100*1.96*(store_1_sd[!is.na(store_1[,i]),i])/
#        sqrt(store_1_nr[!is.na(store_1[,i]),i]),col=3,lty=2)
#legend(7.5,100,c('Probability to trade','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

CI_0=100*1.96*sqrt(rowMeans(store_0_sd[,-1]^2,na.rm=TRUE))/sqrt(rowSums(store_0_nr[,-1],na.rm=TRUE))
CI_1=100*1.96*sqrt(rowMeans(store_1_sd[,-1]^2,na.rm=TRUE))/sqrt(rowSums(store_1_nr[,-1],na.rm=TRUE))

par(mfrow=c(1,2))
plot(x=store_0[,1],y=(1-rowMeans(store_0[,-1],na.rm=TRUE))*100,ylim=c(40,100),col=1,type='l',
     main='Aggregated survival probability',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_0[,1],y=(1-rowMeans(store_0[,-1],na.rm=TRUE))*100+CI_0,col=2,lty=2)
lines(x=store_0[,1],y=(1-rowMeans(store_0[,-1],na.rm=TRUE))*100-CI_0,col=3,lty=2)
legend(6.5,100,c('Probability to trade','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))
plot(x=store_1[,1],y=(rowMeans(store_1[,-1],na.rm=TRUE))*100,ylim=c(35,60),col=1,type='l',
     main='Aggregated probability to trade quaterly',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_1[,1],y=(rowMeans(store_1[,-1],na.rm=TRUE))*100+CI_1,col=2,lty=2)
lines(x=store_1[,1],y=(rowMeans(store_1[,-1],na.rm=TRUE))*100-CI_1,col=3,lty=2)
legend(6.5,60,c('Probability to trade','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))
####################################################
####################################################
new_fun=function(alpha){
  a=0
  for (i in 1:66){
    a=a+(1/(1+alpha*i*0.25)-(1-rowMeans(store_0[i,-1],na.rm=TRUE)))^2
  }
  return(a)
}

res1=nloptr(x0=c(1),lb=c(0.0001),eval_f=new_fun,opts=list( "algorithm" = "NLOPT_LN_COBYLA" ,"xtol_rel"= 1.0e-8))
res1
#gammainc(0.008423679,0.02013317*0.25)[3]


a=c()
for (i in 1:66){
  a=c(a,gammainc(0.008423679,0.02013317*i*0.25)[3])
}

###pareto type
plot((1+(store_0[-67, 1]/2.37736))^(-0.5161399),x=store_0[-67, 1],type='l',xlab='Time with SQ',ylab='Surivival probability',main='Survival probability')
lines(x=store_0[-67, 1],y=(1-rowMeans(store_0[-67,-1],na.rm=TRUE)),col='red')
legend(12, 0.9, legend=c("Pareto II", "Empirical"),
       col=c("black", "red"), lty=c(1,1), cex=0.8)

###pareto type
plot((1+(store_0[-67, 1]/2.37736))^(-0.5161399),x=store_0[-67, 1],type='l',xlab='Time with SQ',ylab='Surivival probability',main='Survival probability')
lines(x=store_0[-67, 1],y=(1-rowMeans(store_0[-67,-1],na.rm=TRUE)),col='red')
legend(12, 0.9, legend=c("Pareto II", "Empirical"),
       col=c("black", "red"), lty=c(1,1), cex=0.8)

plot((1/(1+0.1407*store_0[-67, 1])),x=store_0[-67, 1],type='l',xlab='Time with SQ',ylab='Surivival probability',main='Survival probability')
lines(x=store_0[-67, 1],y=(1-rowMeans(store_0[-67,-1],na.rm=TRUE)),col='red')
legend(12, 0.9, legend=c("Parametric", "Empirical"),
       col=c("black", "red"), lty=c(1,1), cex=0.8)

###### Check quit time

asd=rowSums(1-Total_matrix[,-c(31:34)]) %>% .[!(. %in% c(0,30))]
Quit_quarter=Tab4_lt[client %in% names(asd)]
Quit_quarter=Quit_quarter[client!=656559]

asd=rowSums(1-Total_matrix[,-c(31:34)]) %>% .[(. %in% c(30))]
Still_here=Tab4_lt[client %in% names(asd)]
Still_here_perf=Tab_month[client %in% Still_here$client & account_date>='2015-07-01',list(r_tot=mean(r_true)),by='client']
c(median(Still_here_perf$r_tot),mean(Still_here_perf$r_tot))
#####

Quit_perf=Tab_month[client %in% Quit_quarter$client,
                    list(r_tot=(prod(((1+r_true))[r_true!=0 & account_date<=Quit_quarter$last_tr_int[Quit_quarter$client==client[1]]+30
                                                          & account_date>Quit_quarter$last_tr_int[Quit_quarter$client==client[1]]-365]))-1),
                    by='client']
c(median(Quit_perf$r_tot,na.rm=TRUE),mean(Quit_perf$r_tot,na.rm=TRUE))*100

Still_here_perf=Tab_month[client %in% Still_here$client,
                          list(r_tot=(prod(((1+r_true))[r_true!=0 & account_date<=Still_here$last_tr_int[Still_here$client==client[1]]+30
                                                        & account_date>Still_here$last_tr_int[Still_here$client==client[1]]-365]))-1),
                          by='client']
c(median(Still_here_perf$r_tot,na.rm=TRUE),mean(Still_here_perf$r_tot,na.rm=TRUE))

Quit_perf
Quit_perf[,type:='Quit']
Still_here_perf[,type:='Not-quit']
Total_perf=rbind(Quit_perf,Still_here_perf)
Total_perf[,Quit:=type=='Quit']
histogram(Still_here_perf$r_tot[Still_here_perf$r_tot!=0],breaks=50,xlab='Returns',main='Perfomance 1-year until last trade (still in SQ by 2017-06) \n median=5.36% mean=3.48%')
ggplot(Total_perf[r_tot<1.5], aes(r_tot, fill = type)) + geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity',bins=50)+xlab('Returns')+ggtitle('Performance 1-year until last trade')

histogram(Quit_perf$r_tot[Quit_perf$r_tot!=0],breaks=50,xlab='Returns',main='Perfomance 1-year until last trade (quiters)\n median=-2.38% mean=-7.09%')
lin=speedglm(I(Quit) ~ r_tot, data=Total_perf, family=binomial(link="logit"))
summary(lin)



################ Distribution AgeA
Stayer=Age*(1-Total_matrix)

par(mfrow=c(3,3))
hist(as.numeric(Stayer[Stayer[,2]!=0,2])/4,36,xlab='Age (approximated to 0.25 precision)',main='Age of survivers 2010 Q2')
hist(as.numeric(Stayer[Stayer[,6]!=0,6])/4,40,xlab='Age (approximated to 0.25 precision)',main='Age of survivers 2011 Q2')
hist(as.numeric(Stayer[Stayer[,10]!=0,10])/4,44,xlab='Age (approximated to 0.25 precision)',main='Age of survivers 2012 Q2')
hist(as.numeric(Stayer[Stayer[,14]!=0,14])/4,48,xlab='Age (approximated to 0.25 precision)',main='Age of survivers 2013 Q2')
hist(as.numeric(Stayer[Stayer[,18]!=0,18])/4,52,xlab='Age (approximated to 0.25 precision)',main='Age of survivers 2014 Q2')
hist(as.numeric(Stayer[Stayer[,22]!=0,22])/4,56,xlab='Age (approximated to 0.25 precision)',main='Age of survivers 2015 Q2')
hist(as.numeric(Stayer[Stayer[,26]!=0,26])/4,60,xlab='Age (approximated to 0.25 precision)',main='Age of survivers 2016 Q2')
hist(as.numeric(Stayer[Stayer[,30]!=0,30])/4,64,xlab='Age (approximated to 0.25 precision)',main='Age of survivers 2017 Q2')
#hist(as.numeric(Stayer[Stayer[,34]!=0,34])/4,68,xlab='Age (approximated to 0.25 precision)',main='Age of survivers 2018 Q2')

### First quit

Quiter=t(apply(Total_matrix,1,cumsum))
Quiter=Quiter[,-1]

Age_quit=Age[,-1]
hist(as.numeric(Age_quit[Quiter[,]==1])/4,42)
hist(as.numeric(Age_quit[Quiter==1])/4,68,xlab='Time with SQ',main='Time with SQ at the "Quit" (2010-2018)\n rounded to quarters')

