#########################################
############## Losers/Winners ###########
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
source("~/workspace/R/OWM_research/OWMfga/R-ex/OWMresearch.R")

last_date=as.Date('2017-12-31')
load('/home/godinat/Data_31_03_2018/data/Trading/utrans_2001_2018.rda')
u.trans=u.trans[!is.na(asset_type)] %>% .[!(asset_type %in% c(67,68,69))] %>%.[date_trans<=last_date] %>% .[isin!='']
u.trans[,isin:=substr(isin,1,12)]
u.trans[is.na(trade_id),'trade_id']=0
u.trans=u.trans[order(date_trans,trade_id)] %>% .[action %in% c('A','B','C','S','D','E')] #.[action %in% c('A','B','C','S')]  

SMI=OWMbdh(ticker =  "SMI Index", field = "PX_LAST", start.date = as.Date('2001-01-01'),  end.date = last_date)%>%.[!is.na(.)]
SMI_def=cbind(as.data.table(time(SMI)),as.data.table(SMI))
colnames(SMI_def)=c("date",'price_SMI')
SMI_def=merge(SMI_def,u.trans[!duplicated(date_trans),'date_trans'],by.x='date',by.y='date_trans',all=TRUE)
SMI_def[,SMI:=na.locf(na.locf(SMI_def$price_SMI), fromLast=T)]

u.trans=merge(u.trans,SMI_def[,c('date','SMI')],by.x='date_trans',by.y='date',all.x=TRUE)

Fun_price=function(vol_chf_trans,qty_trans,action){
  cum_qty=cumsum(qty_trans*(action %in% c('A','B'))-qty_trans*(action %in% c('S','C')))
  cum_qty=cum_qty-min(c(0,cum_qty))
  ind=head(which(action %in% c('A','B','C','S')),1)
  price=rep(NA,length(cum_qty))
  price[ind]=vol_chf_trans[ind]/qty_trans[ind]
  if (length(ind)>0){
    if (ind<length(cum_qty)){
      for (i in (ind+1):length(cum_qty)){
        if (action[i] %in% c('A','B')){
          price[i]=(price[i-1]*(cum_qty[i]-qty_trans[i])/cum_qty[i]+vol_chf_trans[i]/cum_qty[i])
        }
        if (action[i] %in% c('S','C')){
          price[i]=price[i-1]
        }
        if (action[i] %in% c('E','D')){
          price[i]=(price[i-1]*cum_qty[i]-vol_chf_trans[i])/cum_qty[i]
        }
      }
    }
  return(price)
  }else{
    return(price)
  }
}


u.trans[,cum_position:=cumsum(qty_trans*(action %in% c('A','B'))-qty_trans*(action %in% c('S','C')))-
          min(c(0,cumsum(qty_trans*(action %in% c('A','B'))-qty_trans*(action %in% c('S','C'))))),by=c('client','security_key')]
#u.trans[!(cum_position==0 & (action %in% c('D','E')))]
ptm <- proc.time()
asd2=u.trans[,list(sum(action %in% c('A','B','C','S'))),by=c('client','security_key')]
u.trans=u.trans[!(paste0(client,security_key) %in% paste0(asd2$client[asd2$V1==0],asd2$security_key[asd2$V1==0]))]
u.trans[,mod_price:=Fun_price(vol_chf_trans,qty_trans,action),by=c('client','security_key')]
u.trans[action %in% c('A','B','C','S'),mod_SMI:=Fun_price(SMI*qty_trans,qty_trans,action),by=c('client','security_key')]
proc.time() - ptm
u.trans[,':='(symbol=NULL,contractsize_trans=NULL,group_order=NULL,stock_exchange=NULL,isin=NULL,cur=NULL)]

u.trans_mod=u.trans[(action %in% c('C','S')) & date_trans>=as.Date('2010-01-01')] #[(action %in% c('C','S')) & date_trans>=as.Date('2010-01-01')]

u.trans_mod[,quarter:=as.Date(timeLastDayInQuarter(date_trans-91.25))+1]
#u.trans_mod[,new_price:=vol_chf_trans/qty_trans]
#u.trans_mod$new_price[u.trans_mod$action %in% c('A','B')]=NA
#u.trans_mod[,new_price:=na.locf(new_price,fromLast = TRUE,na.rm=FALSE),by=c('client','security_key')]
u.trans_mod[,net_trans:=(vol_chf_trans/(mod_price*qty_trans)-SMI/mod_SMI)*(mod_price*qty_trans)]# >0 delete [,net_trans:=(vol_chf_trans/(mod_price*qty_trans)-SMI/mod_SMI)*(mod_price*qty_trans)] # >0 delete
quarter_net=u.trans_mod[,list(net_trans_sell=sum(net_trans),nr_sel=length(net_trans)),by=c('client','quarter')] # mean to sum

quarter_net2=quarter_net[client %in% as.character(Tab4_lt$client)] %>%.[order(client)]
quarter_net2$client=as.character(quarter_net2$client)
Age2=Age[rownames(Age) %in% as.character(unique(quarter_net2$client)),]
Total_matrix2=Total_matrix[rownames(Total_matrix) %in% as.character(unique(quarter_net2$client)),]

###############################
#### Quarterly performance ####
delta_sec_in=u.trans[action %in% c('A','C'),list(sec_in=sum(vol_chf_trans[action=='A'],na.rm=TRUE),
                                                   sec_out=sum(vol_chf_trans[action=='C'],na.rm=TRUE)),by=c('client','date_trans')]
#store_tab=c()
#for (years in c(2010:2017)){
#  Tab2=get_data(paste0('/home/godinat/Data_31_03_2018/data/Tab2_',years,'.rda')) %>% .[client %in% Tab4_lt$client] %>% .[abs(tot_val)!=0]
#  Tab2[,':='(cash_val=NULL,assets_val=NULL)]
#  Tab2[,quarter:=as.Date(timeFirstDayInQuarter(account_date))]
#  delta_sec_in2=delta_sec_in[year(date_trans)==years]
#  delta_sec_in2$date_trans[!(delta_sec_in2$date_trans %in% unique(Tab2$account_date))]=delta_sec_in2$date_trans[!(delta_sec_in2$date_trans %in% unique(Tab2$account_date))]+1
#  delta_sec_in2$date_trans[!(delta_sec_in2$date_trans %in% unique(Tab2$account_date))]=delta_sec_in2$date_trans[!(delta_sec_in2$date_trans %in% unique(Tab2$account_date))]+1
#  Tab2=merge(Tab2,delta_sec_in2,by.x=c('client','account_date'),by.y=c('client','date_trans'),all.x=TRUE)
#  Tab2[is.na(sec_in),c('sec_in')]=0
#  Tab2[is.na(sec_out),c('sec_out')]=0
#  Tab2=merge(Tab2,SMI_def[,c('date','SMI')],by.x='account_date',by.y='date',all.x=TRUE)
#  List=Tab2[,list(len=length(account_date)),by=c('quarter','client')] %>% .[len==1]
#  Last_tab=Tab2[!(paste0(client,quarter) %in% paste0(List$client,List$quarter))]%>%.[order(account_date)]
#  Last_tab=Last_tab[,list(r_perf=(tail(tot_val,1)-tot_val[1]+sum(cashin[-1])-sum(cashout[-1])+sum(sec_in[-1])-sum(sec_out[-1]))/
#                            (tot_val[1]+sum(cashin[-1])+sum(sec_in[-1])),r_SMI=tail(SMI,1)/SMI[1]-1),by=c('client','quarter')]
#  store_tab=rbind(store_tab,Last_tab)
#}

#quarter_net=u.trans_mod[!duplicated(paste0(client,quarter))]
#quarter_net2=merge(quarter_net,store_tab,by=c('client','quarter'),all.x=TRUE) %>% .[client %in% as.character(Tab4_lt$client)] %>%.[order(client)]
#quarter_net2[is.na(r_perf),c('r_perf')]=0
#quarter_net2[,r_new:=r_perf-r_SMI]
Net_Sell_matrix=dcast(quarter_net2,client~quarter,value.var = 'r_new') %>%.[order(as.numeric(client))]

Net_Sell_matrix=dcast(quarter_net2,client~quarter,value.var = 'net_trans_sell') %>%.[order(as.numeric(client))]
Win_matrix=Net_Sell_matrix[,-1]>0 # put Net_matrix>0
rownames(Win_matrix)=rownames(Age2)
Age_mat=(!is.na(Win_matrix))*Age2
Age_mat[Age_mat==0]=NA


store_sell=data.frame(age=unique(Age[!is.na(Age)])/4)
store_sell_sd=data.frame(age=unique(Age[!is.na(Age)])/4)
store_sell_nr=data.frame(age=unique(Age[!is.na(Age)])/4)
for (t in 1:32){
  x=Age_mat[,t]
  store=data.frame(age=as.numeric(),prob=as.numeric())
  store_sd=data.frame(age=as.numeric(),prob=as.numeric())
  store_nr=data.frame(age=as.numeric(),prob=as.numeric())
  colnames(store)[2]=paste0('prob_',t)
  colnames(store_sd)[2]=paste0('prob_',t)
  colnames(store_nr)[2]=paste0('prob_',t)
  for (i in unique(x[order(x)])%>%.[!is.na(.)]){
    store[i,]=c(i/4,mean(Win_matrix[x==i,t]*(x[x==i]>0),na.rm=TRUE))
    store_sd[i,]=c(i/4,sd(Win_matrix[x==i,t]*(x[x==i]>0),na.rm=TRUE))
    store_nr[i,]=c(i/4,sum(!is.na(Win_matrix[x==i,t]*(x[x==i]>0))))
  }      
  
  
  #store1=store1[-dim(store1)[1],]
  store_sell=merge(store_sell,store,by='age',all=TRUE) 
  store_sell=store_sell[!is.na(store_sell[,1]),]
  store_sell_sd=merge(store_sell_sd,store_sd,by='age',all=TRUE) 
  store_sell_sd=store_sell_sd[!is.na(store_sell_sd[,1]),]
  store_sell_nr=merge(store_sell_nr,store_nr,by='age',all=TRUE) 
  store_sell_nr=store_sell_nr[!is.na(store_sell_nr[,1]),]
  
}

lin=lm(c(unlist(store_sell[,-1]))~rep(c(store_sell[,1]),32)+rep(rep(c(store_sell[,1]^2),32)))

CI_0=100*1.96*sqrt(rowMeans(store_sell_sd[,-1]^2,na.rm=TRUE))/sqrt(rowSums(store_sell_nr[,-1],na.rm=TRUE))
CI_1=100*1.96*sqrt(rowMeans(store_sell_sd[,-1]^2,na.rm=TRUE))/sqrt(rowSums(store_sell_nr[,-1],na.rm=TRUE))


plot(x=store_sell[,1],y=(rowMeans(store_sell[,-1],na.rm=TRUE))*100,ylim=c(45,70),col=1,type='l',
     main='Aggregated successful sell by clients',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[,1],y=(rowMeans(store_sell[,-1],na.rm=TRUE))*100+CI_0,col=2,lty=2)
lines(x=store_sell[,1],y=(rowMeans(store_sell[,-1],na.rm=TRUE))*100-CI_0,col=3,lty=2)
legend(0,70,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=1,lty=c(1,2,2))


par(mfrow=c(2,4))
i=2
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2010 Q1',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,35,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

i=3
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2010 Q2',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,35,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

i=4
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2010 Q3',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,35,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

i=5
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2010 Q4',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,75,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

i=6
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2011 Q1',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,35,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))
i=7
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2011 Q2',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,75,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

i=8
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2011 Q3',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,75,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

i=9
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2011 Q4',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,75,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))



par(mfrow=c(2,4))
i=10
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2012 Q1',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,35,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

i=11
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2012 Q2',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,75,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

i=12
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2012 Q3',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,35,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

i=13
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2012 Q4',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,75,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

i=14
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2013 Q1',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,35,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))
i=15
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2013 Q2',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,75,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

i=16
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2013 Q3',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,35,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

i=17
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2013 Q4',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,35,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))



par(mfrow=c(2,4))
i=18
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2014 Q1',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,35,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

i=19
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2014 Q2',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,35,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

i=20
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2014 Q3',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,35,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

i=21
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2014 Q4',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,35,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

i=22
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2015 Q1',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,35,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))
i=23
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2015 Q2',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,35,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

i=24
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2015 Q3',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,35,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

i=25
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2015 Q4',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,35,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))


par(mfrow=c(2,4))
i=26
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2016 Q1',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,35,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

i=27
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2016 Q2',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,35,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

i=28
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2016 Q3',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,35,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

i=29
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2016 Q4',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,35,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

i=30
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2017 Q1',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,35,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))
i=31
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2017 Q2',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,35,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

i=32
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2017 Q3',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,35,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))

i=33
plot(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100,ylim=c(25,75),col=1,type='l',
     main='Propability of successeful sell 2017 Q4',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100+100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=2,lty=2)
lines(x=store_sell[!is.na(store_sell[,i]),1],y=(store_sell[!is.na(store_sell[,i]),i])*100-100*1.96*(store_sell_sd[!is.na(store_sell[,i]),i])/
        sqrt(store_sell_nr[!is.na(store_sell[,i]),i]),col=3,lty=2)
legend(0,35,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=0.6,lty=c(1,2,2))



CI_0=100*1.96*sqrt(rowMeans(store_sell_sd[,-1]^2,na.rm=TRUE))/sqrt(rowSums(store_sell_nr[,-1],na.rm=TRUE))
CI_1=100*1.96*sqrt(rowMeans(store_sell_sd[,-1]^2,na.rm=TRUE))/sqrt(rowSums(store_sell_nr[,-1],na.rm=TRUE))


plot(x=store_sell[,1],y=(rowMeans(store_sell[,-1],na.rm=TRUE))*100,ylim=c(45,70),col=1,type='l',
     main='Aggregated Prob successeful sell',ylab='Percentage %',xlab='Time in SQ',cex.main=0.9,cex.lab=0.9)
lines(x=store_sell[,1],y=(rowMeans(store_sell[,-1],na.rm=TRUE))*100+CI_0,col=2,lty=2)
lines(x=store_sell[,1],y=(rowMeans(store_sell[,-1],na.rm=TRUE))*100-CI_0,col=3,lty=2)
legend(0,70,c('Prob successeful sell','Upper 95% CI','Lower 95% CI'),col=c(1:3),pch=1,cex=1,lty=c(1,2,2))


p1=ggplot(store_sell_nr[!is.na(store_sell$prob_1),],aes(x=age,y=prob_1))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2010 Q1')
p2=ggplot(store_sell_nr[!is.na(store_sell$prob_2),],aes(x=age,y=prob_2))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2010 Q2')
p3=ggplot(store_sell_nr[!is.na(store_sell$prob_3),],aes(x=age,y=prob_3))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2010 Q3')
p4=ggplot(store_sell_nr[!is.na(store_sell$prob_4),],aes(x=age,y=prob_4))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2010 Q4')
p5=ggplot(store_sell_nr[!is.na(store_sell$prob_5),],aes(x=age,y=prob_5))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2011 Q1')
p6=ggplot(store_sell_nr[!is.na(store_sell$prob_6),],aes(x=age,y=prob_6))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2011 Q2')
p7=ggplot(store_sell_nr[!is.na(store_sell$prob_7),],aes(x=age,y=prob_7))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2011 Q3')
p8=ggplot(store_sell_nr[!is.na(store_sell$prob_8),],aes(x=age,y=prob_8))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2011 Q4')
p9=ggplot(store_sell_nr[!is.na(store_sell$prob_9),],aes(x=age,y=prob_9))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2012 Q1')
p10=ggplot(store_sell_nr[!is.na(store_sell$prob_10),],aes(x=age,y=prob_10))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2012 Q2')
p11=ggplot(store_sell_nr[!is.na(store_sell$prob_11),],aes(x=age,y=prob_11))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2012 Q3')
p12=ggplot(store_sell_nr[!is.na(store_sell$prob_12),],aes(x=age,y=prob_12))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2012 Q4')
grid.arrange(p1, p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, nrow = 3)

p1=ggplot(store_sell_nr[!is.na(store_sell$prob_13),],aes(x=age,y=prob_13))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2013 Q1')
p2=ggplot(store_sell_nr[!is.na(store_sell$prob_14),],aes(x=age,y=prob_14))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2013 Q2')
p3=ggplot(store_sell_nr[!is.na(store_sell$prob_15),],aes(x=age,y=prob_15))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2013 Q3')
p4=ggplot(store_sell_nr[!is.na(store_sell$prob_16),],aes(x=age,y=prob_16))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2013 Q4')
p5=ggplot(store_sell_nr[!is.na(store_sell$prob_17),],aes(x=age,y=prob_17))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2014 Q1')
p6=ggplot(store_sell_nr[!is.na(store_sell$prob_18),],aes(x=age,y=prob_18))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2014 Q2')
p7=ggplot(store_sell_nr[!is.na(store_sell$prob_19),],aes(x=age,y=prob_19))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2014 Q3')
p8=ggplot(store_sell_nr[!is.na(store_sell$prob_20),],aes(x=age,y=prob_20))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2014 Q4')
p9=ggplot(store_sell_nr[!is.na(store_sell$prob_21),],aes(x=age,y=prob_21))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2015 Q1')
p10=ggplot(store_sell_nr[!is.na(store_sell$prob_22),],aes(x=age,y=prob_22))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2015 Q2')
p11=ggplot(store_sell_nr[!is.na(store_sell$prob_23),],aes(x=age,y=prob_23))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2015 Q3')
p12=ggplot(store_sell_nr[!is.na(store_sell$prob_24),],aes(x=age,y=prob_24))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2015 Q4')
grid.arrange(p1, p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, nrow = 3)


p1=ggplot(store_sell_nr[!is.na(store_sell$prob_25),],aes(x=age,y=prob_25))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2016 Q1')
p2=ggplot(store_sell_nr[!is.na(store_sell$prob_26),],aes(x=age,y=prob_26))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2016 Q2')
p3=ggplot(store_sell_nr[!is.na(store_sell$prob_27),],aes(x=age,y=prob_27))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2016 Q3')
p4=ggplot(store_sell_nr[!is.na(store_sell$prob_28),],aes(x=age,y=prob_28))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2016 Q4')
p5=ggplot(store_sell_nr[!is.na(store_sell$prob_29),],aes(x=age,y=prob_29))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2017 Q1')
p6=ggplot(store_sell_nr[!is.na(store_sell$prob_30),],aes(x=age,y=prob_30))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2017 Q2')
p7=ggplot(store_sell_nr[!is.na(store_sell$prob_31),],aes(x=age,y=prob_31))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2017 Q3')
p8=ggplot(store_sell_nr[!is.na(store_sell$prob_32),],aes(x=age,y=prob_32))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2017 Q4')
p9=ggplot(store_sell_nr[!is.na(store_sell$prob_33),],aes(x=age,y=prob_33))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2018 Q1')
p10=ggplot(store_sell_nr[!is.na(store_sell$prob_34),],aes(x=age,y=prob_34))+geom_bar(stat = "identity")+xlab('Age')+ylab('Nr. client')+ggtitle('Nr clients selling 2018 Q2')
grid.arrange(p1, p2,p3,p4,p5,p6,p7,p8,p9,p10, nrow = 3)


Quiter=t(apply(Total_matrix,1,cumsum))
Quiter=Quiter[,-1]

quit_client=rownames(Quiter)[rowSums(Quiter==1)==1]
asd=Quiter[rownames(Quiter) %in% quit_client,]
quit_quarter=colnames(Quiter)[apply(asd==1,1,which)]


asd=quarter_net2[client %in% as.numeric(quit_client)]

asd=quarter_net2[!(client %in% as.numeric(quit_client))]
asd2=asd[,list(positive_quarters=sum(net_trans_sell>=0,na.rm=TRUE),quarter_act=length(net_trans_sell)),by='client']
