library(HighFreq)
library(tidyverse)
library(data.table)
library(plotly)
library(googlesheets4)
library(quantmod)
library(etrader)
library(lubridate)

###parameters##############################################################################################
period=35000
i=0

s1="PINS"
s4="SE"
s5="UPST"
s2="UBER"
s3="MSTR"
# s1="FCX"

### SE, GTLB,U, ROKU,BBIO (HEALTH STOCK),RBLX, RIVN, DKNG, TSLA, UPST,FL,APLS(HEALTH STOCK), GH(HEALTH STOCK), TRUP(HEALTH STOCK)

##Moving average periods
ma=2 #period to measure slope of moving average (ma)
ma_slope=0.000
#a,b,c
#fin targets#####################################################################
mon=50000
a1=0.005   ##REDUCE IF DAY IS BAD/confirm with simulations
floor=-0.0035   #maximium loss per trade
ab_floor=-0.005
time="10:30"

#The Decider#########################################################################################################

while (between(difftime(Sys.time(), paste0(Sys.Date()," 08:00:00"), units = "secs") , 0,period)) {

  # if (paste0(Sys.Date()," 03:45:00")){
  #   write_sheet(stock, ss="1-0YM62HbgjJy5IlISZ9bbbLGxlliRFSfI-rQlYt1zZ8", sheet="Intraday")
  # } else {print("yo")}
  
  # status<-"on"
  
status<-stat(Sys.time())
# print(status)
##varibles##############################################################################################
i<-i+1 #counter
z<-ifelse(i<ma,i,i-ma)
#########################################################################################################

quote<-  etrd_market_quote(
symbols = c(s1,s2,s3,s4),
detailFlag = "ALL",
output = c("df"),
access_tokens = NULL,
etrade_cred = etrade_cred,
sandbox = FALSE
)
quote$All.bid<-as.numeric(quote$All.bid)
quote$All.ask<-as.numeric(quote$All.ask)
stock_out<- data.frame(Count=i,Date=Sys.time(),Symbol=s1,
                     Volume=as.numeric(quote$All.totalVolume[1]),Price=as.numeric(quote$All.lastTrade[1]),
                     avg=0,diff=0,diff_smooth=0,
                     m=0,PP=0,PP1=0,BC=0,SC=0,avg_8=0,
                     avg_13=0,Decide="",
                     Buycheck="0",Sellcheck="0",SellCount=0,
                     Time=format(round_date(as.POSIXct(Sys.time()),unit="minute"), format = "%H:%M"),Size=0,
                     stringsAsFactors = FALSE)

######################################################################
##get existing holdings
hodl<-
# "x" 
try(etrd_account_portfolio(account = account,
                           access_tokens = access_tokens,
                           etrade_cred = etrade_cred
                           ,sandbox = FALSE)
    , TRUE)

hodl_out<-try(dplyr::bind_rows(lapply(hodl$AccountPortfolio[[1]]$Position,
                        function(x) {data.frame(x)})) , TRUE)

if(class(hodl_out)=="try-error") {
  df1<-out
  # stock$PP<-0
  df2<-out
  # stock2$PP<-0
  df3<-out
  df4<-out  } else {
    if(nrow(hodl_out %>% filter (symbolDescription==s1))==0){
  df1<-out
  # stock$PP<-0
  } else{
  df1<-data.frame(try(hodl_out %>% filter (symbolDescription==s1),TRUE))
  }

  if(nrow(hodl_out %>% filter (symbolDescription==s2))==0){
    df2<-out
    # stock2$PP<-0
  } else {
    df2<-data.frame(try(hodl_out %>% filter (symbolDescription==s2),TRUE))
  }
    
  if(nrow(hodl_out %>% filter (symbolDescription==s3))==0){
    df3<-out
    # stock3$PP<-0
  } else {
    df3<-data.frame(try(hodl_out %>% filter (symbolDescription==s3),TRUE))
  }  
    
  if(nrow(hodl_out %>% filter (symbolDescription==s4))==0){
    df4<-out
    # stock4$PP<-0
  } else {
    df4<-data.frame(try(hodl_out %>% filter (symbolDescription==s4),TRUE))
  }  

}


################################################################################################################################################################
stock$Price<-as.numeric(stock$Price)
stock<-unique(rbind(stock,stock_out))
stock<-as.data.table(stock)
stock$avg<-as.numeric(stock$avg)

##smoothing MA version 1
if (nrow(stock)==1){stock$avg<-0  } else { stock$avg<-round(ZLEMA(stock$Price, n = min(i,30), ratio = NULL),4)}
if (nrow(stock)==1){stock$avg_8<-0  } else { stock$avg_8<-round(ZLEMA(stock$Price, n = min(i,48), ratio = NULL),4)}
if (nrow(stock)==1){stock$avg_13<-0  } else {stock$avg_13<-round(ZLEMA(stock$Price, n = min(i,78), ratio = NULL),4)}

stock[is.na(stock$avg)]$avg<-0
stock[is.na(stock$avg_8)]$avg_8<-0
stock[is.na(stock$avg_13)]$avg_13<-0
  

stock$diff[i]<-round(stock$Price[i]-stock$avg[i],2)

# if (i>100) {fit <- with(stock, ksmooth(Count, diff, kernel = "box", bandwidth = 48))
# } else {
# fit <- with(stock, ksmooth(Count, diff, kernel = "box", bandwidth = 48,n.points = min(100L, nrow(stock))))
# }
# stock$diff_smooth[i]<-round(fit$y[i],4)

##smoothing MA version 2
# if (nrow(stock)==1){stock$diff_smooth<-0   } else { stock$diff_smooth<-round(roll_vwap(ohlc=stock,close=stock$diff, look_back=min(nrow(stock),48)),4)}
if(i>400){
  stoch_out<-data.frame(stoch(stock[,c("Price")], n = 60,nFastD = 36,nSlowD = 36))
  stoch_out[is.na(stoch_out)]<-0
  stock$diff_smooth[i]<-round(stoch_out[i,2],4)
  print(stoch_out[i,2])
  } else {stock$diff_smooth[i]<-0}
################################################################################################################################################################
##getter
if(stock[i]$Count==1) {
stock[i]$Buycheck<-"NO"
stock[i]$Sellcheck<-"NO"
stock$m<-0
stock$Size[i]<-stock$Volume[i]
} else {
  x<-ifelse(i<ma,0,lm(avg_13~Count,data=stock[c(z:i),]))
  stock$m[i]<-round(ifelse(i<ma,0,x[[1]][["Count"]]),4)
  stock$Size[i]<-stock$Volume[i]-stock$Volume[i-1]
  # stock[i]$Buycheck<-ifelse(stock$avg[i]>stock$avg[i-1] & stock$avg_8[i]>stock$avg_8[i-1] & stock$avg_13[i]>stock$avg_13[i-1] & stock$m[i]>=0.000,"YES","NO")
  if(i>400){
    # stock[i]$Buycheck<-ifelse(between(stoch_out$fastD[i],0.15,0.5) & stoch_out$fastD[i]>=stoch_out$slowD[i] & stock$m[i]>0.009,"YES","NO")
    stock[i]$Buycheck<-ifelse(between(stoch_out$fastD[i],0.10,0.25) & stoch_out$fastD[i]>=stoch_out$slowD[i] & stock$m[i]>ma_slope,"YES","NO")
    stock[i]$Sellcheck<-ifelse(between(stoch_out$fastD[i],0.65,0.75) & 
                                 stoch_out$fastD[i]<=stoch_out$slowD[i] ,"YES","NO")
    
  } else {
  stock[i]$Buycheck<-"NO"
  stock[i]$Sellcheck<-"NO"
  }

  stock$BC[i]<-ifelse(stock$Buycheck[i]=="YES"  ,stock$BC[i-1]+1,0)
  stock$SC[i]<-ifelse(stock$Sellcheck[i]=="YES",stock$SC[i-1]+1,0)
  
  ##buy
  stock$Decide[i]<-ifelse(stock$BC[i]==2 & stock$Decide[i-1]!="hold","buy","")
  stock$Decide[i]<-ifelse(stock$Decide[i-1]=="buy" || stock$Decide[i-1]=="hold","hold", stock$Decide[i])
  
  ##sell
  stock$Decide[i]<-ifelse(stock$SC[i]==10 & (stock$Decide[i-1]=="buy" || stock$Decide[i-1]=="hold"),"sell",stock$Decide[i])   
  
  
  stock$PP[i]<-ifelse(df1$pricePaid==0,0,round(stock$Price[i]/df1$pricePaid-1,4))
  stock$PP1[i]<-ifelse(df1$pricePaid==0,0,round(stock$Price[i]/df1$pricePaid-1,4))
  stock$PP[i]<-stock$PP1[i]

  

  stock$Decide[i]<-ifelse(stock$PP[i]>0.000, seller(stock$diff_smooth[i],stock$PP1[i],stock$PP,stock$Decide[i],a1),  stock$Decide[i])
  stock$Decide[i]<-ifelse(stock$PP[i]<= floor & df1$symbolDescription==s1,"sell",stock$Decide[i])
  
  
  stock$Decide[i]<-ifelse(stock$Decide[i]=="hold" & df1$symbolDescription!=s1,"",stock$Decide[i])
  stock$Decide[i]<-ifelse(stock$Decide[i-1]=="sell" & stock$Decide[i]=="sell","",stock$Decide[i])



}

################################################################################################################################################################
print(stock)
print(max(stock$PP))


xx<-as.numeric(min(quote$All.bid[1]+.02,quote$All.ask[1]))
yy<-as.numeric(min(quote$All.bid[1]+.02,quote$All.ask[1]))
#  #####THE DECIDER ################################################################################################ 
# if (stock$Decide[i]=="buy" & df1$symbolDescription!=s1 & stock$PP[i]==0 & Sys.time()>paste0(Sys.Date(),time)  & status=="on" ){
# etrd_place_eq_order(
#   account = account,symbol = stock$Symbol[i],
#   quantity=round(mon/stock$Price[i],0),
#   orderAction="BUY"#, BUY_TO_COVER,    SELL_SHORT  BUY,
#   ,priceType="LIMIT"
#   # ,priceType="MARKET"
#   #"MARKET", LIMIT, STOP, STOP_LIMIT, TRAILING_STOP_CNST,
#   #TRAILING_STOP_PRCT, MARKET_ON_OPEN, MARKET_ON_CLOSE, LIMIT_ON_OPEN,
#   #LIMIT_ON_CLOSE
#   ,stopPrice = "",
#   limitPrice = xx,
#   stopLimitPrice = "",
#   quantityType = "quantity",
#   orderTerm = "good_for_day",
#   marketSession = "regular",
#   allOrNone = "true",
#   previewOrder = "none",
#   access_tokens = access_tokens,
#   etrade_cred = etrade_cred
#   # ,sandbox = TRUE
# )
#  }

if ((stock$Decide[i]=="sell" | stock$PP[i]<= ab_floor) & df1$symbolDescription==s1 ){
stock$PP<-0
# stock$BC<-0
stock$diff_smooth<-0
etrd_place_eq_order(
  account = account,symbol = stock$Symbol[i],
  quantity=hodl_out$quantity[hodl_out$symbolDescription==s1],
  orderAction="SELL"#, BUY_TO_COVER,    SELL_SHORT  BUY,
   # ,priceType="LIMIT"
  ,priceType="MARKET"

  #"MARKET", LIMIT, STOP, STOP_LIMIT, TRAILING_STOP_CNST,
  #TRAILING_STOP_PRCT, MARKET_ON_OPEN, MARKET_ON_CLOSE, LIMIT_ON_OPEN,
  #LIMIT_ON_CLOSE
  ,stopPrice = "",
  limitPrice = yy,
  stopLimitPrice = "",
  quantityType = "quantity",
  orderTerm = "good_for_day",
  marketSession = "regular",
  allOrNone = "true",
  previewOrder = "none",
  access_tokens = access_tokens,
  etrade_cred = etrade_cred
  # ,sandbox = TRUE
)
}
####THE DECIDER ENDS ################################################################################################ 
write.csv(stock[stock$Date!='2023-05-04 16:00:04',],paste0("technical_",Sys.Date(),".csv"),row.names=FALSE)
write.csv(stock[stock$Date!='2023-05-04 16:00:04',],paste0("technical.csv"),row.names=FALSE)
# write.csv(stock %>% group_by(Symbol,Time) %>% summarize(Volume=sum(Size),Price=mean(Price)),paste0("Volume",".csv"),row.names=FALSE)  
# Sys.sleep(5)
# }
########################################################################################################################################################


stock2_out<- data.frame(Count=i,Date=Sys.time(),Symbol=s2,
                       Volume=as.numeric(quote$All.totalVolume[2]),Price=as.numeric(quote$All.lastTrade[2]),
                       avg=0,diff=0,diff_smooth=0,
                       m=0,PP=0,PP1=0,BC=0,SC=0,avg_8=0,
                       avg_13=0,Decide="",
                       Buycheck="0",Sellcheck="0",SellCount=0,
                       Time=format(round_date(as.POSIXct(Sys.time()),unit="minute"), format = "%H:%M"),Size=0,
                       stringsAsFactors = FALSE)

################################################################################################################################################################
stock2$Price<-as.numeric(stock2$Price)
stock2<-unique(rbind(stock2,stock2_out))
stock2<-as.data.table(stock2)
stock2$avg<-as.numeric(stock2$avg)

##smoothing MA version 1
if (nrow(stock2)==1){stock2$avg<-0  } else { stock2$avg<-round(ZLEMA(stock2$Price, n = min(i,30), ratio = NULL),4)}
if (nrow(stock2)==1){stock2$avg_8<-0  } else { stock2$avg_8<-round(ZLEMA(stock2$Price, n = min(i,48), ratio = NULL),4)}
if (nrow(stock2)==1){stock2$avg_13<-0  } else {stock2$avg_13<-round(ZLEMA(stock2$Price, n = min(i,78), ratio = NULL),4)}

stock2[is.na(stock2$avg)]$avg<-0
stock2[is.na(stock2$avg_8)]$avg_8<-0
stock2[is.na(stock2$avg_13)]$avg_13<-0


stock2$diff[i]<-round(stock2$Price[i]-stock2$avg[i],2)

# if (i>100) {fit <- with(stock2, ksmooth(Count, diff, kernel = "box", bandwidth = 48))
# } else {
#   fit <- with(stock2, ksmooth(Count, diff, kernel = "box", bandwidth = 48,n.points = min(100L, nrow(stock2))))
# }
# stock2$diff_smooth[i]<-round(fit$y[i],4)

##smoothing MA version 2
# if (nrow(stock2)==1){stock2$diff_smooth<-0   } else { stock2$diff_smooth<-round(roll_vwap(ohlc=stock2,close=stock2$diff, look_back=min(nrow(stock2),48)),4)}
if(i>400){
  stoch2_out<-data.frame(stoch(stock2[,c("Price")], n = 60,nFastD = 36,nSlowD = 36))
  stoch2_out[is.na(stoch2_out)]<-0
  stock2$diff_smooth[i]<-round(stoch2_out[i,2],4)
  print(stoch_out[i,])
  print(stoch2_out[i,])
} else { stock2$diff_smooth[i]<-0}

################################################################################################################################################################
##getter
if(stock2[i]$Count==1) {
  stock2[i]$Buycheck<-"NO"
  stock2[i]$Sellcheck<-"NO"
  stock2$m<-0
  stock2$Size[i]<-stock2$Volume[i]
} else {
  x<-ifelse(i<ma,0,lm(avg_13~Count,data=stock2[c(z:i),]))
  stock2$m[i]<-round(ifelse(i<ma,0,x[[1]][["Count"]]),4)
  stock2$Size[i]<-stock2$Volume[i]-stock2$Volume[i-1]
  
  if(i>400){
    # stock2[i]$Buycheck<-ifelse(between(stoch2_out$fastD[i],0.15,1.0) & stoch2_out$fastD[i]>=stoch2_out$slowD[i],"YES","NO")
    stock2[i]$Buycheck<-ifelse(between(stoch2_out$fastD[i],0.10,0.25) & stoch2_out$fastD[i]>=stoch2_out$slowD[i] & stock2$m[i]>ma_slope,"YES","NO")
    stock2[i]$Sellcheck<-ifelse(between(stoch2_out$fastD[i],0.65,0.75) & 
                                  stoch2_out$fastD[i]<=stoch2_out$slowD[i] ,"YES","NO")
    
  } else {
    stock2[i]$Buycheck<-"NO"
    stock2[i]$Sellcheck<-"NO"
  }
  
  
  stock2$BC[i]<-ifelse(stock2$Buycheck[i]=="YES"  ,stock2$BC[i-1]+1,0)
  stock2$SC[i]<-ifelse(stock2$Sellcheck[i]=="YES",stock2$SC[i-1]+1,0)
  
  ##buy
  stock2$Decide[i]<-ifelse(stock2$BC[i]==2 & stock2$Decide[i-1]!="hold","buy","")
  stock2$Decide[i]<-ifelse(stock2$Decide[i-1]=="buy" || stock2$Decide[i-1]=="hold","hold", stock2$Decide[i])
  
  ##sell
  stock2$Decide[i]<-ifelse(stock2$SC[i]==12 & (stock2$Decide[i-1]=="buy" || stock2$Decide[i-1]=="hold"),"sell",stock2$Decide[i])   
  
  
  stock2$PP[i]<-ifelse(df2$pricePaid==0,0,round(stock2$Price[i]/df2$pricePaid-1,4))
  stock2$PP1[i]<-ifelse(df2$pricePaid==0,0,round(stock2$Price[i]/df2$pricePaid-1,4))
  stock2$PP[i]<-stock2$PP1[i]
  
  stock2$Decide[i]<-ifelse(stock2$PP[i]>0.000,   seller(stock2$diff_smooth[i],stock2$PP1[i],stock2$PP,stock2$Decide[i],a1),stock2$Decide[i]) 
  # stock2$Decide[i]<-ifelse(stock2$PP[i]>0.000,   seller2(stock2$PP1[i],stock2$PP,stock2$Decide[i],a1),stock2$Decide[i]) 
  stock2$Decide[i]<-ifelse(stock2$PP[i]<= floor & df2$symbolDescription==s2,"sell",stock2$Decide[i])

  stock2$Decide[i]<-ifelse(stock2$Decide[i]=="hold" & df2$symbolDescription!=s2,"",stock2$Decide[i])
  stock2$Decide[i]<-ifelse(stock2$Decide[i-1]=="sell" & stock2$Decide[i]=="sell","",stock2$Decide[i])
  
}
################################################################################################################################################################

xx2<-as.numeric(min(quote$All.bid[2]+.05,quote$All.ask[2]))
yy2<-as.numeric(min(quote$All.bid[2]+.05,quote$All.ask[2]))
#  #####THE DECIDER ################################################################################################ 
# if (stock2$Decide[i]=="buy" & df2$symbolDescription!=s2 & stock2$PP[i]==0 & Sys.time()>paste0(Sys.Date(),time) & status=="on"){
#   etrd_place_eq_order(
#     account = account,symbol = stock2$Symbol[i],
#     quantity=round(mon/stock2$Price[i],0),
#     orderAction="BUY"#, BUY_TO_COVER,    SELL_SHORT  BUY,
#      ,priceType="LIMIT"
#      # ,priceType="MARKET"
#     #"MARKET", LIMIT, STOP, STOP_LIMIT, TRAILING_STOP_CNST,
#     #TRAILING_STOP_PRCT, MARKET_ON_OPEN, MARKET_ON_CLOSE, LIMIT_ON_OPEN,
#     #LIMIT_ON_CLOSE
#     ,stopPrice = "",
#     limitPrice = xx2,
#     stopLimitPrice = "",
#     quantityType = "quantity",
#     orderTerm = "good_for_day",
#     marketSession = "regular",
#     allOrNone = "true",
#     previewOrder = "none",
#     access_tokens = access_tokens,
#     etrade_cred = etrade_cred
#     # ,sandbox = TRUE
#   )
# }

if ((stock2$Decide[i]=="sell" | stock2$PP[i]<= ab_floor) & df2$symbolDescription==s2){
  stock2$PP<-0
  # stock2$BC<-0
  stock2$diff_smooth<-0
  etrd_place_eq_order(
    account = account,symbol = stock2$Symbol[i],
    quantity=hodl_out$quantity[hodl_out$symbolDescription==s2],
    orderAction="SELL"#, BUY_TO_COVER,    SELL_SHORT  BUY,
     # ,priceType="LIMIT"
     ,priceType="MARKET"

    #"MARKET", LIMIT, STOP, STOP_LIMIT, TRAILING_STOP_CNST,
    #TRAILING_STOP_PRCT, MARKET_ON_OPEN, MARKET_ON_CLOSE, LIMIT_ON_OPEN,
    #LIMIT_ON_CLOSE
    ,stopPrice = "",
    limitPrice = yy2,
    stopLimitPrice = "",
    quantityType = "quantity",
    orderTerm = "good_for_day",
    marketSession = "regular",
    allOrNone = "true",
    previewOrder = "none",
    access_tokens = access_tokens,
    etrade_cred = etrade_cred
    # ,sandbox = TRUE
  )
}

write.csv(stock2[stock2$Date!='2023-05-04 16:00:04',],paste0("technical2_",Sys.Date(),".csv"),row.names=FALSE)
write.csv(stock2[stock2$Date!='2023-05-04 16:00:04',],paste0("technical2.csv"),row.names=FALSE)
# write.csv(stock2 %>% group_by(Symbol,Time) %>% summarize(Volume=sum(Size),Price=mean(Price)),paste0("Volume2",".csv"),row.names=FALSE)  
# Sys.sleep(5)
# }
####THE DECIDER ENDS ################################################################################################ 

stock3_out<- data.frame(Count=i,Date=Sys.time(),Symbol=s3,
                        Volume=as.numeric(quote$All.totalVolume[3]),Price=as.numeric(quote$All.lastTrade[3]),
                        avg=0,diff=0,diff_smooth=0,
                        m=0,PP=0,PP1=0,BC=0,SC=0,avg_8=0,
                        avg_13=0,Decide="",
                        Buycheck="0",Sellcheck="0",SellCount=0,
                        Time=format(round_date(as.POSIXct(Sys.time()),unit="minute"), format = "%H:%M"),Size=0,
                        stringsAsFactors = FALSE)


################################################################################################################################################################
stock3$Price<-as.numeric(stock3$Price)
stock3<-unique(rbind(stock3,stock3_out))
stock3<-as.data.table(stock3)
stock3$avg<-as.numeric(stock3$avg)

##smoothing MA version 1
if (nrow(stock3)==1){stock3$avg<-0  } else { stock3$avg<-round(ZLEMA(stock3$Price, n = min(i,30), ratio = NULL),4)}
if (nrow(stock3)==1){stock3$avg_8<-0  } else { stock3$avg_8<-round(ZLEMA(stock3$Price, n = min(i,48), ratio = NULL),4)}
if (nrow(stock3)==1){stock3$avg_13<-0  } else {stock3$avg_13<-round(ZLEMA(stock3$Price, n = min(i,78), ratio = NULL),4)}

stock3[is.na(stock3$avg)]$avg<-0
stock3[is.na(stock3$avg_8)]$avg_8<-0
stock3[is.na(stock3$avg_13)]$avg_13<-0


stock3$diff[i]<-round(stock3$Price[i]-stock3$avg[i],2)

# if (i>100) {fit <- with(stock3, ksmooth(Count, diff, kernel = "box", bandwidth = 48))
# } else {
#   fit <- with(stock3, ksmooth(Count, diff, kernel = "box", bandwidth = 48,n.points = min(100L, nrow(stock3))))
# }
# stock3$diff_smooth[i]<-round(fit$y[i],4)
# ##smoothing MA version 2
# if (nrow(stock3)==1){stock3$diff_smooth<-0   } else { stock3$diff_smooth<-round(roll_vwap(ohlc=stock3,close=stock3$diff, look_back=min(nrow(stock3),48)),4)}
if(i>400){
  stoch3_out<-data.frame(stoch(stock3[,c("Price")], n = 60,nFastD = 36,nSlowD = 36))
  stoch3_out[is.na(stoch3_out)]<-0
  stock3$diff_smooth[i]<-round(stoch3_out[i,2],4)
  print(stoch3_out[i,])
}

################################################################################################################################################################
##getter
if(stock3[i]$Count==1) {
  stock3[i]$Buycheck<-"NO"
  stock3[i]$Sellcheck<-"NO"
  stock3$m<-0
  stock3$Size[i]<-stock3$Volume[i]
} else {
  x<-ifelse(i<ma,0,lm(avg_13~Count,data=stock3[c(z:i),]))
  stock3$m[i]<-round(ifelse(i<ma,0,x[[1]][["Count"]]),4)
  stock3$Size[i]<-stock3$Volume[i]-stock3$Volume[i-1]
  
  if(i>400){
    stock3[i]$Buycheck<-ifelse(between(stoch3_out$fastD[i],0.10,0.25) & stoch3_out$fastD[i]>=stoch3_out$slowD[i] & stock3$m[i]>ma_slope,"YES","NO")
    stock3[i]$Sellcheck<-ifelse(between(stoch3_out$fastD[i],0.65,0.75) & 
                                  stoch3_out$fastD[i]<=stoch3_out$slowD[i] ,"YES","NO")
    
  } else {
    stock3[i]$Buycheck<-"NO"
    stock3[i]$Sellcheck<-"NO"
  }
  
  
  stock3$BC[i]<-ifelse(stock3$Buycheck[i]=="YES"  ,stock3$BC[i-1]+1,0)
  stock3$SC[i]<-ifelse(stock3$Sellcheck[i]=="YES",stock3$SC[i-1]+1,0)
  
  ##buy
  stock3$Decide[i]<-ifelse(stock3$BC[i]==3 & stock3$Decide[i-1]!="hold","buy","")
  stock3$Decide[i]<-ifelse(stock3$Decide[i-1]=="buy" || stock3$Decide[i-1]=="hold","hold", stock3$Decide[i])
  
  ##sell
  stock3$Decide[i]<-ifelse(stock3$SC[i]==10 & (stock3$Decide[i-1]=="buy" || stock3$Decide[i-1]=="hold"),"sell",stock3$Decide[i])   
  
  
  stock3$PP[i]<-ifelse(df3$pricePaid==0,0,round(stock3$Price[i]/df3$pricePaid-1,4))
  stock3$PP1[i]<-ifelse(df3$pricePaid==0,0,round(stock3$Price[i]/df3$pricePaid-1,4))
  stock3$PP[i]<-stock3$PP1[i]
  
  stock3$Decide[i]<-ifelse(stock3$PP[i]>0.000,   seller(stock3$diff_smooth[i],stock3$PP1[i],stock3$PP,stock3$Decide[i],a1),stock3$Decide[i]) 
  stock3$Decide[i]<-ifelse(stock3$PP[i]<= floor & df3$symbolDescription==s3,"sell",stock3$Decide[i])

  
  stock3$Decide[i]<-ifelse(stock3$Decide[i]=="hold" & df3$symbolDescription!=s3,"",stock3$Decide[i])
  stock3$Decide[i]<-ifelse(stock3$Decide[i-1]=="sell" & stock3$Decide[i]=="sell","",stock3$Decide[i])
  
}
################################################################################################################################################################

xx3<-as.numeric(min(quote$All.bid[3]+.05,quote$All.ask[3]))
yy3<-as.numeric(min(quote$All.bid[3]+.05,quote$All.ask[3]))
######THE DECIDER ################################################################################################ 
# if (stock3$Decide[i]=="buy" & df3$symbolDescription!=s3 & stock3$PP[i]==0 & Sys.time()>paste0(Sys.Date(),time ) & status=="on") {
#   etrd_place_eq_order(
#     account = account,symbol = stock3$Symbol[i],
#     quantity=round(mon/stock3$Price[i],0),
#     orderAction="BUY"#, BUY_TO_COVER,    SELL_SHORT  BUY,
#     # ,priceType="LIMIT"
#      ,priceType="MARKET"
#     #"MARKET", LIMIT, STOP, STOP_LIMIT, TRAILING_STOP_CNST,
#     #TRAILING_STOP_PRCT, MARKET_ON_OPEN, MARKET_ON_CLOSE, LIMIT_ON_OPEN,
#     #LIMIT_ON_CLOSE
#     ,stopPrice = "",
#     limitPrice = xx3,
#     stopLimitPrice = "",
#     quantityType = "quantity",
#     orderTerm = "good_for_day",
#     marketSession = "regular",
#     allOrNone = "true",
#     previewOrder = "none",
#     access_tokens = access_tokens,
#     etrade_cred = etrade_cred
#     # ,sandbox = TRUE
#   )
# }

if ((stock3$Decide[i]=="sell" | stock3$PP[i]<= ab_floor)  & df3$symbolDescription==s3){
  stock3$PP<-0
  stock3$BC<-0
  stock3$diff_smooth<-0
  etrd_place_eq_order(
    account = account,symbol = stock3$Symbol[i],
    quantity=hodl_out$quantity[hodl_out$symbolDescription==s3],
    orderAction="SELL"#, BUY_TO_COVER,    SELL_SHORT  BUY,
    #,priceType="LIMIT"
     ,priceType="MARKET"

    #"MARKET", LIMIT, STOP, STOP_LIMIT, TRAILING_STOP_CNST,
    #TRAILING_STOP_PRCT, MARKET_ON_OPEN, MARKET_ON_CLOSE, LIMIT_ON_OPEN,
    #LIMIT_ON_CLOSE
    ,stopPrice = "",
    limitPrice = yy3,
    stopLimitPrice = "",
    quantityType = "quantity",
    orderTerm = "good_for_day",
    marketSession = "regular",
    allOrNone = "true",
    previewOrder = "none",
    access_tokens = access_tokens,
    etrade_cred = etrade_cred
    # ,sandbox = TRUE
  )
}
# ####THE DECIDER ENDS ################################################################################################ 
write.csv(stock3[stock3$Date!='2023-05-04 16:00:04',],paste0("technical3_",Sys.Date(),".csv"),row.names=FALSE)
write.csv(stock3[stock3$Date!='2023-05-04 16:00:04',],paste0("technical3",".csv"),row.names=FALSE)
# Sys.sleep(5)
# }

stock4_out<- data.frame(Count=i,Date=Sys.time(),Symbol=s4,
                        Volume=as.numeric(quote$All.totalVolume[4]),Price=as.numeric(quote$All.lastTrade[4]),
                        avg=0,diff=0,diff_smooth=0,
                        m=0,PP=0,PP1=0,BC=0,SC=0,avg_8=0,
                        avg_13=0,Decide="",
                        Buycheck="0",Sellcheck="0",SellCount=0,
                        Time=format(round_date(as.POSIXct(Sys.time()),unit="minute"), format = "%H:%M"),Size=0,
                        stringsAsFactors = FALSE)


################################################################################################################################################################
stock4$Price<-as.numeric(stock4$Price)
stock4<-unique(rbind(stock4,stock4_out))
stock4<-as.data.table(stock4)
stock4$avg<-as.numeric(stock4$avg)

##smoothing MA version 1
if (nrow(stock4)==1){stock4$avg<-0  } else { stock4$avg<-round(ZLEMA(stock4$Price, n = min(i,30), ratio = NULL),4)}
if (nrow(stock4)==1){stock4$avg_8<-0  } else { stock4$avg_8<-round(ZLEMA(stock4$Price, n = min(i,48), ratio = NULL),4)}
if (nrow(stock4)==1){stock4$avg_13<-0  } else {stock4$avg_13<-round(ZLEMA(stock4$Price, n = min(i,78), ratio = NULL),4)}

stock4[is.na(stock4$avg)]$avg<-0
stock4[is.na(stock4$avg_8)]$avg_8<-0
stock4[is.na(stock4$avg_13)]$avg_13<-0


stock4$diff[i]<-round(stock4$Price[i]-stock4$avg[i],2)

# if (i>100) {fit <- with(stock4, ksmooth(Count, diff, kernel = "box", bandwidth = 48))
# } else {
#   fit <- with(stock4, ksmooth(Count, diff, kernel = "box", bandwidth = 48,n.points = min(100L, nrow(stock4))))
# }
# stock4$diff_smooth[i]<-round(fit$y[i],4)
# ##smoothing MA version 2
# if (nrow(stock4)==1){stock4$diff_smooth<-0   } else { stock4$diff_smooth<-round(roll_vwap(ohlc=stock4,close=stock4$diff, look_back=min(nrow(stock4),48)),4)}
if(i>400){
  stoch4_out<-data.frame(stoch(stock4[,c("Price")], n = 60,nFastD = 36,nSlowD = 36))
  stoch4_out[is.na(stoch4_out)]<-0
  stock4$diff_smooth[i]<-round(stoch4_out[i,2],4)
  print(stoch4_out[i,])
}

################################################################################################################################################################
##getter
if(stock4[i]$Count==1) {
  stock4[i]$Buycheck<-"NO"
  stock4[i]$Sellcheck<-"NO"
  stock4$m<-0
  stock4$Size[i]<-stock4$Volume[i]
} else {
  x<-ifelse(i<ma,0,lm(avg_13~Count,data=stock4[c(z:i),]))
  stock4$m[i]<-round(ifelse(i<ma,0,x[[1]][["Count"]]),4)
  stock4$Size[i]<-stock4$Volume[i]-stock4$Volume[i-1]
  
  if(i>400){
    stock4[i]$Buycheck<-ifelse(between(stoch4_out$fastD[i],0.10,0.25) & stoch4_out$fastD[i]>=stoch4_out$slowD[i] & stock4$m[i]>ma_slope,"YES","NO")
    stock4[i]$Sellcheck<-ifelse(between(stoch4_out$fastD[i],0.65,0.75) & 
                                  stoch4_out$fastD[i]<=stoch4_out$slowD[i] ,"YES","NO")
    
  } else {
    stock4[i]$Buycheck<-"NO"
    stock4[i]$Sellcheck<-"NO"
  }
  
  
  stock4$BC[i]<-ifelse(stock4$Buycheck[i]=="YES"  ,stock4$BC[i-1]+1,0)
  stock4$SC[i]<-ifelse(stock4$Sellcheck[i]=="YES",stock4$SC[i-1]+1,0)
  
  ##buy
  stock4$Decide[i]<-ifelse(stock4$BC[i]==3 & stock4$Decide[i-1]!="hold","buy","")
  stock4$Decide[i]<-ifelse(stock4$Decide[i-1]=="buy" || stock4$Decide[i-1]=="hold","hold", stock4$Decide[i])
  
  ##sell
  stock4$Decide[i]<-ifelse(stock4$SC[i]==10 & (stock4$Decide[i-1]=="buy" || stock4$Decide[i-1]=="hold"),"sell",stock4$Decide[i])   
  
  
  stock4$PP[i]<-ifelse(df4$pricePaid==0,0,round(stock4$Price[i]/df4$pricePaid-1,4))
  stock4$PP1[i]<-ifelse(df4$pricePaid==0,0,round(stock4$Price[i]/df4$pricePaid-1,4))
  stock4$PP[i]<-stock4$PP1[i]
  
  stock4$Decide[i]<-ifelse(stock4$PP[i]>0.000,   seller(stock4$diff_smooth[i],stock4$PP1[i],stock4$PP,stock4$Decide[i],a1),stock4$Decide[i]) 
  stock4$Decide[i]<-ifelse(stock4$PP[i]<= floor & df4$symbolDescription==s4,"sell",stock4$Decide[i])

  
  stock4$Decide[i]<-ifelse(stock4$Decide[i]=="hold" & df4$symbolDescription!=s4,"",stock4$Decide[i])
  stock4$Decide[i]<-ifelse(stock4$Decide[i-1]=="sell" & stock4$Decide[i]=="sell","",stock4$Decide[i])
  
}
# ################################################################################################################################################################
xx4<-as.numeric(min(quote$All.bid[4]+.05,quote$All.ask[4]))
yy4<-as.numeric(min(quote$All.bid[4]+.05,quote$All.ask[4]))
##########THE DECIDER ################################################################################################ 
# if (stock4$Decide[i]=="buy" & df4$symbolDescription!=s4 & stock4$PP[i]==0 & Sys.time()>paste0(Sys.Date(),time) & status=="on" ){
#   etrd_place_eq_order(
#     account = account,symbol = stock4$Symbol[i],
#     quantity=round(mon/stock4$Price[I],0),
#     orderAction="BUY"#, BUY_TO_COVER,    SELL_SHORT  BUY,
#      # ,priceType="LIMIT"
#      ,priceType="MARKET"
#     #"MARKET", LIMIT, STOP, STOP_LIMIT, TRAILING_STOP_CNST,
#     #TRAILING_STOP_PRCT, MARKET_ON_OPEN, MARKET_ON_CLOSE, LIMIT_ON_OPEN,
#     #LIMIT_ON_CLOSE
#     ,stopPrice = "",
#     limitPrice = xx4,
#     stopLimitPrice = "",
#     quantityType = "quantity",
#     orderTerm = "good_for_day",
#     marketSession = "regular",
#     allOrNone = "true",
#     previewOrder = "none",
#     access_tokens = access_tokens,
#     etrade_cred = etrade_cred
#     # ,sandbox = TRUE
#   )
# }

if ((stock4$Decide[i]=="sell" | stock4$PP[i]<= ab_floor)  & df4$symbolDescription==s4){
  stock4$PP<-0
  stock4$BC<-0
  stock4$diff_smooth<-0
  etrd_place_eq_order(
    account = account,symbol = stock4$Symbol[i],
    quantity=hodl_out$quantity[hodl_out$symbolDescription==s4],
    orderAction="SELL"#, BUY_TO_COVER,    SELL_SHORT  BUY,
     # ,priceType="LIMIT"
    ,priceType="MARKET"

    #"MARKET", LIMIT, STOP, STOP_LIMIT, TRAILING_STOP_CNST,
    #TRAILING_STOP_PRCT, MARKET_ON_OPEN, MARKET_ON_CLOSE, LIMIT_ON_OPEN,
    #LIMIT_ON_CLOSE
    ,stopPrice = "",
    limitPrice = yy4,
    stopLimitPrice = "",
    quantityType = "quantity",
    orderTerm = "good_for_day",
    marketSession = "regular",
    allOrNone = "true",
    previewOrder = "none",
    access_tokens = access_tokens,
    etrade_cred = etrade_cred
    # ,sandbox = TRUE
  )
}

write.csv(stock4[stock4$Date!='2023-05-04 16:00:04',],paste0("technical4_",Sys.Date(),".csv"),row.names=FALSE)
write.csv(stock4[stock4$Date!='2023-05-04 16:00:04',],paste0("technical4",".csv"),row.names=FALSE)
Sys.sleep(5)

}
####THE DECIDER ENDS ################################################################################################ 

# 
# 
# trade<-etrd_transactions(
#   accoun=account,
#   count = 50,
#   fromDate = Sys.Date() - 1,
#   toDate = Sys.Date(),
#   output = c("df"),
#   access_tokens = NULL,
#   etrade_cred = etrade_cred,
#   sandbox = FALSE
# )
# 
# trade$Date<-Sys.Date()
# trade$amount<-as.numeric(trade$amount)
# 
# # write_sheet(trade, ss="1-0YM62HbgjJy5IlISZ9bbbLGxlliRFSfI-rQlYt1zZ8", sheet="Intraday")