



library(rusquant)
library(caret)
library(nnet)
library(e1071)
library(caretEnsemble)
library(PerformanceAnalytics)

tech_fields = getSymbolList('gigapack',type = 'tech')
candles_fields = getSymbolList('gigapack',type = 'candles')
universe = c('ROSN','SBER','GAZP','LKOH','AFLT')


################
#for(symbol_i in 1:5)
#{
symbol = universe[2]

###### get real data ##########
technical_features = getSymbols.Gigapack(symbol, type = 'tech')
all_features = technical_features[,c('symbol','date','close','EMA_EXP_RSI_28','chaikin_mfi_10','atr_28')]
for(i in c(41,145,209)) all_features = merge(all_features, getSymbols.Gigapack(symbol,field = candles_fields[i],type = 'candles'),by=c('date','symbol'))
all_features[,next_ret:=shift(close/shift(close,1)-1,-1),by='symbol']
dt_train = all_features[date<'2023-01-01',]
dt_test = all_features[date>'2023-01-01',]

###### get fake data ##########

# technical data
for(i in 1:10)
{
  alternative_ts = getSymbols.Gigapack(symbol, type = 'tech',fake = T)
  alternative_ts$sim = i
  if(i == 1) alternative_data = alternative_ts
  if(i != 1) alternative_data = rbind(alternative_data,alternative_ts)
}
all_features_fake = alternative_data[,c('symbol','date','close','EMA_EXP_RSI_28','chaikin_mfi_10','atr_28','sim')]

# add algopack data
for(i in c(41,145,209)) 
{
  algopack_data_fake = getSymbols.Gigapack(symbol,field = candles_fields[i],type = 'candles',fake = T,reps = 10)
  all_features_fake = merge(all_features_fake,algopack_data_fake,by=c('date','symbol','sim'))
}
all_features_fake[,next_ret:=shift(close/shift(close,1)-1,-1),by='symbol,sim']
dt_train_fake = all_features_fake[date<'2023-01-01',]
#dt_test_fake = all_features_fake[date>'2023-01-01',]


### fit  train ##
fitControl <- trainControl(method="repeatedcv", number=2, repeats=2)
rf_model <- train(next_ret ~ .,
                  data = dt_train[,-c('date','symbol','close')],
                  method = "rf",
                  trControl=fitControl)
### train ##
rf_forecast <- predict(rf_model, dt_train)
dt_train$predict = rf_forecast
dt_train$position = 0
dt_train[predict>0,]$position=1
dt_train[predict<0,]$position=-1

#add commisions
dt_train$pnl = dt_train$position * dt_train$next_ret
dt_train[,I_trade:= c(NA,diff(position))]
dt_train$pnl = dt_train$pnl - (0.1/100)*abs(dt_train$I_trade)

train_pnl = xts(dt_train$pnl,order.by = as.Date(dt_train$date))
charts.PerformanceSummary(train_pnl['2020/'],geometric = T)
SharpeRatio.annualized(train_pnl['2020/'])
CalmarRatio(train_pnl['2020/'])



### fake calc strategy #
dt_train_fake$position = 0
dt_train_fake$predict = 0
dt_train_fake$pnl = 0
dt_train_fake$I_trade = 0
for(j in 1:10)
{
  rf_forecast <- predict(rf_model, dt_train_fake[sim==j])
  dt_train_fake[sim==j]$predict = rf_forecast
  dt_train_fake[sim==j & predict>0,]$position=1
  dt_train_fake[sim==j & predict<0,]$position=-1
  dt_train_fake$pnl = dt_train_fake$position * dt_train_fake$next_ret
  dt_train_fake[,I_trade:= c(NA,diff(position)),by='symbol,sim']
}
dt_train_fake$pnl = dt_train_fake$pnl - (0.1/100)*abs(dt_train_fake$I_trade)

mean(dt_train_fake[,sum(pnl,na.rm = T),by = sim]$V1)
fake_strats = dt_train_fake[,sum(pnl,na.rm = T),by = sim]


library(ggplot2)
ggplot(fake_strats, aes(V1*100,fill='blue',alpha=.2)) + geom_density() + xlim(0, 1500)+ 
     geom_vline(xintercept = 100*sum(dt_train$pnl,na.rm = T))+ labs(x = "Sum of PnL, %, 2020-01-01 - 2023-01-01")




### test ##
rf_forecast <- predict(rf_model, dt_test)
dt_test$predict = rf_forecast
dt_test$position = 0
dt_test[predict>0,]$position=1
dt_test[predict<0,]$position=-1
dt_test$pnl = dt_test$position * dt_test$next_ret
dt_test[,I_trade:= c(NA,diff(position))]
dt_test$pnl = dt_test$pnl - (0.1/100)*abs(dt_test$I_trade)
test_pnl = xts(dt_test$pnl,order.by = as.Date(dt_test$date))
charts.PerformanceSummary(test_pnl['2020/'],geometric = T)
SharpeRatio.annualized(test_pnl['2020/'])
CalmarRatio(test_pnl['2020/'])

## add data to strategy portfolio
symbol_ts = rbind(train_pnl,test_pnl)
names(symbol_ts) = symbol
if(i==symbol_i) portfolio_ts = symbol_ts
if(i!=symbol_i) portfolio_ts = merge(portfolio_ts,symbol_ts)
print(symbol_i)
}

portfolio_ts = na.omit(portfolio_ts)
charts.PerformanceSummary(portfolio_ts['2020/2022'],geometric = T)
SharpeRatio.annualized(portfolio_ts['2020/2022'])
CalmarRatio(portfolio_ts['2020/2022'])



#### compare with alternative history ####









####### real data test ############

charts.PerformanceSummary(portfolio_ts['2023/'],geometric = T)
SharpeRatio.annualized(portfolio_ts['2023/'])
CalmarRatio(portfolio_ts['2023/'])









getSymbols.Gigapack(symbol, type = 'tech',field = 'bband_h_14_2')
getSymbols.Gigapack(symbol, type = 'tech',fake=T)
getSymbols.Gigapack(symbol,field = tech_fields[i], type = 'tech',fake = T)

getSymbols.Gigapack(symbol,field = candles_fields[i], type = 'candles')
getSymbols.Gigapack(symbol,field = candles_fields[i], type = 'candles',fake=T,reps=4)
getSymbols.Gigapack(symbol,field = tech_fields[i], type = 'tech',fake = T)



for(i in 1:40)
{
  x = getSymbols.Gigapack(symbol,field = tech_fields[i], type = 'tech')
  if(i==1) dt = x
  if(i!=1) dt = merge(dt,x,by=c('date','symbol'))
}


x = getSymbols.Gigapack(symbol,field = 'close', type = 'tech')
dt = merge(dt,x,by=c('date','symbol'))
dt[,next_ret:=shift(close/shift(close,1)-1,-1),by='symbol']

dt_train = dt[date<'2023-01-01',]
dt_test = dt[date>'2023-01-01',]

dt_train[,date:=NULL]
dt_train[,symbol:=NULL]


### fit ##
fitControl <- trainControl(method="repeatedcv", number=2, repeats=2)
rf_model <- train(next_ret ~ .,
                  data=dt_train,
                  method="rf",
                  trControl=fitControl)
### train ##
rf_forecast <- predict(rf_model, dt_train)
dt_train$predict = rf_forecast
dt_train$position = 0
dt_train[predict>0,]$position=1
dt_train$pnl = dt_train$position * dt_train$next_re
dt_train[,I_trade:= c(NA,diff(position))]
dt_train$pnl = dt_train$pnl - (0.1/100)*abs(dt_train$I_trade)
plot((cumsum(na.omit(dt_train$pnl))),type='l')
### test ##
rf_forecast <- predict(rf_model, dt_test)
dt_test$predict = rf_forecast
dt_test$position = 0
dt_test[predict>0,]$position=1
dt_test$pnl = dt_test$position * dt_test$next_re
dt_test[,I_trade:= c(NA,diff(position))]
dt_test$pnl = dt_test$pnl - (0.1/100)*abs(dt_test$I_trade)
plot((cumsum(na.omit(dt_test$pnl))),type='l')









###

glm_model <- train(
  price ~ 1 + carat,
  data=diamonds_train,
  method="glm",
  trControl=fitControl
)

glm_forecast <- predict(glm_model, diamonds_test)
errs_glm <- abs(100 - glm_forecast*100/diamonds_test$price)
err_glm <- c(err_glm, mean(errs_glm))

###

boost_model <- train(
  price ~ 1 + carat,
  data=diamonds_train,
  method='nnet',
  trControl=fitControl
)

boost_forecast <- predict(boost_model, diamonds_test)
errs_boost <- abs(100 - boost_forecast*100/diamonds_test$price)
err_boost <- c(err_boost, mean(errs_boost))

###








# Results
res <- resamples(multi_mod)
summary(res)












x = getSymbols.Gigapack(symbol,field = candles_fields[23])
y = 








y = getSymbols.Gigapack('ROSN',field = 'ret',fake = T,reps = 100,trim = 1)
names(y)[4] = 'sim1'

plot(x$value,y[,4][[1]])
grid()
abline(0,1)

plot(x$value,apply(y[,5:30],1,function(x) exp(mean(log(x)))))
grid()
abline(0,1)



plot(x$value,y[,4][[1]])


library(scales)

plot(100*exp(cumsum(x$value)),type='l')
for(i in 4:30) lines(100*exp(cumsum(y[,i,with=FALSE][[1]])),col=alpha('gray',0.3))
lines(as.numeric(AAPL[1,4])*exp(cumsum(ret)),col='black')


plot(x$value,z)
library(meboot)

z = meboot(x=as.numeric(x$value), reps=100,expand.sd=F,trim = 1)$ensemble
plot(x$value,type='l')
for(i in 1:100)
  lines(z[,2],col='gray')




