
###  Giga momentum ###


library(rusquant)
library(PerformanceAnalytics)
library(data.table)
library(caret)

tech_fields = getSymbolList('gigapack',type = 'tech')
candles_fields = getSymbolList('gigapack',type = 'candles')
moex_stock_universe = getSymbolList(src='moex')


liquidity_universe = getSymbols.Gigapack(paste(moex_stock_universe$SECID,collapse = ','),date='2020-01-03',field = 'spread_bbo.q80',type = 'candles')
liquidity_universe = liquidity_universe[spread_bbo.q80<10]
universe = liquidity_universe$symbol

# feature from algopack = 174 - imbalance_vol.median
# disb.median
################

###### get real data ##########
# get data from technical indicators
trading_data = getSymbols.Gigapack(paste(universe,collapse = ','),field = 'stoch,close', type = 'tech')
# get data from microstructure indicators
algo_pack_data = getSymbols.Gigapack(paste(universe,collapse = ','),field = 'disb.median',type = 'candles')

# merge data
trading_data = merge(trading_data,algo_pack_data,by = c('date','symbol'))
trading_data[,next_ret:=shift(close/shift(close,1)-1,-1),by='symbol']
trading_data[,imbalance_rank:=frank(imbalance_vol.median),by='date']
trading_data[,indicator_rank:=frank(stoch),by='date']
trading_data[,total_rank:=imbalance_rank+indicator_rank,by='date']
trading_data = trading_data[order(date,-total_rank)]
trading_data = na.omit(trading_data)


number_op_positions = 4 # choose number of position 
#pnl = trading_data[,mean(next_ret[1:number_op_positions]),by=date]
trading_portfolio = trading_data[,list(symbol = symbol[1:number_op_positions],
                   pnl = next_ret[1:number_op_positions]/number_op_positions,
                   position = 1,
                   total_rank = total_rank[1:number_op_positions]),by=date]

# add cost for each day rebalancing
full_date_symbol = expand.grid(date = unique(trading_portfolio$date),
            symbol = unique(trading_portfolio$symbol))


trading_portfolio = merge(trading_portfolio,full_date_symbol,all = T,by=c('date','symbol'))
trading_portfolio[is.na(position),position:=0]
trading_portfolio[is.na(pnl),pnl:=0]
trading_portfolio[,I_trade:= c(NA,diff(position)),by=symbol]
trading_portfolio[is.na(I_trade),I_trade:=0]
trading_portfolio$pnl = trading_portfolio$pnl - (0.05/100)*abs(trading_portfolio$I_trade)/number_op_positions
ts_portf =  trading_portfolio[,sum(pnl),by='date']

train_pnl = na.omit(xts(ts_portf$V1,order.by = as.Date(ts_portf$date)))
charts.PerformanceSummary(train_pnl['2020/2022'],geometric = T)


my_sharpe =  SharpeRatio.annualized(train_pnl['2020/2022'])
my_calmar =  CalmarRatio(train_pnl['2020/2022'])
my_return = Return.annualized(train_pnl['2020/2022'])



######################################## overfiting estimation #######################################
# alternative technical data
alternative_data = data.table()
for(symbol in universe)
{
  alternative_data = rbind(alternative_data,
                           getSymbols.Gigapack(symbol,field = 'close,stoch' ,type = 'tech',fake = T,reps = 20))
  print(symbol)
}
# algopack data
alternative_algopack = data.table()
for(symbol in universe)
{
  alternative_algopack = rbind(alternative_algopack,
                           getSymbols.Gigapack(symbol,field = 'imbalance_vol.median' ,type = 'candles',fake = T,reps = 20))
  print(symbol)
}


alternative_data = merge(alternative_data,alternative_algopack,by=c('date','symbol','sim'))
alternative_data = alternative_data[order(symbol,sim,date)]
alternative_data[,next_ret:=shift(close/shift(close,1)-1,-1),by='symbol,sim']

alternative_data[,imbalance_rank:=frank(imbalance_vol.median),by='sim,date']
alternative_data[,indicator_rank:=frank(stoch),by='sim,date']
alternative_data[,total_rank:=imbalance_rank+indicator_rank,by='sim,date']
alternative_data = alternative_data[order(sim,date,-total_rank)]
alternative_data = na.omit(alternative_data)


number_op_positions = 4 # choose number of position 
#pnl = trading_data[,mean(next_ret[1:number_op_positions]),by=date]
alternative_portfolio = alternative_data[,list(symbol = symbol[1:number_op_positions],
                                       pnl = next_ret[1:number_op_positions]/number_op_positions,
                                       position = 1,
                                       total_rank = total_rank[1:number_op_positions]),by='sim,date']
# add cost for each day rebalancing
full_date_symbol = expand.grid(date = unique(alternative_portfolio$date),
                               symbol = unique(alternative_portfolio$symbol),
                               sim = unique(alternative_portfolio$sim))

alternative_portfolio = merge(alternative_portfolio,full_date_symbol,all = T,by=c('date','symbol','sim'))

alternative_portfolio[is.na(position),position:=0]
alternative_portfolio[is.na(pnl),pnl:=0]
alternative_portfolio[,I_trade:= c(NA,diff(position)),by='sim,date']
alternative_portfolio[is.na(I_trade),I_trade:=0]

alternative_portfolio$pnl = alternative_portfolio$pnl - (0.05/100)*abs(alternative_portfolio$I_trade)/number_op_positions
#alt_results =  alternative_portfolio[as.Date(date)<'2023-01-01',sum(pnl),by='sim,date']
alt_results =  alternative_portfolio[,sum(pnl),by='sim,date']


alt_calmar = alt_results[,CalmarRatio(xts(V1,order.by = as.Date(date))),by=sim]
alt_sharpe = alt_results[,SharpeRatio.annualized(xts(V1,order.by = as.Date(date))),by=sim]
alt_return = alt_results[,Return.annualized(xts(V1,order.by = as.Date(date))),by=sim]


library(ggplot2)
ggplot(alt_return, aes(V1*100)) + geom_density(fill='blue',alpha=.2) + xlim(0,100) + 
  geom_vline(xintercept = 100*sum(my_return,na.rm = T))+ labs(x = "Sum of PnL, %, 2020-01-01 - 2023-01-01")

ggplot(alt_sharpe, aes(V1)) + geom_density(fill='green',alpha=.2) + xlim(0, 3) + 
  geom_vline(xintercept = sum(my_sharpe,na.rm = T))+ labs(x = "Sharpe ratio, 2020-01-01 - 2023-01-01")

ggplot(alt_calmar, aes(V1)) + geom_density(fill='red',alpha=.2) + xlim(0, 3) + 
  geom_vline(xintercept = sum(my_calmar,na.rm = T))+ labs(x = "Calmar, 2020-01-01 - 2023-01-01")


(1 + simple_ret).cumprod()

plot(as.Date(ts_portf$date),cumprod(1+ts_portf$V1)-1,type='l',xlab='Time',ylab='PnL,')
grid()
for(i in 1:20)
{
  sim_ts = alt_results[sim==i]
  lines(as.Date(sim_ts$date),cumprod(1+sim_ts$V1)-1,col=alpha('gray',0.5))
}
lines(as.Date(ts_portf$date),cumprod(1+ts_portf$V1)-1,lwd=2)

IMOEX = getSymbols.Finam('IMOEX',from='2020-01-01')
imoex_ret = dailyReturn(IMOEX[,4])
lines(as.Date(index(imoex_ret)),cumprod(1+imoex_ret)-1,lwd=2,col='blue')
legend('topleft',col=c('black','blue','gray'),lwd=c(2,2,1),legend=c('GigaMomentum','iMOEX','Alternative Momentum'))



oss_sharpe =  SharpeRatio.annualized(train_pnl['2023/'])
oss_calmar =  CalmarRatio(train_pnl['2023/'])
oss_return = Return.annualized(train_pnl['2023/'])
