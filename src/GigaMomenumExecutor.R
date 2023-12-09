



library(data.table)
library(xts)
library(rusquant)
library(ggplot2)
library(gridExtra)
library(telegram.bot)
bot <- Bot(token = "telegram token")

##  get signals block
#get universe

moex_stock_universe = getSymbolList(src='moex')
liquidity_universe = getSymbols.Gigapack(paste(moex_stock_universe$SECID,collapse = ','),date='2020-01-03',field = 'spread_bbo.q80',type = 'candles')
liquidity_universe = liquidity_universe[spread_bbo.q80<10]
universe = liquidity_universe$symbol


# get data from technical indicators
trading_data = getSymbols.Gigapack(paste(universe,collapse = ','),field = 'stoch,close', type = 'tech')
# get data from microstructure indicators
algo_pack_data = getSymbols.Gigapack(paste(universe,collapse = ','),field = 'imbalance_vol.median',type = 'candles')
trading_data = merge(trading_data,algo_pack_data,by = c('date','symbol'))

trading_data[,imbalance_rank:=frank(imbalance_vol.median),by='date']
trading_data[,indicator_rank:=frank(stoch),by='date']
trading_data[,total_rank:=imbalance_rank+indicator_rank,by='date']

last_date = max(trading_data$date)
trading_data = trading_data[date==last_date] # only today
trading_data = trading_data[order(date,-total_rank)]
current_signals = head(trading_data,4) # only first 4 position
current_signals$p = 0.25
signals = unique(current_signals[,.(symbol,p)])

##

print(signals)

##### trading logic ###
finam_account = ''
finam_token = ''

#get portfolio info
finam_portfolio = getPortfolio(src = 'Finam',api.key = finam_token,clientId = finam_account)
current_balance = finam_portfolio$equity
signals$position_per_symbol = signals$p*current_balance
trade_positions = unique(signals[,.(symbol,position_per_symbol)])
universe_trade = unique(trade_positions$symbol)

finam_universe = data.table(getSymbolList(src = 'Finam',api.key = finam_token))
trade_positions = merge(trade_positions,finam_universe[board=='TQBR',.(shortName,board,code,decimals,lotSize,minStep,accruedInterest,lotDivider)],by.x = 'symbol',by.y='code',all.x = T)  


# calc position size
trade_positions$price = 0
trade_positions$size = 0
if(!exists('symbol_list_FINAM')) getSymbolList(src = 'Finam',auto.assign = TRUE)
for(i in 1:nrow(trade_positions))
{
  trade_symbol = trade_positions$symbol[i]
  last_price = try(tail((getSymbols(trade_symbol,src='Finam',period = '1min',from=Sys.Date()-2,auto.assign = F))[,4],1),silent = T)
  if(class(last_price)[1] == "try-error")
    last_price = try(tail((getSymbols(trade_symbol,src='Moex',period = '1min',from=Sys.Date()-2,auto.assign = F)[,2]),1),silent = T)
  trade_positions$size[i] = abs(as.numeric(round(trade_positions$position_per_symbol[i]/(trade_positions$lotSize[i]*last_price))))
  trade_positions$price[i] = abs(as.numeric(last_price))
}
print(trade_positions)
trade_positions$money_val = trade_positions$price*trade_positions$size*trade_positions$lotSize
trade_positions$position_delta =  trade_positions$money_val/trade_positions$position_per_symbol-1

# diff between theoretical and actual portfolio
if(length(finam_portfolio$positions)>0)
{
  change_portfolio = merge(trade_positions,
                           finam_portfolio$positions[c('securityCode','balance')],
                           by.x = 'symbol',by.y = 'securityCode',all = T)
  change_portfolio[is.na(size)]$size = 0
  change_portfolio[is.na(balance)]$balance = 0
  change_portfolio$balance_lots = change_portfolio$balance/change_portfolio$lotSize
  change_portfolio$pract = change_portfolio$balance_lots   
  change_portfolio$theor = change_portfolio$size 
  change_portfolio$trade_size = change_portfolio$size - change_portfolio$balance_lots
}
if(length(finam_portfolio$positions)==0)
{
  change_portfolio = trade_positions
  change_portfolio$pract = change_portfolio$balance_lots   
  change_portfolio$theor = change_portfolio$size 
  change_portfolio$trade_size = change_portfolio$size
}
change_portfolio = change_portfolio[order(trade_size)]
print(change_portfolio)


png("strategy_positions.png", height = 600, width = 1400)
grid.table(change_portfolio)
dev.off()


bot$sendPhoto(128937981723,
              photo = "strategy_positions.png",caption = paste('Strategy',Sys.Date()))


#change positions
for(i in 1:nrow(change_portfolio))
{
  trade_symbol = change_portfolio$symbol[i]
  last_price = try(tail((getSymbols.Finam(trade_symbol,period = '1min',
                                          from=Sys.Date()-2))[,4],1),silent = T)
  if(class(last_price)[1] == "try-error")
    last_price = try(tail((getSymbols(trade_symbol,src='Moex',period = '1min',from=Sys.Date()-2,auto.assign = F)[,2]),1),silent = T)
  size_order = abs(as.numeric(change_portfolio$trade_size[i]))
  trade_side = ifelse(change_portfolio$trade_size[i]>0,'Buy','Sell')
  board = change_portfolio$board[i]
  if(size_order!=0)
  {
    myorder = placeOrder(src = 'finam',
                         symbol = trade_symbol,
                         board = board,
                         action = trade_side,
                         totalQuantity = size_order,
                         lmtPrice = as.numeric(last_price),
                         api.key = finam_token,
                         clientId = finam_account)
    print(myorder)
  }
}

# order routing logic
my_orders = getOrders(src = 'finam',api.key = finam_token,clientId = finam_account)$orders
print(my_orders)


while(nrow(my_orders[my_orders$status=='Active',])!=0)
{
  for(i in 1:nrow(my_orders))
  {
    # if not executed
    if(my_orders$status[i] == 'Active')
    {
      print(my_orders[i,])
      cancelOrder(src = 'finam',api.key = finam_token,orderId = my_orders$transactionId[i] ,clientId = finam_account)
      last_price = try(tail((getSymbols.Finam(my_orders$securityCode[i],period = '1min',from=Sys.Date()-2))[,4],1),silent = T)
      if(class(last_price)[1] == "try-error")
        last_price = try(tail((getSymbols(my_orders$securityCode[i],src='Moex',period = '1min',from=Sys.Date()-2,auto.assign = F)[,2]),1),silent = T)
      
      change_price = change_portfolio[symbol == my_orders$securityCode[i]]$minStep/10^change_portfolio[symbol == my_orders$securityCode[i]]$decimals
      board = change_portfolio[symbol == my_orders$securityCode[i]]$board
      neworder = placeOrder(src = 'finam',
                            symbol = my_orders$securityCode[i] ,
                            board = board,
                            action = my_orders$buySell[i] ,
                            totalQuantity = my_orders$balance[i] ,
                            lmtPrice = as.numeric(my_orders$price[i])+2*change_price*ifelse(my_orders$buySell[i] == 'Buy',1,-1),
                            api.key = finam_token,
                            clientId = finam_account)
    }
  }
  Sys.sleep(10)
  my_orders = getOrders(src = 'finam',api.key = finam_token,clientId = finam_account)$orders
}




