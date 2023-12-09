

########################  Alpha Stoic  ##################

library(data.table)
library(xts)
library(rusquant)
library(ggplot2)
library(gridExtra)
library(telegram.bot)
bot <- Bot(token = "")

load(file = 'C:/Users/?????????????/YandexDisk/Trading/Alpha/Finam/symbol_list_FINAM.RData')
a_universe = 
alpha = getSymbols('NVTK',src='Rusquant',field = 'A1_L_P1',api.key = 'KDFJKSJ',market='ru',from=Sys.Date()-4,auto.assign = F)
res = xts(alpha$pnl,order.by = as.Date(unique(alpha$date)))
res[,1]= 0
symbol='SBER'
tinkoff_token = ''
tinkoff_universe = data.table(getSymbolList(src = "tinkoff",api.key=tinkoff_token))



for(alpha_name in a_universe[1:3])
  {
    alpha = getSymbols(symbol,src='Rusquant',field = alpha_name,api.key = 'KDFJKSJ',market='ru',from=Sys.Date()-4,auto.assign = F)
     #plot(alpha$total,type='l')
    alpha[,trade:=c(NA,diff(signal))]
    #alpha$finam_com = (3.5/10000)*abs(alpha$trade)
    alpha$pnl_wc = alpha$pnl-(3.5/10000)*abs(alpha$trade)
    alpha[is.na(pnl_wc)]$pnl_wc = 0
    alpha_signal = xts(alpha$signal,order.by = as.Date(alpha$date))
    res = merge(res,alpha_signal)
    names(res)[ncol(res)] = paste(symbol,alpha_name,sep = '_')
  }
res = res[,-1]
res = res['2020-01-01/']

current_signals = tail(res,1)
current_signals = data.table(symbol = substr(names(current_signals),1,4),signal = t(current_signals[1,]))
names(current_signals)[2]='signal'

signals = (current_signals[,p:=sum(signal),by='symbol'])
signals = unique(signals[,.(symbol,p)])
print(signals)

##### trading logic ###
finam_account = ''
finam_token = '='


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
  ob_raw = getOrderbook(tinkoff_universe[ticker == trade_symbol]$figi ,depth = 10,src = "tinkoff",api.key=tinkoff_token)
  last_price = (as.numeric(ob_raw$lastPrice$units) + as.numeric(ob_raw$lastPrice$nano)/1000000000)
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




png("positions.png", height = 600, width = 1400)
grid.table(change_portfolio)
dev.off()


bot$sendPhoto(1231321321,
              photo = "positions.png",caption = paste('positions',Sys.Date()))




#change positions
for(i in 1:nrow(change_portfolio))
{
  trade_symbol = change_portfolio$symbol[i]
  ob_raw = getOrderbook(tinkoff_universe[ticker == trade_symbol]$figi ,depth = 10,src = "tinkoff",api.key=tinkoff_token)
  last_price = (as.numeric(ob_raw$lastPrice$units) + as.numeric(ob_raw$lastPrice$nano)/1000000000)
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

Sys.sleep(10)
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







