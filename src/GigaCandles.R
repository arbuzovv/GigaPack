
# clean data and add data from NA


library(DBI)
library(ClickHouseHTTP)
library(data.table)
library(rusquant)
library(moments)
# tradestat data
tradestats = fread('/Users/vyacheslav/Documents/Rusquant/goalgo/data/tradestats_2020.csv')
tradestats = rbind(tradestats,fread('/Users/vyacheslav/Documents/Rusquant/goalgo/data/tradestats_2021.csv'))
tradestats = rbind(tradestats,fread('/Users/vyacheslav/Documents/Rusquant/goalgo/data/tradestats_2022.csv'))
tradestats = rbind(tradestats,fread('/Users/vyacheslav/Documents/Rusquant/goalgo/data/tradestats_2023.csv'))
tradestats$tradedate = as.Date(tradestats$tradedate)
tradestats[,SYSTIME:=NULL]
# obstats data
obstats = fread('/Users/vyacheslav/Documents/Rusquant/goalgo/data/obstats_2020.csv')
obstats = rbind(obstats,fread('/Users/vyacheslav/Documents/Rusquant/goalgo/data/obstats_2021.csv'))
obstats = rbind(obstats,fread('/Users/vyacheslav/Documents/Rusquant/goalgo/data/obstats_2022.csv'))
obstats = rbind(obstats,fread('/Users/vyacheslav/Documents/Rusquant/goalgo/data/obstats_2023.csv'))
obstats$tradedate = as.Date(obstats$tradedate)
obstats[,SYSTIME:=NULL]
# orderstats data
orderstats = fread('/Users/vyacheslav/Documents/Rusquant/goalgo/data/orderstats_2020.csv')
orderstats = rbind(orderstats,fread('/Users/vyacheslav/Documents/Rusquant/goalgo/data/orderstats_2021.csv'))
orderstats = rbind(orderstats,fread('/Users/vyacheslav/Documents/Rusquant/goalgo/data/orderstats_2022.csv'))
orderstats = rbind(orderstats,fread('/Users/vyacheslav/Documents/Rusquant/goalgo/data/orderstats_2023.csv'))
orderstats$tradedate = as.Date(orderstats$tradedate)
orderstats[,SYSTIME:=NULL]

universe = unique(c(unique(tradestats$secid),unique(obstats$secid),unique(orderstats$secid)))
universe = universe[universe!='']



for(i in 1:length(universe))
{
 
symbol = universe[i]

tradestats_update = getSymbols.Algopack(symbol,from = '2023-11-01',to='2023-12-01',type='tradestats')
tradestats_update$tradedate = as.Date(tradestats_update$tradedate)
tradestats_update[,SYSTIME:=NULL]

orderstats_update = getSymbols.Algopack(symbol,from = '2023-11-01',to='2023-12-01',type='orderstats')
orderstats_update$tradedate = as.Date(orderstats_update$tradedate)
orderstats_update[,SYSTIME:=NULL]

obstats_update = getSymbols.Algopack(symbol,from = '2023-11-01',to='2023-12-01',type='obstats')
obstats_update$tradedate = as.Date(obstats_update$tradedate)
obstats_update[,SYSTIME:=NULL]

if(nrow(tradestats_update)>0)
{
  tradestats_stock = rbind(tradestats[secid == symbol],tradestats_update)
  orderstats_stock= rbind(orderstats[secid == symbol],orderstats_update)
  obstats_stock = rbind(obstats[secid == symbol],obstats_update)
}

if(nrow(tradestats_update)==0)
{
  tradestats_stock = tradestats[secid == symbol]
  orderstats_stock= orderstats[secid == symbol]
  obstats_stock = obstats[secid == symbol]
}


super_candle = merge(tradestats_stock,obstats_stock,by = c('tradedate','tradetime','secid'),all = T,suffixes = c(".tradestats", ".obstats"))
super_candle = merge(super_candle,orderstats_stock,by = c('tradedate','tradetime','secid'),all = T,suffixes = c(".supercandle", ".obstats"))

#################### na values ########################
#TRADE STATS
super_candle[, pr_close := pr_close[1], by = cumsum(!is.na(pr_close))] # last value of close 
super_candle[is.na(pr_open),pr_open:=pr_close]
super_candle[is.na(pr_high),pr_high:=pr_close]
super_candle[is.na(pr_low),pr_low:=pr_close]
super_candle[, pr_change := 100 * (pr_close - pr_open) / pr_open] # recalc pr_change
super_candle[is.na(vol),vol:=0]
super_candle[is.na(val),val:=0]
super_candle[is.na(trades),trades:=0]
super_candle[, pr_vwap := pr_vwap[1], by = cumsum(!is.na(pr_vwap))] # last value of close for all type of prices
super_candle[is.na(pr_std),pr_std:=0]
super_candle[is.na(trades_b),trades_b:=0]
super_candle[is.na(trades_s),trades_s:=0]
super_candle[is.na(val_s.tradestats),val_s.tradestats:=0]
super_candle[is.na(val_b.tradestats),val_b.tradestats:=0]
super_candle[is.na(vol_s.tradestats),vol_s.tradestats:=0]
super_candle[is.na(vol_b.tradestats),vol_b.tradestats:=0]
super_candle[is.na(disb),disb:=0]
super_candle[is.na(pr_vwap_b) & is.na(pr_vwap_s),c("pr_vwap_b", "pr_vwap_s") := list(pr_close, pr_close)] # if both na
super_candle[is.na(pr_vwap_b),pr_vwap_b := pr_vwap_s] # if pr_vwap_b na then pr_vwap_s
super_candle[is.na(pr_vwap_s),pr_vwap_s := pr_vwap_b] # if pr_vwap_b na then pr_vwap_s

# ORDERBOOK STATS
super_candle[is.na(spread_bbo),spread_bbo:=10000] # spread 100%
super_candle[is.na(spread_lv10),spread_lv10:=10000] # spread 100%
super_candle[is.na(spread_1mio),spread_1mio:=10000] # spread 100%
super_candle[is.na(levels_b),levels_b:=0]
super_candle[is.na(levels_s),levels_s:=0] 
super_candle[is.na(vol_b.obstats),vol_b.obstats:=0] 
super_candle[is.na(vol_s.obstats),vol_s.obstats:=0] 
super_candle[is.na(val_b.obstats),val_b.obstats:=0] 
super_candle[is.na(val_s.obstats),val_s.obstats:=0] 
super_candle[is.na(imbalance_vol_bbo),imbalance_vol_bbo:=0] 
super_candle[is.na(imbalance_val_bbo),imbalance_val_bbo:=0] 
super_candle[is.na(imbalance_vol),imbalance_vol:=0] 
super_candle[is.na(imbalance_val),imbalance_val:=0] 
super_candle[is.na(vwap_b),vwap_b:=0] 
super_candle[is.na(vwap_s),vwap_s:=1000*1000] 
super_candle[is.na(vwap_b_1mio),vwap_b_1mio:=0] 
super_candle[is.na(vwap_s_1mio),vwap_s_1mio:=1000*1000] 

#ORDER STATS
super_candle[is.na(put_orders_b),put_orders_b:=0]
super_candle[is.na(put_orders_s),put_orders_s:=0]
super_candle[is.na(put_val_b),put_val_b:=0]
super_candle[is.na(put_val_s),put_val_s:=0]
super_candle[is.na(put_vol_b),put_vol_b:=0]
super_candle[is.na(put_vol_s),put_vol_s:=0]
super_candle[is.na(put_vol),put_vol:=0]
super_candle[is.na(put_val),put_val:=0]
super_candle[is.na(put_orders),put_orders:=0]
super_candle[is.na(cancel_orders_b),cancel_orders_b:=0]
super_candle[is.na(cancel_orders_s),cancel_orders_s:=0]
super_candle[is.na(cancel_val_b),cancel_val_b:=0]
super_candle[is.na(cancel_val_s),cancel_val_s:=0]
super_candle[is.na(cancel_vol_b),cancel_vol_b:=0]
super_candle[is.na(cancel_vol_s),cancel_vol_s:=0]
super_candle[is.na(cancel_vol),cancel_vol:=0]
super_candle[is.na(cancel_val),cancel_val:=0]
super_candle[is.na(cancel_orders),cancel_orders:=0]
super_candle[is.na(put_vwap_b),put_vwap_b:=0]
super_candle[is.na(put_vwap_s),put_vwap_s:=1000*1000]
super_candle[is.na(cancel_vwap_b),cancel_vwap_b:=0]
super_candle[is.na(cancel_vwap_s),cancel_vwap_s:=1000*1000]
# additional features
super_candle[is.na(cancel_vwap_s),cancel_vwap_s:=1000*1000]
super_candle[,otr_ratio:=put_orders/trades]
super_candle[,cancel_ratio:=cancel_orders/put_orders]
super_candle[is.na(otr_ratio),otr_ratio:=0]  
super_candle[is.na(cancel_ratio),cancel_ratio:=0]  

# apply(super_candle,2,function(x) sum(is.na(x))) # check cols by na
#View(super_candle)

######################### calc gigcandles features ###############

calc_columns <- names(super_candle)[-(1:3)]

## mean ##
gigacandles = super_candle[, lapply(.SD, mean),by='tradedate',.SDcols = calc_columns]
names(gigacandles)[-1] = paste0(names(gigacandles)[-1],'.mean') 
export_gigacandles = melt(gigacandles,id.vars = c('tradedate'),
                          variable.name = 'field', value.name = "value")
export_gigacandles$symbol = symbol
names(export_gigacandles)[1] = 'date'
setcolorder(export_gigacandles, c('date','symbol','field','value'))
export_gigacandles[is.na(value),value:=0]  

path_ch = fread('https://storage.yandexcloud.net/alpha-data/dict/path_to_clickhouse.txt')
con = dbConnect(
  ClickHouseHTTP(),
  host = path_ch$value[1],
  port = as.numeric(path_ch$value[2]),
  user = path_ch$value[3],
  password = path_ch$value[4],
  https = TRUE)

dbAppendTable(con, "algopack_giga", export_gigacandles)


## median ##
gigacandles = super_candle[, lapply(.SD, median),by='tradedate',.SDcols = calc_columns]
names(gigacandles)[-1] = paste0(names(gigacandles)[-1],'.median') 
export_gigacandles = melt(gigacandles,id.vars = c('tradedate'),
                          variable.name = 'field', value.name = "value")
export_gigacandles$symbol = symbol
names(export_gigacandles)[1] = 'date'
setcolorder(export_gigacandles, c('date','symbol','field','value'))
export_gigacandles[is.na(value),value:=0]  
dbAppendTable(con, "algopack_giga", export_gigacandles)


## sd ##
gigacandles = super_candle[, lapply(.SD, sd),by='tradedate',.SDcols = calc_columns]
names(gigacandles)[-1] = paste0(names(gigacandles)[-1],'.sd') 
export_gigacandles = melt(gigacandles,id.vars = c('tradedate'),
                   variable.name = 'field', value.name = "value")
export_gigacandles$symbol = symbol
names(export_gigacandles)[1] = 'date'
setcolorder(export_gigacandles, c('date','symbol','field','value'))
export_gigacandles[is.na(value),value:=0]  
dbAppendTable(con, "algopack_giga", export_gigacandles)


## acf(1) ##
gigacandles = super_candle[, lapply(.SD, function(x) tryCatch(acf(x,lag.max = 1,plot = F)$acf[2,1,1], error = function(e) 0)),by='tradedate',.SDcols = calc_columns]
names(gigacandles)[-1] = paste0(names(gigacandles)[-1],'.acf1') 
export_gigacandles = melt(gigacandles,id.vars = c('tradedate'),
                          variable.name = 'field', value.name = "value")
export_gigacandles$symbol = symbol
names(export_gigacandles)[1] = 'date'
setcolorder(export_gigacandles, c('date','symbol','field','value'))
export_gigacandles[is.na(value),value:=0]  
dbAppendTable(con, "algopack_giga", export_gigacandles)

## q80 ##
gigacandles = super_candle[, lapply(.SD, function(x) quantile(x,probs = 0.8,na.rm=T)),by='tradedate',.SDcols = calc_columns]
names(gigacandles)[-1] = paste0(names(gigacandles)[-1],'.q80') 
export_gigacandles = melt(gigacandles,id.vars = c('tradedate'),
                          variable.name = 'field', value.name = "value")
export_gigacandles$symbol = symbol
names(export_gigacandles)[1] = 'date'
setcolorder(export_gigacandles, c('date','symbol','field','value'))
export_gigacandles[is.na(value),value:=0]  
dbAppendTable(con, "algopack_giga", export_gigacandles)

## q20 ##
gigacandles = super_candle[, lapply(.SD, function(x) quantile(x,probs = 0.2,na.rm=T)),by='tradedate',.SDcols = calc_columns]
names(gigacandles)[-1] = paste0(names(gigacandles)[-1],'.q20') 
export_gigacandles = melt(gigacandles,id.vars = c('tradedate'),
                          variable.name = 'field', value.name = "value")
export_gigacandles$symbol = symbol
names(export_gigacandles)[1] = 'date'
setcolorder(export_gigacandles, c('date','symbol','field','value'))
export_gigacandles[is.na(value),value:=0]  
dbAppendTable(con, "algopack_giga", export_gigacandles)

## skewness ##
gigacandles = super_candle[, lapply(.SD, skewness),by='tradedate',.SDcols = calc_columns]
names(gigacandles)[-1] = paste0(names(gigacandles)[-1],'.skew') 
export_gigacandles = melt(gigacandles,id.vars = c('tradedate'),
                          variable.name = 'field', value.name = "value")
export_gigacandles$symbol = symbol
names(export_gigacandles)[1] = 'date'
setcolorder(export_gigacandles, c('date','symbol','field','value'))
export_gigacandles[is.na(value),value:=0]  
dbAppendTable(con, "algopack_giga", export_gigacandles)

## kurtosis ##
gigacandles = super_candle[, lapply(.SD, kurtosis),by='tradedate',.SDcols = calc_columns]
names(gigacandles)[-1] = paste0(names(gigacandles)[-1],'.kurtosis') 
export_gigacandles = melt(gigacandles,id.vars = c('tradedate'),
                          variable.name = 'field', value.name = "value")
export_gigacandles$symbol = symbol
names(export_gigacandles)[1] = 'date'
setcolorder(export_gigacandles, c('date','symbol','field','value'))
export_gigacandles[is.na(value),value:=0]  
dbAppendTable(con, "algopack_giga", export_gigacandles)

## sum ##
gigacandles = super_candle[, lapply(.SD, sum),by='tradedate',.SDcols = calc_columns]
names(gigacandles)[-1] = paste0(names(gigacandles)[-1],'.sum') 
export_gigacandles = melt(gigacandles,id.vars = c('tradedate'),
                          variable.name = 'field', value.name = "value")
export_gigacandles$symbol = symbol
names(export_gigacandles)[1] = 'date'
setcolorder(export_gigacandles, c('date','symbol','field','value'))
export_gigacandles[is.na(value),value:=0]  
dbAppendTable(con, "algopack_giga", export_gigacandles)


gigacandles = super_candle[, lapply(.SD, function(x) x[.N]/x[1]-1),by='tradedate',.SDcols = calc_columns]
names(gigacandles)[-1] = paste0(names(gigacandles)[-1],'.diff') 
export_gigacandles = melt(gigacandles,id.vars = c('tradedate'),
                          variable.name = 'field', value.name = "value")
export_gigacandles$symbol = symbol
names(export_gigacandles)[1] = 'date'
setcolorder(export_gigacandles, c('date','symbol','field','value'))
export_gigacandles[is.na(value),value:=0]  
dbAppendTable(con, "algopack_giga", export_gigacandles)



# IBS = (close-low)/(high-low)
gigacandles = super_candle[, lapply(.SD, function(x) (x[.N]-min(x))/(max(x)-min(x))),by='tradedate',.SDcols = calc_columns]
names(gigacandles)[-1] = paste0(names(gigacandles)[-1],'.ibs') 
export_gigacandles = melt(gigacandles,id.vars = c('tradedate'),
                          variable.name = 'field', value.name = "value")
export_gigacandles$symbol = symbol
names(export_gigacandles)[1] = 'date'
setcolorder(export_gigacandles, c('date','symbol','field','value'))
export_gigacandles[is.na(value),value:=0]  
dbAppendTable(con, "algopack_giga", export_gigacandles)

# ## lm b ##
# super_candle[,ind:=1:.N,by='tradedate']
# gigacandles = super_candle[, lapply(.SD, function(x) tryCatch(lm(x ~  ind)$coefficients[2], error = function(e) 0)),by='tradedate',.SDcols = calc_columns]
# names(gigacandles)[-1] = paste0(names(gigacandles)[-1],'.lm_b') 
# export_gigacandles = melt(gigacandles,id.vars = c('tradedate'),
#                           variable.name = 'field', value.name = "value")
# export_gigacandles$symbol = symbol
# names(export_gigacandles)[1] = 'date'
# setcolorder(export_gigacandles, c('date','symbol','field','value'))
# dbAppendTable(con, "algopack_giga", export_gigacandles)
print(symbol)
}













