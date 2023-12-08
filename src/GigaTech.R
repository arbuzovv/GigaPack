# Giga Tech

library(TTR)
library(DBI)
library(ClickHouseHTTP)
library(data.table)



path_ch = fread('https://storage.yandexcloud.net/alpha-data/dict/path_to_clickhouse.txt')
old_con  = dbConnect(
  ClickHouseHTTP(),
  host = path_ch$value[1],
  port = as.numeric(path_ch$value[2]),
  user = path_ch$value[3],
  password = path_ch$value[4],
  https = TRUE)
#ru candles
data_eod = dbReadTable(old_con,'eod_ru')
data_eod = data.table(data_eod)
data_eod = data_eod[,-c('adjclose')]
data_eod = unique(data_eod)
data_eod = data_eod[order(symbol,date)]

universe = data_eod[,.N,by='symbol'][N>200]$symbol
data_eod = data_eod[symbol %in% universe]
data_eod[,ret:=ROC(close),by='symbol']

data_eod[,RSI_2:=RSI(close, n=2),by='symbol']
data_eod[,RSI_5:=RSI(close, n=5),by='symbol']
data_eod[,RSI_10:=RSI(close, n=10),by='symbol']
data_eod[,RSI_14:=RSI(close, n=14),by='symbol']
data_eod[,RSI_20:=RSI(close, n=20),by='symbol']
data_eod[,RSI_28:=RSI(close, n=28),by='symbol']
data_eod[,RSI_50:=RSI(close, n=50),by='symbol']
data_eod[,RSI_100:=RSI(close, n=100),by='symbol']
data_eod[,EMA_RSI_2:=RSI(close, n=2, maType="EMA"),by='symbol']
data_eod[,EMA_RSI_5:=RSI(close, n=5, maType="EMA"),by='symbol']
data_eod[,EMA_RSI_10:=RSI(close, n=10, maType="EMA"),by='symbol']
data_eod[,EMA_RSI_14:=RSI(close, n=14, maType="EMA"),by='symbol']
data_eod[,EMA_RSI_20:=RSI(close, n=20, maType="EMA"),by='symbol']
data_eod[,EMA_RSI_28:=RSI(close, n=28, maType="EMA"),by='symbol']
data_eod[,EMA_RSI_50:=RSI(close, n=50, maType="EMA"),by='symbol']
data_eod[,EMA_EXP_RSI_2:=RSI(close, n=2,maType="EMA", wilder = FALSE,ratio = 1/exp(1)),by='symbol']
data_eod[,EMA_EXP_RSI_5:=RSI(close, n=5,maType="EMA", wilder = FALSE,ratio = 1/exp(1)),by='symbol']
data_eod[,EMA_EXP_RSI_10:=RSI(close, n=10,maType="EMA", wilder = FALSE,ratio = 1/exp(1)),by='symbol']
data_eod[,EMA_EXP_RSI_14:=RSI(close, n=14,maType="EMA", wilder = FALSE,ratio = 1/exp(1)),by='symbol']
data_eod[,EMA_EXP_RSI_20:=RSI(close, n=20,maType="EMA", wilder = FALSE,ratio = 1/exp(1)),by='symbol']
data_eod[,EMA_EXP_RSI_28:=RSI(close, n=28,maType="EMA", wilder = FALSE,ratio = 1/exp(1)),by='symbol']
data_eod[,EMA_EXP_RSI_50:=RSI(close, n=50,maType="EMA", wilder = FALSE,ratio = 1/exp(1)),by='symbol']
data_eod[,V_RSI_2:=RSI(close, n=2, maType="WMA", wts=volume),by='symbol']
data_eod[,V_RSI_5:=RSI(close, n=5, maType="WMA", wts=volume),by='symbol']
data_eod[,V_RSI_10:=RSI(close, n=10, maType="WMA", wts=volume),by='symbol']
data_eod[,V_RSI_14:=RSI(close, n=14, maType="WMA", wts=volume),by='symbol']
data_eod[,V_RSI_20:=RSI(close, n=20, maType="WMA", wts=volume),by='symbol']
data_eod[,V_RSI_28:=RSI(close, n=28, maType="WMA", wts=volume),by='symbol']
data_eod[,V_RSI_50:=RSI(close, n=50, maType="WMA", wts=volume),by='symbol']
data_eod[,V_RSI_100:=RSI(close, n=100, maType="WMA", wts=volume),by='symbol']

data_eod[,ult_osc:=ultimateOscillator(data.table(high,low,close)),by='symbol']
data_eod[,zig_zag_5:=ZigZag(data.table(high,low),change=5),by='symbol']

data_eod[,bband_l_5_2:=BBands(close,5, maType = 'SMA', 2)[,1],by='symbol']
data_eod[,bband_h_5_2:=BBands(close,5, maType = 'SMA', 2)[,3],by='symbol']
data_eod[,bband_l_10_2:=BBands(close,10, maType = 'SMA', 2)[,1],by='symbol']
data_eod[,bband_h_10_2:=BBands(close,10, maType = 'SMA', 2)[,3],by='symbol']
data_eod[,bband_l_14_2:=BBands(close,14, maType = 'SMA', 2)[,1],by='symbol']
data_eod[,bband_h_14_2:=BBands(close,14, maType = 'SMA', 2)[,3],by='symbol']
data_eod[,bband_l_28_2:=BBands(close,28, maType = 'SMA', 2)[,1],by='symbol']
data_eod[,bband_h_28_2:=BBands(close,28, maType = 'SMA', 2)[,3],by='symbol']
data_eod[,bband_l_50_2:=BBands(close,50, maType = 'SMA', 2)[,1],by='symbol']
data_eod[,bband_h_50_2:=BBands(close,50, maType = 'SMA', 2)[,3],by='symbol']
data_eod[,bband_l_5_1:=BBands(close,5, maType = 'SMA', 1)[,1],by='symbol']
data_eod[,bband_h_5_1:=BBands(close,5, maType = 'SMA', 1)[,3],by='symbol']
data_eod[,bband_l_10_1:=BBands(close,10, maType = 'SMA', 1)[,1],by='symbol']
data_eod[,bband_h_10_1:=BBands(close,10, maType = 'SMA', 1)[,3],by='symbol']
data_eod[,bband_l_14_1:=BBands(close,14, maType = 'SMA', 1)[,1],by='symbol']
data_eod[,bband_h_14_1:=BBands(close,14, maType = 'SMA', 1)[,3],by='symbol']
data_eod[,bband_l_28_1:=BBands(close,28, maType = 'SMA', 1)[,1],by='symbol']
data_eod[,bband_h_28_1:=BBands(close,28, maType = 'SMA', 1)[,3],by='symbol']
data_eod[,bband_l_50_1:=BBands(close,50, maType = 'SMA', 1)[,1],by='symbol']
data_eod[,bband_h_50_1:=BBands(close,50, maType = 'SMA', 1)[,3],by='symbol']
data_eod[,bband_p_5_2:=BBands(close,5, maType = 'SMA', 2)[,4],by='symbol']
data_eod[,bband_p_10_2:=BBands(close,10, maType = 'SMA', 2)[,4],by='symbol']
data_eod[,bband_p_14_2:=BBands(close,14, maType = 'SMA', 2)[,4],by='symbol']
data_eod[,bband_p_28_2:=BBands(close,28, maType = 'SMA', 2)[,4],by='symbol']
data_eod[,bband_p_50_2:=BBands(close,50, maType = 'SMA', 2)[,4],by='symbol']
data_eod[,bband_p_5_1:=BBands(close,5, maType = 'SMA', 1)[,4],by='symbol']
data_eod[,bband_p_10_1:=BBands(close,10, maType = 'SMA', 1)[,4],by='symbol']
data_eod[,bband_p_14_1:=BBands(close,14, maType = 'SMA', 1)[,4],by='symbol']
data_eod[,bband_p_28_1:=BBands(close,28, maType = 'SMA', 1)[,4],by='symbol']
data_eod[,bband_p_50_1:=BBands(close,50, maType = 'SMA', 1)[,4],by='symbol']

data_eod[,adx_5:=ADX(data.table(high,low,close),5)[,4],by='symbol']
data_eod[,adx_10:=ADX(data.table(high,low,close),10)[,4],by='symbol']
data_eod[,adx_14:=ADX(data.table(high,low,close),14)[,4],by='symbol']
data_eod[,adx_28:=ADX(data.table(high,low,close),28)[,4],by='symbol']
data_eod[,adx_50:=ADX(data.table(high,low,close),50)[,4],by='symbol']

data_eod[,atr_5:=ATR(data.table(high,low,close),5)[,2],by='symbol']
data_eod[,atr_10:=ATR(data.table(high,low,close),10)[,2],by='symbol']
data_eod[,atr_14:=ATR(data.table(high,low,close),14)[,2],by='symbol']
data_eod[,atr_28:=ATR(data.table(high,low,close),28)[,2],by='symbol']
data_eod[,atr_50:=ATR(data.table(high,low,close),50)[,2],by='symbol']

data_eod[,kst_1:=KST(close)[,2],by='symbol']
data_eod[,kst_2:=KST(close)[,1],by='symbol']


data_eod[,mfi_5:=MFI(data.table(high,low,close),volume,5),by='symbol']
data_eod[,mfi_10:=MFI(data.table(high,low,close),volume,10),by='symbol']
data_eod[,mfi_14:=MFI(data.table(high,low,close),volume,14),by='symbol']
data_eod[,mfi_28:=MFI(data.table(high,low,close),volume,28),by='symbol']
data_eod[,mfi_50:=MFI(data.table(high,low,close),volume,50),by='symbol']
data_eod[,obv:=OBV(close,volume),by='symbol']

data_eod[,macd_osc:=MACD(close)[,1],by='symbol']
data_eod[,macd_signal:=MACD(close)[,2],by='symbol']
data_eod[,clv:=CLV(data.table(high,low,close)),by='symbol']

data_eod[,chaikin_mfi_5:=CMF(data.table(high,low,close),volume,5),by='symbol']
data_eod[,chaikin_mfi_10:=CMF(data.table(high,low,close),volume,10),by='symbol']
data_eod[,chaikin_mfi_14:=CMF(data.table(high,low,close),volume,14),by='symbol']
data_eod[,chaikin_mfi_28:=CMF(data.table(high,low,close),volume,28),by='symbol']
data_eod[,chaikin_mfi_50:=CMF(data.table(high,low,close),volume,50),by='symbol']


data_eod[,chande_momentum_5:=CMO(close,5),by='symbol']
data_eod[,chande_momentum_10:=CMO(close,10),by='symbol']
data_eod[,chande_momentum_14:=CMO(close,14),by='symbol']
data_eod[,chande_momentum_28:=CMO(close,28),by='symbol']
data_eod[,chande_momentum_50:=CMO(close,50),by='symbol']

data_eod$stoch = 0
data_eod[high!=low,stoch:=stoch(data.table(high,low,close),)[,1],by='symbol']

data_eod = data_eod[date>'2020-01-01']
data_eod$volume = as.numeric(data_eod$volume)

str(data_eod)

giga_tech = melt(data_eod,id.vars = c('date','symbol'),
                          variable.name = 'field', value.name = "value")
giga_tech = giga_tech[order(symbol,date)]

setcolorder(giga_tech, c('date','symbol','field','value'))
giga_tech[is.na(value),value:=0]  


dbAppendTable(old_con, "algopack_gigatech", giga_tech)




