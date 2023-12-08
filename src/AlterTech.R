# AlterTech


library(jsonlite)
library(TTR)
library(data.table)

alter_ret = data.table(fromJSON('https://api.rusquant.io/altergiga?symbol=SBER&field=ret&from=2017-01-01&reps=10'))
alter_ret[,field:=NULL]
alter_ret = melt(alter_ret,id.vars = c('date', 'symbol'),
               variable.name = 'num_sim', value.name = 'ret')
alter_ret = alter_ret[order(symbol,num_sim,date)]
alter_ret[,close:=100*exp(cumsum(ret)),by='symbol,num_sim']

alter_vol = data.table(fromJSON('https://api.rusquant.io/altergiga?symbol=SBER&field=volume&from=2017-01-01&reps=10'))
alter_vol[,field:=NULL]
alter_vol = melt(alter_vol,id.vars = c('date', 'symbol'),
                 variable.name = 'num_sim', value.name = 'volume')
alter_data = merge(alter_ret,alter_vol,by = c('date', 'symbol', 'num_sim'))

alter_h = data.table(fromJSON('https://api.rusquant.io/altergiga?symbol=SBER&field=h_c&from=2017-01-01&reps=10'))
alter_h[,field:=NULL]
alter_h = melt(alter_h,id.vars = c('date', 'symbol'),
                 variable.name = 'num_sim', value.name = 'h_c')
alter_data = merge(alter_data,alter_h,by = c('date', 'symbol', 'num_sim'))

alter_l = data.table(fromJSON('https://api.rusquant.io/altergiga?symbol=SBER&field=l_c&from=2017-01-01&reps=10'))
alter_l[,field:=NULL]
alter_l = melt(alter_l,id.vars = c('date', 'symbol'),
               variable.name = 'num_sim', value.name = 'l_c')
alter_data = merge(alter_data,alter_l,by = c('date', 'symbol', 'num_sim'))

alter_data[,high:= close*(1+h_c)]
alter_data[,low:= close*(1+l_c)]
alter_data[,h_c:=NULL]
alter_data[,l_c:=NULL]
data_eod = alter_data
data_eod = data_eod[order(symbol,num_sim,date)]

##### technical ind
data_eod[,RSI_2:=RSI(close, n=2),by='symbol,num_sim']
data_eod[,RSI_5:=RSI(close, n=5),by='symbol,num_sim']
data_eod[,RSI_10:=RSI(close, n=10),by='symbol,num_sim']
data_eod[,RSI_14:=RSI(close, n=14),by='symbol,num_sim']
data_eod[,RSI_20:=RSI(close, n=20),by='symbol,num_sim']
data_eod[,RSI_28:=RSI(close, n=28),by='symbol,num_sim']
data_eod[,RSI_50:=RSI(close, n=50),by='symbol,num_sim']
data_eod[,RSI_100:=RSI(close, n=100),by='symbol,num_sim']
data_eod[,EMA_RSI_2:=RSI(close, n=2, maType="EMA"),by='symbol,num_sim']
data_eod[,EMA_RSI_5:=RSI(close, n=5, maType="EMA"),by='symbol,num_sim']
data_eod[,EMA_RSI_10:=RSI(close, n=10, maType="EMA"),by='symbol,num_sim']
data_eod[,EMA_RSI_14:=RSI(close, n=14, maType="EMA"),by='symbol,num_sim']
data_eod[,EMA_RSI_20:=RSI(close, n=20, maType="EMA"),by='symbol,num_sim']
data_eod[,EMA_RSI_28:=RSI(close, n=28, maType="EMA"),by='symbol,num_sim']
data_eod[,EMA_RSI_50:=RSI(close, n=50, maType="EMA"),by='symbol,num_sim']
data_eod[,EMA_EXP_RSI_2:=RSI(close, n=2,maType="EMA", wilder = FALSE,ratio = 1/exp(1)),by='symbol,num_sim']
data_eod[,EMA_EXP_RSI_5:=RSI(close, n=5,maType="EMA", wilder = FALSE,ratio = 1/exp(1)),by='symbol,num_sim']
data_eod[,EMA_EXP_RSI_10:=RSI(close, n=10,maType="EMA", wilder = FALSE,ratio = 1/exp(1)),by='symbol,num_sim']
data_eod[,EMA_EXP_RSI_14:=RSI(close, n=14,maType="EMA", wilder = FALSE,ratio = 1/exp(1)),by='symbol,num_sim']
data_eod[,EMA_EXP_RSI_20:=RSI(close, n=20,maType="EMA", wilder = FALSE,ratio = 1/exp(1)),by='symbol,num_sim']
data_eod[,EMA_EXP_RSI_28:=RSI(close, n=28,maType="EMA", wilder = FALSE,ratio = 1/exp(1)),by='symbol,num_sim']
data_eod[,EMA_EXP_RSI_50:=RSI(close, n=50,maType="EMA", wilder = FALSE,ratio = 1/exp(1)),by='symbol,num_sim']
data_eod[,V_RSI_2:=RSI(close, n=2, maType="WMA", wts=volume),by='symbol,num_sim']
data_eod[,V_RSI_5:=RSI(close, n=5, maType="WMA", wts=volume),by='symbol,num_sim']
data_eod[,V_RSI_10:=RSI(close, n=10, maType="WMA", wts=volume),by='symbol,num_sim']
data_eod[,V_RSI_14:=RSI(close, n=14, maType="WMA", wts=volume),by='symbol,num_sim']
data_eod[,V_RSI_20:=RSI(close, n=20, maType="WMA", wts=volume),by='symbol,num_sim']
data_eod[,V_RSI_28:=RSI(close, n=28, maType="WMA", wts=volume),by='symbol,num_sim']
data_eod[,V_RSI_50:=RSI(close, n=50, maType="WMA", wts=volume),by='symbol,num_sim']
data_eod[,V_RSI_100:=RSI(close, n=100, maType="WMA", wts=volume),by='symbol,num_sim']
data_eod[,ult_osc:=ultimateOscillator(data.table(high,low,close)),by='symbol,num_sim']
data_eod[,zig_zag_5:=ZigZag(data.table(high,low),change=5),by='symbol,num_sim']
data_eod[,bband_l_5_2:=BBands(close,5, maType = 'SMA', 2)[,1],by='symbol,num_sim']
data_eod[,bband_h_5_2:=BBands(close,5, maType = 'SMA', 2)[,3],by='symbol,num_sim']
data_eod[,bband_l_10_2:=BBands(close,10, maType = 'SMA', 2)[,1],by='symbol,num_sim']
data_eod[,bband_h_10_2:=BBands(close,10, maType = 'SMA', 2)[,3],by='symbol,num_sim']
data_eod[,bband_l_14_2:=BBands(close,14, maType = 'SMA', 2)[,1],by='symbol,num_sim']
data_eod[,bband_h_14_2:=BBands(close,14, maType = 'SMA', 2)[,3],by='symbol,num_sim']
data_eod[,bband_l_28_2:=BBands(close,28, maType = 'SMA', 2)[,1],by='symbol,num_sim']
data_eod[,bband_h_28_2:=BBands(close,28, maType = 'SMA', 2)[,3],by='symbol,num_sim']
data_eod[,bband_l_50_2:=BBands(close,50, maType = 'SMA', 2)[,1],by='symbol,num_sim']
data_eod[,bband_h_50_2:=BBands(close,50, maType = 'SMA', 2)[,3],by='symbol,num_sim']
data_eod[,bband_l_5_1:=BBands(close,5, maType = 'SMA', 1)[,1],by='symbol,num_sim']
data_eod[,bband_h_5_1:=BBands(close,5, maType = 'SMA', 1)[,3],by='symbol,num_sim']
data_eod[,bband_l_10_1:=BBands(close,10, maType = 'SMA', 1)[,1],by='symbol,num_sim']
data_eod[,bband_h_10_1:=BBands(close,10, maType = 'SMA', 1)[,3],by='symbol,num_sim']
data_eod[,bband_l_14_1:=BBands(close,14, maType = 'SMA', 1)[,1],by='symbol,num_sim']
data_eod[,bband_h_14_1:=BBands(close,14, maType = 'SMA', 1)[,3],by='symbol,num_sim']
data_eod[,bband_l_28_1:=BBands(close,28, maType = 'SMA', 1)[,1],by='symbol,num_sim']
data_eod[,bband_h_28_1:=BBands(close,28, maType = 'SMA', 1)[,3],by='symbol,num_sim']
data_eod[,bband_l_50_1:=BBands(close,50, maType = 'SMA', 1)[,1],by='symbol,num_sim']
data_eod[,bband_h_50_1:=BBands(close,50, maType = 'SMA', 1)[,3],by='symbol,num_sim']
data_eod[,adx_5:=ADX(data.table(high,low,close),5)[,4],by='symbol,num_sim']
data_eod[,adx_10:=ADX(data.table(high,low,close),10)[,4],by='symbol,num_sim']
data_eod[,adx_14:=ADX(data.table(high,low,close),14)[,4],by='symbol,num_sim']
data_eod[,adx_28:=ADX(data.table(high,low,close),28)[,4],by='symbol,num_sim']
data_eod[,adx_50:=ADX(data.table(high,low,close),50)[,4],by='symbol,num_sim']
data_eod[,atr_5:=ATR(data.table(high,low,close),5)[,2],by='symbol,num_sim']
data_eod[,atr_10:=ATR(data.table(high,low,close),10)[,2],by='symbol,num_sim']
data_eod[,atr_14:=ATR(data.table(high,low,close),14)[,2],by='symbol,num_sim']
data_eod[,atr_28:=ATR(data.table(high,low,close),28)[,2],by='symbol,num_sim']
data_eod[,atr_50:=ATR(data.table(high,low,close),50)[,2],by='symbol,num_sim']
data_eod[,kst_1:=KST(close)[,2],by='symbol,num_sim']
data_eod[,kst_2:=KST(close)[,1],by='symbol,num_sim']
data_eod[,mfi_5:=MFI(data.table(high,low,close),volume,5),by='symbol,num_sim']
data_eod[,mfi_10:=MFI(data.table(high,low,close),volume,10),by='symbol,num_sim']
data_eod[,mfi_14:=MFI(data.table(high,low,close),volume,14),by='symbol,num_sim']
data_eod[,mfi_28:=MFI(data.table(high,low,close),volume,28),by='symbol,num_sim']
data_eod[,mfi_50:=MFI(data.table(high,low,close),volume,50),by='symbol,num_sim']
data_eod[,obv:=OBV(close,volume),by='symbol,num_sim']
data_eod[,macd_osc:=MACD(close)[,1],by='symbol,num_sim']
data_eod[,macd_signal:=MACD(close)[,2],by='symbol,num_sim']
data_eod[,clv:=CLV(data.table(high,low,close)),by='symbol,num_sim']
data_eod[,chaikin_mfi_5:=CMF(data.table(high,low,close),volume,5),by='symbol,num_sim']
data_eod[,chaikin_mfi_10:=CMF(data.table(high,low,close),volume,10),by='symbol,num_sim']
data_eod[,chaikin_mfi_14:=CMF(data.table(high,low,close),volume,14),by='symbol,num_sim']
data_eod[,chaikin_mfi_28:=CMF(data.table(high,low,close),volume,28),by='symbol,num_sim']
data_eod[,chaikin_mfi_50:=CMF(data.table(high,low,close),volume,50),by='symbol,num_sim']
data_eod[,chande_momentum_5:=CMO(close,5),by='symbol,num_sim']
data_eod[,chande_momentum_10:=CMO(close,10),by='symbol,num_sim']
data_eod[,chande_momentum_14:=CMO(close,14),by='symbol,num_sim']
data_eod[,chande_momentum_28:=CMO(close,28),by='symbol,num_sim']
data_eod[,chande_momentum_50:=CMO(close,50),by='symbol,num_sim']
data_eod$stoch = 0
data_eod[high!=low,stoch:=stoch(data.table(high,low,close),)[,1],by='symbol,num_sim']
data_eod = data_eod[date>'2020-01-01']



