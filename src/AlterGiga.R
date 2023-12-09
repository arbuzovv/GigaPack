
library(rusquant)
library(scales)
library(data.table)
library(ggplot2)


all_fields = getSymbolList('gigapack')

#### Compare real price and Alter Price trajectories ####
real_price = getSymbols.Gigapack('SBER',field = 'ret')
alter_price = getSymbols.Gigapack('SBER',field = 'ret',fake = T,reps = 30,trim = 1)

plot(100*exp(cumsum(real_price$ret)),type='l',ylim = c(30,300),ylab='Price',xlab='Time')
grid()
for(i in 4:100) lines(100*exp(cumsum(alter_price[sim==i]$ret)),col=alpha('gray',0.3))
lines(100*exp(cumsum(real_price$value)),col='black',lwd=3)

## Compare real price and Alter Price characteristics
real_price$sim = 0
setcolorder(real_price, names(alter_price))
price_data = rbind(data.table(real_price,type='real'),data.table(alter_price[,1:4],type='alter'))

#compare density
ggplot(price_data, aes(ret,fill=factor(type),alpha=.01)) + geom_density(adjust=.3)

#compare ACF
acf(real_price$ret)
acf(alter_price[sim==1]$ret)
acf(alter_price[sim==2]$ret)


#### Compare AlgoPack data with Alternative Data
#### Compare real price and Alter Price trajectories ####
real_data = getSymbols.Gigapack('ROSN',field = 'trades.skew',from='2023-01-01')
alter_data = getSymbols.Gigapack('ROSN',field = 'trades.skew',fake = T,reps = 30,trim = 0.1)




plot(real_data[date>'2023-09-01']$trades.skew,type='l',ylab='trades.skew',xlab='Time')
grid()
for(i in 1:10) lines(alter_data[date>'2023-09-01'][sim==i]$trades.skew,col=alpha('gray',0.5))
lines(real_data[date>'2023-09-01']$trades.skew,col='black',lwd=0.2)


real_data$sim = 0
setcolorder(real_data, names(alter_data))
compare_data = rbind(data.table(real_data,type='real'),data.table(alter_data[,1:4],type='alter'))

#compare density
ggplot(compare_data, aes(trades.skew  ,fill=factor(type),alpha=.01)) + geom_density(adjust=.3)

#compare ACF
acf(real_data$trades.skew)
acf(alter_data[sim==1]$trades.skew)
