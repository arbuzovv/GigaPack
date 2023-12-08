

library(rusquant)
library(scales)
library(data.table)
library(ggplot2)


all_fields = getSymbolList('gigapack')

#### Compare real price and Alter Price trajectories ####
real_price = getSymbols.Gigapack('ROSN',field = 'ret')
alter_price = getSymbols.Gigapack('ROSN',field = 'ret',fake = T,reps = 100,trim = 1)

plot(100*exp(cumsum(real_price$value)),type='l',ylim = c(30,300),ylab='Price',xlab='Time')
grid()
for(i in 4:100) lines(100*exp(cumsum(alter_price[,i,with=FALSE][[1]])),col=alpha('gray',0.3))
lines(100*exp(cumsum(real_price$value)),col='black',lwd=3)

## Compare real price and Alter Price characteristics
names(alter_price)[4] = 'value'
price_data = rbind(data.table(real_price,type='real'),data.table(alter_price[,1:4],type='alter'))
  
#compare density
ggplot(price_data, aes(value,fill=factor(type),alpha=.01)) + geom_density(adjust=.3)

#compare ACF
acf(real_price$value)
acf(alter_price$value)


#### Compare AlgoPack data with Alternative Data
#### Compare real price and Alter Price trajectories ####
real_data = getSymbols.Gigapack('ROSN',field = 'trades.skew')
alter_data = getSymbols.Gigapack('ROSN',field = 'trades.skew',fake = T,reps = 30,trim = 0.1)


plot(real_data$value,type='l',ylab='disb.mean',xlab='Time')
grid()
for(i in 4:30) lines(alter_data[,i,with=FALSE][[1]],col=alpha('gray',0.3))
lines(real_data$value,col='black',lwd=0.1)


names(alter_data)[4] = 'value'
compare_data = rbind(data.table(real_data,type='real'),data.table(alter_data[,1:4],type='alter'))

#compare density
ggplot(compare_data, aes(value,fill=factor(type),alpha=.01)) + geom_density(adjust=.3)

#compare ACF
acf(real_data$value)
acf(alter_data$value)
