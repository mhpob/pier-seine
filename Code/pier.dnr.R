source('p:/obrien/menhaden/pier analysis/pier.data.agg.R')
library(ggplot2)

ind <- read.csv('p:/obrien/potomac/juvind/jiseine19572011.csv', header = T)
ind2 <- read.csv('p:/obrien/potomac/juvind/MDDNRseinesurvey20122013.csv', header = T)

ind <- merge(ind,ind2, all = T)
rm(ind2)


dnr <- ind[ind$YEAR >= 1999 & ind$YEAR <= 2012 & ind$RIVER == 'PATUXENTRIVER',]
dnr <- dcast(dnr,SITENAME + ROUND + MONTH + DAY + YEAR ~ SPECNAME, value.var = 'SPECCNT', sum)
sterr <- function(x) {sqrt(var(x)/length(x))}
geo_mean <- function(data) {(sum(data + 1)^(1/length(data)))-1}

dnragg <- aggregate(ATLANTICMENHADEN ~ YEAR, data = dnr, FUN = mean)


dnragg <- cbind(dnragg,(aggregate(ATLANTICMENHADEN ~ YEAR, data = dnr, FUN = sterr))[2])
names(dnragg) <- c('Year','Men','SE')
cblagg <- aggregate(fish$men.yoy ~ env$YEAR, FUN = mean)
cblagg <- cbind(cblagg, aggregate(fish$men.yoy ~ env$YEAR, FUN = sterr)[2])
names(cblagg) <- c('Year','Men','SE')

ggplot(cblagg, aes(x=Year, y=Men, ymin = Men - SE, ymax = Men + SE)) + geom_pointrange() + 
  scale_y_log10() + ggtitle('CBL Pier') + ylab('Menhaden Catch')
ggplot(dnragg, aes(x=Year, y=Men, ymin = Men - SE, ymax = Men + SE)) + geom_pointrange() + 
  scale_y_log10() + ggtitle('MD DNR, Patuxent River') + ylab('Menhaden Catch')

plot(x=cblagg$Men, y=dnragg$Men, type="n", ylab = 'MD DNR',xlab = 'CBL Pier', main = 'Average Yearly Menhaden Catch, 1999-2012')
text(x=cblagg$Men, y=dnragg$Men, labels = substr(dnragg$Year,3,4))


ggplot(cblagg, aes(x=Year, y=Men)) + geom_bar(stat = 'identity') + ylim(0,0.72) +
  ggtitle('CBL Pier') + ylab('Geometric Mean Menhaden Catch')
ggplot(dnragg, aes(x=Year, y=Men)) + geom_bar(stat = 'identity') + ylim(0,0.72) +
  ggtitle('MD DNR, Patuxent River') + ylab('Geometric Mean Menhaden Catch')


ggplot(cblagg, aes(x=Year, y=Men)) + geom_line() + 
  geom_errorbar(aes(ymin = cblagg$Men - cblagg$SE, ymax = cblagg$Men + cblagg$SE), position = 'dodge', width = 0.1) +
  scale_y_log10() + ggtitle('CBL Pier') + ylab('Mean Menhaden Catch')
ggplot(dnragg, aes(x=Year, y=Men)) + geom_line() + 
  geom_errorbar(aes(ymin = dnragg$Men - dnragg$SE, ymax = dnragg$Men + dnragg$SE), position = 'dodge', width = 0.1) +
  scale_y_log10() + ggtitle('MD DNR, Patuxent River') + ylab('Mean Menhaden Catch')
