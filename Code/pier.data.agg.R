setwd("p:/obrien/menhaden/pier analysis")
library(reshape2);library(lubridate)

## Missing salinity data was extrapolated (mean of before and after)
## Missing temp data was taken from NOAA tides and currents website
## Empty hauls on 9/15/2004, 10/1/2004, and 10/8/2004 were removed to allow for analysis

pier <- read.csv("assemblage.csv", header = T, na.strings = "NA")
pier$DATE <- mdy(pier$DATE)

## Weight catch by area covered. Area calculated as follows:
#   (Parallel) hauls 1 & 2 were 80x222ft rectangles (1650 m^2)
#   CPUE haul was quarter-sweep with 80ft radius (467 m^2)
#   If CPUE hauls are 1 UE, Parallel seines will have UE of 3.53 (1650/467). Divide catch of parallel 
#   seines by 3.53. In the code below, I am binding unweighted env vars (assumed to be in columns 1:9)
#   to newly weighted species.

pierF <- cbind(pier[pier$CPUE== F, 1:9], pier[pier$CPUE == F, 10:dim(pier)[2]] / 3.53)

## Bind rows back together
pier <- rbind(pierF,pier[pier$CPUE == "TRUE",])

## Combine silversides (as per Wingate and Secor 2008) and drop age 1+ fish
pier$silvers <- pier$atlsilvers + pier$inlsilvers
pier <- pier[,!names(pier) %in% 
               c("atlsilvers","inlsilvers","menad","wpadult",
                 "sbadult","sflounderadult","blueadult","croakadult")]

# Aggregate assemblage data by date via means (i.e., average hauls from the same day)
fish <- pier[,10:dim(pier)[2]]
fish <- aggregate(fish,list(DATE = pier$DATE),mean)
row.names(fish) <- fish$DATE
fish <- fish[,2:dim(fish)[2]]

## Apply inclusion rule as per (W&S 2008).
##    Drop species that do not occur in at least 3% of hauls or with a 
##    mean CPUE < 0.03 fish/haul.
cpue <- (colSums(fish)/dim(fish)[1]) >= 0.03
presence <- (colSums(fish > 0)/dim(fish)[1]) >= 0.03

fish <- fish[,(cpue | presence == T)]

# Split out env vars. keep year, week, temp, sal
# Drop record num, CPUE, number, time.
# Place date in first col of both data sets. Remove duplicates.
env <- pier[, 4:8]
env <- env[!duplicated(env$DATE),]


# Add flow data
flow <- 'http://waterservices.usgs.gov/nwis/dv/?site=01594440&startDT=1999-01-01&statCd=00003&variable=00060&format=rdb'
flow <- droplevels(read.table(flow, sep = '\t', header = T, stringsAsFactors = F)[-1, -c(1:2, 5)])
names(flow) <- c('date','flow')
flow[,1] <- ymd(flow[,1])
flow[,2] <- as.numeric(flow[,2])

flow[,2] <- flow[,2] * 0.0283168 #convert from ft^3/s to m^3/s
flow$season <-ifelse(month(flow$date) >= 1 & month(flow$date) <= 3, "flowwinter", 
                    ifelse(month(flow$date) >= 4 & month(flow$date) <= 6, "flowspring",
                    ifelse(month(flow$date) >= 7 & month(flow$date) <= 9, "flowsummer",
                           "flowfall")))
flow$year <- year(flow$date)
flow <- dcast(flow, year ~ season, value.var = "flow", mean, na.rm=T)

env <- merge(env, flow, by.x = 'YEAR', by.y = 'year')
row.names(env) <- env$DATE
env <- env[order(env$DATE),]


# Add MEI (Multivariate ENSO Index)
mei <- read.table('http://www.esrl.noaa.gov/psd/enso/mei/table.html', header = T, 
               skip = 12, nrows = year(now()) - 1949, fill = T)
names(mei) <- c('year',1,2,3,4,5,6,7,8,9,10,11,12)

mei.melt <- melt(mei, id = c('year'), variable.name = 'month', value.name = 'mei')
mei.melt$month <- as.integer(levels(mei.melt$month)[mei.melt$month])
mei.melt$season <-ifelse(mei.melt$month <= 3, "meiwinter", 
                    ifelse(mei.melt$month >= 4 & mei.melt$month <= 6, "meispring",
                           ifelse(mei.melt$month >= 7 & mei.melt$month <= 9, "meisummer", "meifall")))
mei.melt <- dcast(mei.melt, year ~ season, value.var = "mei", mean, na.rm = T)

env <- merge(env, mei.melt, by.x = 'YEAR', by.y = 'year')
env <- env[order(env$DATE),]


# Add NAO
nao <- read.table(
  'http://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/norm.nao.monthly.b5001.current.ascii', 
  col.names = c('year', 'month', 'nao'))

nao$season <-ifelse(nao$month <= 3, "naowinter", 
                     ifelse(nao$month >= 4 & nao$month <= 6, "naospring",
                      ifelse(nao$month >= 7 & nao$month <= 9, "naosummer", "naofall")))
nao <- dcast(nao, year ~ season, value.var = "nao", mean, na.rm = T)
env <- merge(env, nao, by.x = 'YEAR', by.y = 'year', all.y = F)

## Add wind data using wunderimp(). PAX air code = KNHK
# source("p:/obrien/coastal bays/dnr/wunderimp.r")
# wind <- wunderimp("KNHK",1999:2012,1,12)
# write.table(wind,file='wind.txt')

## RESTART RECODING HERE!!!
wind <- read.table('wind.txt', header = T)
rot.axis <- 0 #rotational axis for pier ~116; for Bay ~0
wind$wind.factor <- -cos((rot.axis - wind$WindDirDeg)*(pi/180))*wind$Mean.Wind.SpeedMPH*1.609 #1.609 = change MPH to KPH
wind$Date <- as.Date(wind$Date,format = '%Y-%m-%d')

# Average wind factor of day of pier sampling and two days prior (~48 hrs)
# j <- which(wind$Date %in% env$DATE,arr.ind=F)
# k <- wind[wind$Date %in% env$DATE,]
# for(n in 1:241){
#   k$wf[n]<-mean(c(wind[j[[n]],24],wind[j[[n]]-1,24],wind[j[[n]]-2,24]),na.rm=T)
# }
# env<-merge(env,k[,c(1,25)],by.x= 'DATE',by.y='Date',all.x=T)      
# env[6,17] <- mean(c(7.50024910,-2.25392791))
# env[c(16,17),17] <- mean(c(-6.44661049,-2.25286894))

j <- strptime(wind$Date, format = "%Y-%m-%d")
wind$Month <- j$mon+1
wind$Yr <- j$year+1900
wind$season <-ifelse(wind$Month>=1 & wind$Month <=3, "windwinter", 
                    ifelse(wind$Month>=4 & wind$Month <=6, "windspring",
                           ifelse(wind$Month>=7 & wind$Month<=9, "windsummer", "windfall")))
wind <- dcast(wind,Yr~season,value.var="wind.factor",mean,na.rm=T)
env <- merge(env, wind, by.x='YEAR', by.y='Yr')

# More Temperature Data
temp <- read.csv("p:/obrien/menhaden/pier analysis/pier data/cbl_swtemp_1938-2012.csv",header=T)
temp.melt <- melt(temp,id=c("Year",'Annual','Winter..J.M.','Spring..A.J.',
                            'Summer..J.S.','Fall..O.D.'))
names(temp.melt)[7] <- 'Mon'
names(temp.melt)[8] <- 'Monthly'
temp.melt$Mon <- as.factor(ifelse(temp.melt$Mon == 'Jan',1,
                          ifelse(temp.melt$Mon == 'Feb',2,
                          ifelse(temp.melt$Mon == 'Mar',3,
                          ifelse(temp.melt$Mon == 'Apr',4,
                          ifelse(temp.melt$Mon == 'May',5,
                          ifelse(temp.melt$Mon == 'Jun',6,
                          ifelse(temp.melt$Mon == 'Jul',7,
                          ifelse(temp.melt$Mon == 'Aug',8,
                          ifelse(temp.melt$Mon == 'Sep',9,
                          ifelse(temp.melt$Mon == 'Oct',10,
                          ifelse(temp.melt$Mon == 'Nov',11,12))))))))))))
temp.melt <- temp.melt[,c(1,7:8,2:6)]

names(temp.melt) <- c('Year','Month','MonTemp','AnnTemp','WinTemp','SprTemp','SumTemp','FallTemp')
env <- merge(env,temp.melt,by.x=c('YEAR','month'),by.y=c('Year','Month'))

# More Salinity Data
sal <- read.csv("p:/obrien/menhaden/pier analysis/pier data/cbl_salinity_1938-2012.csv")
sal.melt <- melt(sal,id=c("Year",'Annual','Winter..J.M.','Spring..A.J.',
                            'Summer..J.S.','Fall..O.D.'))
names(sal.melt)[7] <- 'Mon'
names(sal.melt)[8] <- 'Monthly'
sal.melt$Mon <- as.factor(ifelse(sal.melt$Mon == 'Jan',1,
                          ifelse(sal.melt$Mon == 'Feb',2,
                          ifelse(sal.melt$Mon == 'Mar',3,
                          ifelse(sal.melt$Mon == 'Apr',4,
                          ifelse(sal.melt$Mon == 'May',5,
                          ifelse(sal.melt$Mon == 'Jun',6,
                          ifelse(sal.melt$Mon == 'Jul',7,
                          ifelse(sal.melt$Mon == 'Aug',8,
                          ifelse(sal.melt$Mon == 'Sep',9,
                          ifelse(sal.melt$Mon == 'Oct',10,
                          ifelse(sal.melt$Mon == 'Nov',11,12))))))))))))
sal.melt <- sal.melt[,c(1,7:8,2:6)]
names(sal.melt) <- c('Year','Month','MonSal','AnnSal','WinSal','SprSal','SumSal','FallSal')
env <- merge(env,sal.melt,by.x=c('YEAR','month'),by.y=c('Year','Month'))

env <- env[order(env$DATE),]
row.names(env) <- env$DATE
env <- env[,c(1:2,4:dim(env)[2])]
# remove excess from  workspace
rm(flow,pier,pierF,wind,cpue,include,include2,j,presence,mei,mei.melt,nao,temp,temp.melt,sal,sal.melt,rot.axis)
