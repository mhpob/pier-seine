library(ggplot2)
fish$date <- as.Date(row.names(fish))
fish$yr<-env$YEAR
fish$wk <- env$WEEK
wp0 <- ggplot(fish)+geom_point(aes(x=wk,y=wp.yoy)) +scale_y_log10()+ facet_wrap(~yr)
j

wp1 <- ggplot(fish)+geom_point(aes(x=wk,y=wp.yrl)) +scale_y_log10()
wp1

par(mfrow=c(2,3),mar=c(2,0.2,1.7,0.2),oma=c(1,5,0,0), font = 1)
plot(env$date,fish$wp.yoy,log ="y", xlab="",ylab="") +
  mtext(side = 3, "Year")
plot(env$date,fish$wp.yrl,log='y')


fish$wk <- env$WEEK
fish$season <- ifelse(fish$wk <= 26,'spr',
                      ifelse(fish$wk >= 27 & fish$wk <= 35,'sum','fall'))
fish$yr<-env$YEAR

test<-aggregate(fish[,c(1:23)],list(yr=fish$yr,season=fish$season),mean)

spring <- test[test$season == 'spr',]
summer <- test[test$season == 'sum',]
fall<-test[test$season == 'fall',]
#wp
plot(spring$wp.yoy,summer$wp.yoy)
plot(summer$wp.yoy,fall$wp.yoy)
plot(spring$wp.yrl,summer$wp.yrl)
plot(summer$wp.yrl,fall$wp.yrl)
#sb
plot(spring$sb.yoy,summer$sb.yoy)
plot(summer$sb.yoy,fall$sb.yoy)
plot(spring$sb.yrl,summer$sb.yrl)
plot(summer$sb.yrl,fall$sb.yrl)
#men
plot(spring$men.yoy,summer$men.yoy)
plot(summer$men.yoy,fall$men.yoy)
plot(spring$men.yrl,summer$men.yrl)
plot(summer$men.yrl,fall$men.yrl)



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
spring<-merge(spring,temp.melt[,c(1,5,6)],by.x='yr',by.y='Year',all.y=F)
summer<-merge(summer,temp.melt[,c(1,5,6)],by.x='yr',by.y='Year',all.y=F)
fall<-merge(fall,temp.melt[,c(1,5,6)],by.x='yr',by.y='Year',all.y=F)

plot(summer$WinTemp,spring$sb.yoy)