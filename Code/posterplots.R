## Incidence Bar
names(inc)
barplot(inc$inc,names=row.names(inc))

## Catgories for Env Bars
test<-env
test$tempcat <- ifelse(test$WinTemp < 4,"<4",ifelse(test$WinTemp >4 & test$WinTemp < 5,"4-5",">5"))
test$flowcat <- ifelse(test$flowsummer < 10, "<10",ifelse(test$flowsummer > 10 & test$flowsummer < 14, "10-14",">14"))

## PCA (vegan)
t2<- env[,c(6:8,17:20,23:26,29:32)]
pca <- rda(t2)
biplot(pca,scaling = -1)

## PCA (base)
model <- prcomp(t2,scale=T)
biplot(model)

## Put months back into data set
j <- strptime(row.names(env),format = "%Y-%m-%d")
env$month <- j$mon+1

##Make an ifelse() to get seasons (check pier.data.agg)
env$season <-ifelse(env$month>=1 & env$month <=3, "winter", 
                         ifelse(env$month>=4 & env$month <=6, "spring",
                                ifelse(env$month>=7 & env$month<=9, "summer", "fall")))
##Select one value for each year and season (env2)
env$seasonyear<-paste(env$YEAR,env$season)
env2<-env[!duplicated(env$seasonyear),]

##PCA with one value from each year and season (PCA of env2)
t2<- env2[,c(6:8,17:20,23:26,29:32)]
pca <- rda(t2)
biplot(pca,scaling = -1)

##Species association lines (Y value is 0)
spe.proj$Zero<-0
plot(spe.proj$flowsummer,spe.proj$zero)
axis(1,at=spe.proj$flowsummer,labels=row.names(spe.proj))

plot(spe.proj$WinTemp,spe.proj$zero)
axis(1,at=spe.proj$WinTemp,labels=row.names(spe.proj))

plot(spe.proj$WinSal,spe.proj$zero)
axis(1,at=spe.proj$WinSal,labels=row.names(spe.proj))

plot(spe.proj$windspring,spe.proj$zero)
axis(1,at=spe.proj$windspring,labels=row.names(spe.proj))

plot(spe.proj$SumTemp,spe.proj$zero)
axis(1,at=spe.proj$SumTemp,labels=row.names(spe.proj))

plot(spe.proj$flowwinter,spe.proj$zero)
axis(1,at=spe.proj$flowwinter,labels=row.names(spe.proj))

##Switch x and y axis
spe.proj$Zero<-0
plot(spe.proj$zero,spe.proj$flowsummer)
axis(2,at=spe.proj$flowsummer,labels=spe.proj$row.names)

