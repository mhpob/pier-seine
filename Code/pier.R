### Data input and ordering ----
setwd("p:/obrien/menhaden/pier analysis")
library(vegan);library(reshape2)
source('pier.data.agg.R')

# Transform assemblage data (natural log + 1)
fish <- log(fish + 1)


# Copy menhaden catch over to environmental (explanatory) variables
env$men <- fish$men


env <- env[,c(1,3:dim(env)[2])]
#env[,3:dim(env)[2]] <- scale(env[,3:dim(env)[2]], center = T, scale = T) #normalize env vars
#fish <- fish[,!names(fish) %in% c("men")] #remove menhaden from assemblage data

### CCA ----
## Move on to CCA using "VEGAN" package. For any help here, Google "vegan tutor".
#fish1 <- cca(fish~Condition(YEAR)+Condition(WEEK)+flowspring+flowwinter+flowsummer+SprTemp+WinTemp+SumTemp+WinSal+SprSal+SumSal+windwinter+windspring,env)
#fish0 <- cca(fish~Condition(YEAR) + Condition(WEEK),env)
#fish.step <- step(fish0,scope=list(lower=formula(fish0),upper=formula(fish1)),test='perm', trace=F)
fish.step <- cca(fish ~ Condition(YEAR) + Condition(WEEK) + flowsummer + WinTemp + WinSal + windspring + SumTemp + flowwinter, env)

plot(fish.step, dis = "sp", type = "t") # "sp" refers to "species"
points(fish.step, dis = "sites", type = "p", col = "grey", pch=20, cex=.8)
points(fish.step, dis = "bp") # "bp" refers to "biplot"
text(fish.step,dis="cn") # "cn" refers to "canonical names"


fish.step
anova(fish.step)
anova(fish.step, by = "term", permu = 200)
anova(fish.step, by = "mar")
anova(fish.step, by = "axis", perm = 500)

## Species projections onto environmental biplot arrows
j<-summary(fish.step)
k<-as.data.frame(j$species[,1:2])
m<-as.data.frame(j$biplot[,1:2])
spe.proj <- data.frame(cbind(sqrt(k[,1]^2+k[,2]^2)*cos(atan2(k[,1],k[,2])-atan2(m[1,1],m[1,2])),
                             sqrt(k[,1]^2+k[,2]^2)*cos(atan2(k[,1],k[,2])-atan2(m[2,1],m[2,2])),
                             sqrt(k[,1]^2+k[,2]^2)*cos(atan2(k[,1],k[,2])-atan2(m[3,1],m[3,2])),
                             sqrt(k[,1]^2+k[,2]^2)*cos(atan2(k[,1],k[,2])-atan2(m[4,1],m[4,2])),
                             sqrt(k[,1]^2+k[,2]^2)*cos(atan2(k[,1],k[,2])-atan2(m[5,1],m[5,2])),
                             sqrt(k[,1]^2+k[,2]^2)*cos(atan2(k[,1],k[,2])-atan2(m[6,1],m[6,2]))),
                       row.names = row.names(k))
names(spe.proj) <- row.names(m)
rm(j,m,k)

### nmMDS ----
## Move on to nmMDS using "VEGAN" package. For any help, Google "vegan tutor" or "vegan intro".
fish.mds <- metaMDS(fish, trace = F)
plot(fish.mds, dis = "sp", type = "t")
points(fish.mds, dis = "sites", type = "p")

## Env var fit
ef<-envfit(fish.mds ~ ., env, per = 999)
ef
plot(ef)
#ordisurf(fish.mds,env$SAL,add = T, col = "blue", labcex = .8)

### Various plots ----
# Menhaden catch in various years
library(ggplot2)
env$men.yoy<-fish$men.yoy #do this before species data is transformed
j <- ggplot(env)+geom_point(aes(x=WEEK,y=men.yoy)) +scale_y_log10()+ facet_wrap(~YEAR)
j <- j + xlab("Week of Year") + ylab("Menhaden Catch (log-10)")
print(j)

#Cumulative dominance curve
# **********Run this before species are transformed************
dom <- colSums(fish)/sum(colSums(fish))
dom <-as.data.frame(dom)
dom$rank <-rank(-dom)
dom <- dom[order(dom$rank),]
dom$cumu <- cumsum(dom$dom)
plot(dom$rank,dom$cumu,log="x", ylim=c(0,1))
text(dom$rank,dom$cumu,labels = row.names(dom),pos=1,offset=0.5,cex=0.7)
#lines(dom$rank,dom$cumu)

# Percent Incidence
inc <- colSums(fish > 0)/dim(fish)[1]
inc <- as.data.frame(inc)
inc$rank <- rank(-inc)
inc <- inc[order(inc$rank),]
plot(inc$rank,inc$inc)
text(inc$rank,inc$inc,labels = row.names(inc),pos=1,offset=0.5,cex=0.7)
#lines(inc$rank,inc$inc)


# Env. plots vs menhaden catch
par(mfrow=c(2,3),mar=c(2,0.2,1.7,0.2),oma=c(1,5,0,0), font = 1)
plot(env$DATE,fish$wp.yoy,log ="y", xlab="",ylab="") +
  mtext(side = 3, "Year")
plot(env$WEEK,env$men, yaxt = "n", xlab="",ylab="",log ="y") + 
  mtext(side = 3, "Week")
plot(env$TEMP,env$men, yaxt = "n", xlab="",ylab="",log ="y") + 
  mtext(side = 3, "Temperature (C)")
plot(env$SAL,env$men,log ="y", xlab="",ylab="")+ 
  mtext(side = 3, "Salinity")
plot(env$SPFLOW,env$men, yaxt = "n", xlab="",ylab="",log ="y") + 
  mtext(side = 3, "Spring Flow (m^3/s)")
plot(env$WINFLOW,env$men, yaxt = "n", xlab="",ylab="",log ="y") + 
  mtext(side = 3, "Winter Flow (m^3/s)")
title(ylab="Menhaden Catch (log-10)",outer = T)