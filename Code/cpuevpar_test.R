
setwd("p:/obrien/menhaden/pier analysis")
pier <- read.table("assemblage.txt", header = T, na.strings = "NA")
pier$DATE <- strptime(pier$DATE, format = "%m/%d/%Y")


pierF <- cbind(pier[pier$CPUE=="FALSE",1:9], pier[pier$CPUE == "FALSE",10:dim(pier)[2]]/4.44)

weighted <- rbind(pierF,pier[pier$CPUE == "TRUE",])

pier$silvers <- pier$atlsilvers + pier$inlsilvers
pier <- pier[,!names(pier) %in%
  c("atlsilvers","inlsilvers","men1.","wpadult",
    "sbadult","sflounderadult","blueadult","croakadult")]

### T Test ----
pier <- pier[order(pier$DATE),]
dates <- pier[duplicated(pier$DATE),]
dates <- pier[pier$DATE %in% dates$DATE,]
dates <- dates[order(dates$DATE),]


fish <- dates[,10:dim(dates)[2]]
fish <- aggregate(fish,list(DATE=format(dates$DATE, "%Y-%m-%d")),mean)
row.names(fish) <- fish$DATE
fish <- fish[,2:dim(fish)[2]]
fish$sum <- rowSums(fish)
fish$spe <- rowSums(apply(fish[,1:48],2,">",0))

t<-pier[duplicated(pier$DATE),]
tt <- pier[!pier$DATE %in% t$DATE,]
piert <- tt[tt$CPUE == "TRUE",]
piert <- piert[,10:57]
piert$sum <- rowSums(piert)
piert$spe <- rowSums(apply(piert[,1:48],2,">",0))

t.test(fish$sum,piert$sum)
t.test(fish$spe,piert$spe)

### CA Test ----
pier <- read.table("assemblage.txt", header = T, na.strings = "NA")
pier$DATE <- strptime(pier$DATE, format = "%m/%d/%Y")

## Remove weeks where only 1 haul was conducted
dup<-pier[duplicated(pier$DATE),] #find dup dates
nondup <- pier[!pier$DATE %in% dup$DATE,] #find non-duplicated dates
dup <- pier[!pier$DATE %in% nondup$DATE,]  #choose all dup dates
ttt <- dup[dup$CPUE == "TRUE",] #dates with duplicate hauls in which at least 1 is cpue
dup <- pier[pier$DATE %in% ttt$DATE,]


# Weighted weeks
wt <- cbind(dup[dup$CPUE=="FALSE",1:9], dup[dup$CPUE == "FALSE",10:dim(dup)[2]]/4.44)

wt <- rbind(wt,dup[dup$CPUE == "TRUE",])

wt$silvers <- wt$atlsilvers + wt$inlsilvers
wt <- wt[,!names(wt) %in%
  c("atlsilvers","inlsilvers","men1.","wpadult",
    "sbadult","sflounderadult","blueadult","croakadult")]

## Take Means
wfish <- wt[,10:dim(wt)[2]]
wfish <- aggregate(wfish,list(DATE=format(wt$DATE, "%Y-%m-%d")),mean)
row.names(wfish) <- wfish$DATE
wfish <- wfish[,2:dim(wfish)[2]]

## Inclusion rule
include <- function (x) {
  cpue <- colSums(x)/dim(x)[1]
  include <- cpue >= 0.03
  presence <- colSums(x > 0)/dim(x)[1]
  include2 <- presence >= 0.03
  x <- x[,(include | include2 == T)]
}

wfish <- include(wfish)

## CPUE only
cp <- dup[dup$CPUE == "TRUE",]

cp$silvers <- cp$atlsilvers + cp$inlsilvers
cp <- cp[,!names(cp) %in%
  c("atlsilvers","inlsilvers","men1.","wpadult",
    "sbadult","sflounderadult","blueadult","croakadult")]


cpfish <- cp[,10:dim(cp)[2]]
cpfish <- aggregate(cpfish,list(DATE=format(cp$DATE, "%Y-%m-%d")),mean)
row.names(cpfish) <- cpfish$DATE
cpfish <- cpfish[,2:dim(cpfish)[2]]

cpfish <-include(cpfish)


rm(dup,nondup,ttt,cp,wt,pier)

#species in both cpue and weighted
cpfish<-cpfish[,names(cpfish) %in% c(names(wfish))]
wfish<-wfish[,names(wfish) %in% c(names(cpfish))]
names(cpfish) <- paste(names(cpfish),"cp",sep=".")
names(wfish) <- paste(names(wfish),"wt",sep=".")
fish <- cbind(wfish,cpfish)

fish <- log(fish +1)

# fish.ca <- cca(fish)
# fish.ca
# plot(fish.ca, dis = "sp", type = "t")
# fish.pca <- rda(fish)
# fish.pca
# plot(fish.pca, dis = "sp", type = "t")
fish.dca <- decorana(fish)
plot(fish.dca, dis = "sp", type = "t")

scores <- as.data.frame(scores(fish.dca, display = "sp"))
scores$spe <- sapply(strsplit(rownames(scores),'\\.'), "[", 1)
scores$type <- sapply(strsplit(rownames(scores),'\\.'), "[", 2)

split <- function (x) {
  t <- data.frame(species = scores$spe, type = scores$type, scores = scores[,x])
  t$cp <- ifelse(t$type == "cp", t$scores,0)
  t$wt <- ifelse(t$type == "wt", t$scores,0)
  t <- aggregate(t[,4:5],list(t$species),sum)
  print(cor.test(t$cp,t$wt))
  plot(t$cp,t$wt, xlab = "CPUE only", ylab = "Weighted", main = paste("DCA",x))
  abline(a=0, b=1)
}

split(1)
split(2)
split(3)
split(4)