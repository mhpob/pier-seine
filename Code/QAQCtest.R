library(ggplot2); library(mgcv); library(gratia); library(data.table); library(plotly) ;library(dplyr)
lengths <- fread('data/derived/lengths2.csv')
lengths[, yr_fac := as.factor(year)]
lengths[, record_num := as.factor(record_num)]
lengths[, wk := week(date)]
vec <- c(1)
lengths$counts <- vec


#Checking to see for any repeat/Misspelled Names for top 11 fish
unique(lengths$scientific)

  # correction

lengths$scientific <- gsub('menidia mendia', 'menidia menidia', lengths$scientific)
lengths$scientific <- gsub('menidia menida', 'menidia menidia', lengths$scientific)
lengths$scientific <- gsub('brevoortia tyrannus_2', 'brevoortia tyrannus', lengths$scientific)
lengths$scientific <- gsub('dorosoma petense', 'dorosoma petenense', lengths$scientific)
lengths$scientific <- gsub('membras beryllilna', 'menidia beryllina', lengths$scientific)
lengths$scientific <- gsub('stronglyura marina', 'strongylura marina', lengths$scientific)
lengths$scientific <- gsub('strongyluta marina', 'strongylura marina', lengths$scientific)
lengths$scientific <- gsub('Fundulus heterclitus', 'sundulus heteroclitus', lengths$scientific)
lengths$scientific <- gsub('hypsoblenniusÂ hentzÂ ', 'hypsoblennius hentz', lengths$scientific)
lengths$scientific <- gsub('pogonias chromis', 'pogonias cromis', lengths$scientific)
lengths$scientific <- gsub('striped bass', 'morone saxatilis', lengths$scientific)
lengths$scientific <- gsub('sygnathus fuscus', 'syngnathus fuscus', lengths$scientific)
lengths$scientific <- gsub('morone Saxatilis', 'morone saxatilis', lengths$scientific)
lengths$scientific <- gsub('brevoortia Tyrannus', 'brevoortia tyrannus', lengths$scientific)

unique(lengths$scientific)
 fwrite(lengths, 'data/derived/lengths(QAQC2).csv')

#Outlier Discovery Mission

##Atlantic Silverside
a.ss <- lengths[grepl('Menidia menidia', scientific)]

h1<-ggplot(data=a.ss, aes(x=wk, y=counts, color = length), show.legend=FALSE) +
  geom_bar(stat="identity")+ ggtitle("Length by week") 

ggplotly(h1)

p1 <- ggplot(data = a.ss) + 
  geom_histogram(aes(x = length))

ggplotly(p1)

p2 <- ggplot(data = a.ss) + 
  geom_point(aes(x = length, y = 1, color = record_num), show.legend=FALSE)

ggplotly(p2)
 
b1 <- ggplot(data = a.ss, ) +
  geom_boxplot(aes(x = "", y = length, ), show.legend=FALSE) + coord_flip()

ggplotly(b1)

#Outlier Function
outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}

# A.ss Outliers:(Record #, Size)
  #(1721, 703) (1733, 706)
  #MAybe (3067, 199) Check LH Book, "No bigger than max lengths of 180mm" 
  #YOY max length 80mm suggested by books


  #QQ Plot Function
  gg_qq <- function(x, distribution = "norm", ..., line.estimate = NULL, conf = 0.95,
                    labels = names(x)){
    q.function <- eval(parse(text = paste0("q", distribution)))
    d.function <- eval(parse(text = paste0("d", distribution)))
    x <- na.omit(x)
    ord <- order(x)
    n <- length(x)
    P <- ppoints(length(x))
    df <- data.frame(ord.x = x[ord], z = q.function(P, ...))
    
    if(is.null(line.estimate)){
      Q.x <- quantile(df$ord.x, c(0.25, 0.75))
      Q.z <- q.function(c(0.25, 0.75), ...)
      b <- diff(Q.x)/diff(Q.z)
      coef <- c(Q.x[1] - b * Q.z[1], b)
    } else {
      coef <- coef(line.estimate(ord.x ~ z))
    }
    
    zz <- qnorm(1 - (1 - conf)/2)
    SE <- (coef[2]/d.function(df$z)) * sqrt(P * (1 - P)/n)
    fit.value <- coef[1] + coef[2] * df$z
    df$upper <- fit.value + zz * SE
    df$lower <- fit.value - zz * SE
    
    if(!is.null(labels)){ 
      df$label <- ifelse(df$ord.x > df$upper | df$ord.x < df$lower, labels[ord],"")
    }
    
    p <- ggplot(df, aes(x=z, y=ord.x)) +
      geom_point() + 
      geom_abline(intercept = coef[1], slope = coef[2]) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.2) 
    if(!is.null(labels)) p <- p + geom_text( aes(label = label))
    print(p)
    coef
  }  
  
gg_qq(a.ss$length)

IQR(a.ss$length)

outlierReplace(a.ss, "length", which(a.ss$length > 180), NA)  
ggplotly(h1)
ggplotly(p1)
ggplotly(p2)
ggplotly(b1)  
gg_qq(a.ss$length)
##Atlantic needlefish
need <- lengths[grepl('Strongylura', scientific)]

h2<-ggplot(data=need, aes(x=wk, y=counts, color = length), show.legend=FALSE) +
  geom_bar(stat="identity")+ ggtitle("Length by week") 

ggplotly(h2)

p3 <- ggplot(data = need) + 
  geom_histogram(aes(x = length))

ggplotly(p3)

p4 <- ggplot(data = need) + 
  geom_point(aes(x = length, y = 1, color = record_num), show.legend = FALSE)

ggplotly(p4)

b2 <- ggplot(data = need, ) +
  geom_boxplot(aes(x = "", y = length, ), show.legend=FALSE) + coord_flip()

ggplotly(b2)

gg_qq(need$length)
#Max length Needlefish : 1110 mm TL, expected at sampling period ?


##Blue Crab
crab <- lengths[grepl('sapidus', scientific)]

h3<-ggplot(data=crab, aes(x=wk, y=counts, fill = length), show.legend=FALSE) +
  geom_bar(stat="identity")+ ggtitle("Length by week") 

ggplotly(h3)

p5 <- ggplot(data = crab) + 
  geom_histogram(aes(x = length))

ggplotly(p5)

p6 <- ggplot(data = crab) + 
  geom_point(aes(x = length, y = 1, color = record_num), show.legend = FALSE)

ggplotly(p6)

b3 <- ggplot(data = crab, ) +
  geom_boxplot(aes(x = "", y = length, ), show.legend=FALSE) + coord_flip()

ggplotly(b3)

gg_qq(crab$length)
#Spot
spot <- lengths[grepl('xanthurus', scientific)]

h4<-ggplot(data=spot, aes(x=wk, y=counts, color = length), show.legend=FALSE) +
  geom_bar(stat="identity")+ ggtitle("Length by week") 

ggplotly(h4)

p7 <- ggplot(data = spot) + 
  geom_histogram(aes(x = length))

ggplotly(p7)

p8 <- ggplot(data = spot) + 
  geom_point(aes(x = length, y = 1, color = record_num), show.legend = FALSE)

ggplotly(p8)

b4 <- ggplot(data = spot, ) +
  geom_boxplot(aes(x = "", y = length, ), show.legend=FALSE) + coord_flip()

ggplotly(b4)  

gg_qq(spot$length)

#Outliers:(Record #, Size)
        #(1651, 409) Max 360mm
outlierReplace(spot, "length", which(spot$length > 360), NA)  
ggplotly(h4)
ggplotly(p7)
ggplotly(p8)
ggplotly(b4)
gg_qq(spot$length)

###inland silverside###
inland <- lengths[grepl('beryllina', scientific)]

h5<-ggplot(data=inland, aes(x=wk, y=counts, color = length), show.legend=FALSE) +
  geom_bar(stat="identity")+ ggtitle("Length by week") 

ggplotly(h5)

p9 <- ggplot(data = inland) + 
  geom_histogram(aes(x = length))

ggplotly(p9)

p10 <- ggplot(data = inland) + 
  geom_point(aes(x = length, y = 1, color = record_num), show.legend = FALSE)

ggplotly(p10)

b5 <- ggplot(data = inland, ) +
  geom_boxplot(aes(x = "", y = length, ), show.legend=FALSE) + coord_flip()

ggplotly(b5)

gg_qq(inland$length)

#Outliers:(Record #, Size)
      #(1598, 700) Max 150mm

outlierReplace(inland, "length", which(inland$length > 150), NA)  
ggplotly(h5)
ggplotly(p9)
ggplotly(p10)
ggplotly(b5)
gg_qq(inland$length)

##striped bass##

bass <- lengths[grepl('saxatilis', scientific)]

h6<-ggplot(data=bass, aes(x=wk, y=counts, color = length), show.legend=FALSE) +
  geom_bar(stat="identity")+ ggtitle("Length by week") 

ggplotly(h6)

p11 <- ggplot(data = bass) + 
  geom_histogram(aes(x = length))

ggplotly(p11)

p12 <- ggplot(data = bass) + 
  geom_point(aes(x = length, y = 1, color = record_num), show.legend = FALSE)

ggplotly(p12)

b6 <- ggplot(data = bass, ) +
  geom_boxplot(aes(x = "", y = length, ), show.legend=FALSE) + coord_flip()

ggplotly(b6)

gg_qq(bass$length)

#bluefish#

blue <- lengths[grepl('saltatrix', scientific)]

h7<-ggplot(data=blue, aes(x=wk, y=counts, fill = length), show.legend=FALSE) +
  geom_bar(stat="identity")+ ggtitle("Length by week") 

ggplotly(h7)

p13 <- ggplot(data = blue) + 
  geom_histogram(aes(x = length))

ggplotly(p13)

p14 <- ggplot(data = blue) + 
  geom_point(aes(x = length, y = 1, color = record_num), show.legend = FALSE)

ggplotly(p14)
   
b7 <- ggplot(data = blue, ) +
  geom_boxplot(aes(x = "", y = length, ), show.legend=FALSE) + coord_flip()

ggplotly(b7)

gg_qq(blue$length)
#Outliers:(Record #, Size)
    #(1668, 700) Max 1130mm
outlierReplace(blue, "length", which(blue$length > 1130), NA)  
ggplotly(h7)
ggplotly(p13)
ggplotly(p14)
ggplotly(b7)
gg_qq(blue$length)

##bay anchovy##

ancho <- lengths[grepl('mitchilli', scientific)]

h8<-ggplot(data=ancho, aes(x=wk, y=counts, color = length), show.legend=FALSE) +
  geom_bar(stat="identity")+ ggtitle("Length by week") 

ggplotly(h8)

p15 <- ggplot(data = ancho) + 
  geom_histogram(aes(x = length))

ggplotly(p15)

p16 <- ggplot(data = ancho) + 
  geom_point(aes(x = length, y = 1, color = record_num), show.legend = FALSE)

ggplotly(p16)

b8 <- ggplot(data = ancho, ) +
  geom_boxplot(aes(x = "", y = length, ), show.legend=FALSE) + coord_flip()

ggplotly(b8)

gg_qq(ancho$length)
    #Outliers:(Record #, Size)
      #(3100, 186) Max 110mm
outlierReplace(ancho, "length", which(ancho$length > 110), NA)  
ggplotly(h8)
ggplotly(p15)
ggplotly(p16)
ggplotly(b8)
gg_qq(ancho$length)
##menhaden##

men <- lengths[grepl('Brevoortia tyrannus', scientific)]

h9<-ggplot(data=men, aes(x=wk, y=counts, color = length), show.legend=FALSE) +
  geom_bar(stat="identity")+ ggtitle("Length by week") 

ggplotly(h9)

p17 <- ggplot(data = men) + 
  geom_histogram(aes(x = length))

ggplotly(p17)

p18 <- ggplot(data = men) + 
  geom_point(aes(x = length, y = 1, color = record_num), show.legend = FALSE)

ggplotly(p18)

b9 <- ggplot(data = men, ) +
  geom_boxplot(aes(x = "", y = length, ), show.legend=FALSE) + coord_flip()

ggplotly(b9)

gg_qq(men$length)
 #max 500mm

##gizzard shad##

shad <- lengths[grepl('Dorosoma cepedianum', scientific)]

h10<-ggplot(data=shad, aes(x=wk, y=counts,  fill = length), show.legend=FALSE) +
  geom_bar(stat="identity")+ ggtitle("Length by week") 

ggplotly(h10)

p19 <- ggplot(data = shad) + 
  geom_histogram(aes(x = length))

ggplotly(p19)

p20 <- ggplot(data = shad) + 
  geom_point(aes(x = length, y = 1, color = record_num), show.legend = FALSE)

ggplotly(p20)

b10 <- ggplot(data = shad, ) +
  geom_boxplot(aes(x = "", y = length, ), show.legend=FALSE) + coord_flip()

ggplotly(b10)

gg_qq(shad$length)

#max 570mm

##white perch##

perch <- lengths[grepl('Morone americana', scientific)]

h11<-ggplot(data=perch, aes(x=wk, y=counts, fill = length), show.legend=FALSE) +
  geom_bar(stat="identity")+ ggtitle("Length by week") 

ggplotly(h11)

p21 <- ggplot(data = perch) + 
  geom_histogram(aes(x = length))

ggplotly(p21)

p22 <- ggplot(data = perch) + 
  geom_point(aes(x = length, y = 1, color = record_num), show.legend = FALSE)

ggplotly(p22)

b11 <- ggplot(data = perch, ) +
  geom_boxplot(aes(x = "", y = length, ), show.legend=FALSE) + coord_flip()

ggplotly(b11)

gg_qq(perch$length)
#max 580mm

#HypsoblenniusÂ hentzÂ
weird <- lengths[grepl('NA', scientific)]
p23 <- ggplot(data = weird) + 
  geom_point(aes(x = length, y = 1, color = record_num), show.legend = FALSE)

ggplotly(p23)
