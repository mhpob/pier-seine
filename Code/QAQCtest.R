library(ggplot2); library(mgcv); library(gratia); library(data.table); library(plotly) ;library(dplyr)
lengths <- fread('data/derived/lengths.csv')
lengths[, yr_fac := as.factor(year)]
lengths[, record_num := as.factor(record_num)]
lengths[, wk := week(date)]

head(lengths)

#Checking to see for any repeat/Misspelled Names for top 11 fish
unique(lengths$scientific)

  # correction

lengths$scientific <- gsub('Menidia mendia', 'Menidia menidia', lengths$scientific)
lengths$scientific <- gsub('Menidia menida', 'Menidia menidia', lengths$scientific)
lengths$scientific <- gsub('Brevoortia tyrannus_2', 'Brevoortia tyrannus', lengths$scientific)
lengths$scientific <- gsub('Dorosoma petense', 'Dorosoma petenense', lengths$scientific)
lengths$scientific <- gsub('Membras beryllilna', 'Menidia beryllina', lengths$scientific)
lengths$scientific <- gsub('Stronglyura marina', 'Strongylura marina', lengths$scientific)
lengths$scientific <- gsub('Strongyluta marina', 'Strongylura marina', lengths$scientific)
lengths$scientific <- gsub('Fundulus heterclitus', 'Fundulus heteroclitus', lengths$scientific)
lengths$scientific <- gsub('HypsoblenniusÂ hentzÂ', 'Hypsoblennius hentz', lengths$scientific)
lengths$scientific <- gsub('Pogonias chromis', 'Pogonias cromis', lengths$scientific)
lengths$scientific <- gsub('Striped bass', 'Morone saxatilis', lengths$scientific)
lengths$scientific <- gsub('Sygnathus fuscus', 'Syngnathus fuscus', lengths$scientific)
lengths$scientific <- gsub('Morone Saxatilis', 'Morone saxatilis', lengths$scientific)
lengths$scientific <- gsub('Brevoortia Tyrannus', 'Brevoortia tyrannus', lengths$scientific)

unique(lengths$scientific)
fwrite(lengths, 'data/derived/lengths(QAQC2).csv')

#Outlier Discovery Mission

##Atlantic Silverside
a.ss <- lengths[grepl('Menidia menidia', scientific)]

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
ggplotly(b1)
  
##Atlantic needlefish
need <- lengths[grepl('Strongylura', scientific)]

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
ggplotly(p7)
ggplotly(p8)
ggplotly(b4)
gg_qq(spot$length)

###inland silverside###
inland <- lengths[grepl('beryllina', scientific)]

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
ggplotly(p9)
ggplotly(p10)
ggplotly(b5)
gg_qq(inland$length)

##striped bass##

bass <- lengths[grepl('saxatilis', scientific)]

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
ggplotly(p13)
ggplotly(p14)
ggplotly(b7)
gg_qq(blue$length)

##bay anchovy##

ancho <- lengths[grepl('mitchilli', scientific)]

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
ggplotly(p15)
ggplotly(p16)
ggplotly(b8)
gg_qq(ancho$length)
##menhaden##

men <- lengths[grepl('Brevoortia tyrannus', scientific)]

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
