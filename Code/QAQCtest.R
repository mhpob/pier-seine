library(ggplot2); library(mgcv); library(gratia); library(data.table); library(plotly)
lengths <- fread('data/derived/lengths (QAQC).csv')
lengths[, yr_fac := as.factor(year)]
lengths[, record_num := as.factor(record_num)]

head(lengths)

#Checking to see for any repeat/Misspelled Names
unique(lengths$scientific)

#Outlier Discovery Mission
#Atlantic Silverside
a.ss <- lengths[grepl('menidia menidia', scientific)]

p1 <- ggplot(data = a.ss) + 
  geom_histogram(aes(x = length))

ggplotly(p1)

p2 <- ggplot(data = a.ss) + 
  geom_point(aes(x = length, y = 1, color = record_num), show.legend = FALSE)
 
ggplotly(p2)
 # A.ss Outliers:(Record #, Size)
  #(1721, 703) (1733, 706)
  #MAybe (3067, 199) Check LH Book, "No bigger than 153mm"

#Atlantic needlefish
need <- lengths[grepl('strongylura', scientific)]

p3 <- ggplot(data = need) + 
  geom_histogram(aes(x = length))

ggplotly(p3)

p4 <- ggplot(data = need) + 
  geom_point(aes(x = length, y = 1, color = record_num), show.legend = FALSE)

ggplotly(p4)
#Max length Needlefish : 1110 mm TL, expected at sampling period ?

