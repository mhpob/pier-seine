
library(ggplot2); library(mgcv); library(gratia); library(data.table); library(plotly) ;
library(tibble); library(readr);library(lubridate); library(janitor); library(anytime) ; library(tidyverse)

#Temperature
temp <- fread('C:/Users/benba/Documents/GitHub/pier-seine/Data/CBL_pier_temp_2003_2021.csv')
temp <- temp[-c(1), ] 
temp <- janitor::row_to_names(temp,row_number =1 )
names(temp)[names(temp) == 'Value (degrees Celcius)'] <- 'temp'   
names(temp)[names(temp) == 'Timestamp (UTC-05:00)'] <- 'Date'

temp$Date <-anytime::anytime(temp$Date)
temp$temp <- as.numeric(temp$temp)
temp <- na.omit(temp)

          
        dailytemp <-  temp %>%
            mutate(date = date(Date)) %>%
            group_by(date) %>%
            summarise(temp = median(temp))
#Noaa TEMp
temp2 <- fread('C:/Users/benba/Documents/GitHub/pier-seine/Data/NOAA Temp Data.csv')
names(temp2)[names(temp2) == 'degC'] <- 'temp'   
temp2$Date <-anytime::anytime(temp2$Date)

dailytemp2 <-  temp2 %>%
  mutate(date = date(Date)) %>%
  group_by(date) %>%
  summarise(temp = median(temp))


dailytemp <- merge(dailytemp, dailytemp2, by="date", all.x = TRUE, all.y = TRUE)
dailytemp$temp.x <- ifelse(is.na(dailytemp$temp.x), dailytemp$temp.y, dailytemp$temp.x)
names(dailytemp)[names(dailytemp) == 'temp.x'] <- 'temp'
dailytemp = subset(dailytemp, select = -c(temp.y) )

#Salinity
        
sal <- fread('C:/Users/benba/Documents/GitHub/pier-seine/Data/CBL_pier_salinity_2003_2021.csv')
sal <- sal[-c(1), ] 
sal <- janitor::row_to_names(sal,row_number =1 )
names(sal)[names(sal) == 'Value (Practical Salinity Units)'] <- 'sal'   
names(sal)[names(sal) == 'Timestamp (UTC-05:00)'] <- 'Date'

sal$Date <-anytime::anytime(sal$Date)
sal$sal <- as.numeric(sal$sal)
sal <- na.omit(sal)


dailysal <-  sal %>%
  mutate(date = date(Date)) %>%
  group_by(date) %>%
  summarise(sal = median(sal))



#Dissolved Oxygen

DO <- fread('C:/Users/benba/Documents/GitHub/pier-seine/Data/CBL_pier_DO_2003_2021.csv')
DO <- DO[-c(1), ] 
DO <- janitor::row_to_names(DO,row_number =1 )
names(DO)[names(DO) == 'Value (milligrams/liter)'] <- 'DO'   
names(DO)[names(DO) == 'Timestamp (UTC-05:00)'] <- 'Date'

DO$Date <-anytime::anytime(DO$Date)
DO$DO <- as.numeric(DO$DO)
DO <- na.omit(DO)

#DO2 <- DO[,median(DO),Date]

dailyDO <-  DO %>%
  mutate(date = date(Date)) %>%
  group_by(date) %>%
  summarise(DO = median(DO))


#dailyDO <- separate(dailyDO, date, into = c('year', 'month', 'day'), remove = FALSE)



#pH

pH <- fread('C:/Users/benba/Documents/GitHub/pier-seine/Data/CBL_pier_pH_2003_2021.csv')
pH <- pH[-c(1), ] 
pH <- janitor::row_to_names(pH,row_number =1 )
names(pH)[names(pH) == 'Value (NBS Scale)'] <- 'pH'   
names(pH)[names(pH) == 'Timestamp (UTC-05:00)'] <- 'Date'

pH$Date <-anytime::anytime(pH$Date)
pH$pH <- as.numeric(pH$pH)
pH <- na.omit(pH)

dailypH <-  pH %>%
  mutate(date = date(Date)) %>%
  group_by(date) %>%
  summarise(pH = median(pH))



#NAO

NAO <- fread('C:/Users/benba/Documents/GitHub/pier-seine/Data/NAO 2021.csv')
NAO <- NAO[-c(1), ] 
NAO <- janitor::row_to_names(NAO,row_number =1 )
names(NAO)[names(NAO) == 'Value'] <- 'NAO'
names(NAO)[names(NAO) == 'Date'] <- 'date'
NAO$date <-anytime::anydate(NAO$date)
NAO$NAO <- as.numeric(NAO$NAO)
#dailyNAO <- separate(NAO, date, into = c('year', 'month', 'day'), remove = FALSE)
NAO <- NAO[-c(1:588), ]

#Combine Data Sets
edata <- merge(dailyDO, dailytemp, by="date", all.x = TRUE, all.y = TRUE)
edata <- merge(edata, dailysal, by="date", all.x = TRUE, all.y = TRUE)
edata <- merge(edata, dailypH, by="date", all.x = TRUE, all.y = TRUE)

#edata <- merge(edata, NAO, by="date", all.x = TRUE, all.y = TRUE)


lengths <- fread('C:/Users/benba/Documents/GitHub/pier-seine/Data/derived/lengths2.csv')
lengths$date <- anytime::anydate(lengths$date)
alldata <- merge(lengths, edata, by="date", all.x = TRUE, all.y = TRUE)


#merge with lengths

alldata$temp.x <- ifelse(is.na(alldata$temp.x), alldata$temp.y, alldata$temp.x)
alldata$sal.x <- ifelse(is.na(alldata$sal.x), alldata$sal.y, alldata$sal.x)
alldata$temp.x <- signif(alldata$temp.x,digits=3)
alldata$sal.x <- signif(alldata$sal.x,digits=3)
alldata$DO <- signif(alldata$DO,digits=3)
alldata$pH <- signif(alldata$pH,digits=3)

alldata <- alldata[!is.na(alldata$length),]
names(alldata)[names(alldata) == 'temp.x'] <- 'temp'
names(alldata)[names(alldata) == 'sal.x'] <- 'sal'

outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}
outlierReplace(alldata, "temp", which(alldata$temp > 199), NA)
outlierReplace(alldata, "temp", which(alldata$temp < .1), NA)
fwrite(alldata, 'data/derived/lengths-edata.csv')



#checking for gaps



h1<-ggplot(data=alldata, 
           aes(x=date, y=temp), #order = reorder(temp, -temp), 
           text = paste("Temp:", temp, "Date:", date), show.legend=T)+
  geom_point(stat="identity", aes(color = temp))+ geom_line(size=.3) + scale_color_gradient(low="blue", high="red")      
     
ggplotly(h1)  

outlierReplace(alldata, "sal", which(alldata$sal < 1), NA)                

h2<-ggplot(data=alldata, 
           aes(x=date, y=sal), #order = reorder(temp, -temp), 
           text = paste("Temp:", temp, "Date:", date), show.legend=T)+
  geom_point(stat="identity", aes(color = sal))+ geom_line(size=.3) + scale_color_gradient(low="blue", high="goldenrod2")      

ggplotly(h2)  
