
library(ggplot2); library(mgcv); library(gratia); library(data.table); library(plotly) ;
library(tibble); library(readr);library(lubridate); library(janitor); library(anytime) 
temp <- fread('C:/Users/ben/Documents/GitHub/pier-seine/Data/CBL_pier_temp_2003_2021.csv')
temp <- temp[-c(1), ] 
temp <- janitor::row_to_names(temp,row_number =1 )
names(temp)[names(temp) == 'Value (degrees Celcius)'] <- 'Temp_C'   
names(temp)[names(temp) == 'Timestamp (UTC-05:00)'] <- 'Date'

temp$Date <-anytime::anytime(temp$Date)
temp$Temp_C <- as.numeric(temp$Temp_C)

          temp %>%
            mutate(date = floor_date(Date)) %>%
            group_by(date) %>%
            summarise(mean_Temp_C = mean(Temp_C))
          
        dailytemp <-  temp %>%
            mutate(date = date(Date)) %>%
            group_by(date) %>%
            summarise(mean_Temp_C = mean(Temp_C))
