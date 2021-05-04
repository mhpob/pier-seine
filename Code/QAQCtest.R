library(ggplot2); library(mgcv); library(gratia); library(data.table)
lengths <- fread('data/derived/lengths (QAQC).csv')
lengths[, yr_fac := as.factor(year)]
lengths[, record_num := as.factor(record_num)]

head(lengths)

inland <- lengths[grepl('beryllina', scientific)]
inland[, wk := week(date)]
inland[, yr_fac := as.factor(year)]



mod <- gam(list(length ~ s(wk, by = yr_fac) + yr_fac,
                ~ s(wk, k = 15)),
           data = inland, 
           family = 'gaulss')
plot(mod, scale = 0, shade = T, pages = 1)

summary(lengths)

