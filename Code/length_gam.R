library(data.table); library(mgcv)

lengths <- fread('data/derived/lengths.csv')

inland <- lengths[grepl('beryllina', scientific)]
inland[, wk := week(date)]
inland[, yr_fac := as.factor(year)]



mod <- gam(list(length ~ s(wk, by = yr_fac) + yr_fac,
                ~ s(wk, k = 15)),
           data = inland, 
           family = 'gaulss')
plot(mod, scale = 0, shade = T, pages = 1)



atl <- k[grepl('menidia', SCIENTIFIC)]
atl[, wk := week(DATE)]
atl[, yr_fac := as.factor(YEAR)]


library(mgcv)

mod <- gam(list(length ~ yr_fac + s(wk, by = yr_fac, k = 15),
                ~ s(wk, k = 15)),
           data = atl, 
           family = 'gaulss')
plot(mod, scale = 0, shade = T, pages = 3)




silvs <- k[grepl('Menidia', SCIENTIFIC)]
silvs[, wk := week(DATE)]
silvs[, yr_fac := as.factor(YEAR)]

mod <- gam(list(length ~ 
                  s(wk, k = 15) +
                  s(yr_fac, bs = 're'),
                ~ s(wk)),
           data = silvs,
           family = 'gaulss')

summary(mod)
plot(mod, scale = 0, shade = T, pages = 1)


library(gratia)

k <- derivatives(mod)
