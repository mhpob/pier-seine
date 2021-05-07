library(data.table); library(gratia); library(mgcv)

lengths <- fread('data/derived/lengths.csv')

inland <- lengths[grepl('beryllina', scientific)]




mod <- gam(list(length ~ s(wk, k = 15, bs = 'cs') + 
                  s(year, bs = 'cs') +
                  s(temp, bs = 'cs') +
                  s(sal, bs = 'cs'),
                ~ s(wk, k = 15, bs = 'cs')),
           data = inland, 
           family = 'gaulss',
           control = list(nthreads = 16))
plot(mod, scale = 0, shade = T, pages = 1)

k <- derivatives(mod,
                 c('s(wk)', 's.1(wk)'))

k <- data.table(k)[smooth == 's.1(wk)', c('derivative', 'lower', 'upper') :=
                   lapply(.SD, function(.){
                     0.01 + exp(.)
                   }),
                   .SDcols = c('derivative', 'lower', 'upper')]


library(ggplot2)

ggplot(data = k, aes(x = data)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = 'lightgray') +
  geom_line(aes(y = derivative)) +
  facet_wrap(~ smooth, scales = 'free')









atl <- lengths[grepl('menidia', scientific)]
atl[, wk := week(date)]
atl[, yr_fac := as.factor(year)]




library(mgcv)

mod <- gam(list(length ~ s(wk, k = 15, bs = 'cs') +
                  s(year, k = 15, bs = 'cs'),
                ~ s(wk, k = 15, bs = 'cs')),
           data = atl, 
           family = 'gaulss')
plot(mod, scale = 0, shade = T, pages = 1)




silvs <- lengths[grepl('Menidia', scientific)]
silvs[, wk := week(date)]
silvs[, yr_fac := as.factor(year)]

mod <- gam(list(length ~ 
                  s(wk, k = 15) +
                  s(yr_fac, bs = 're'),
                ~ s(wk)),
           data = silvs,
           family = 'gaulss')

summary(mod)
plot(mod, scale = 0, shade = T, pages = 1)





wp <- lengths[grepl('americana', scientific)]

wp[, yr_fac := as.factor(year)]
wp[, wk := as.numeric(week(date))]
wp[, temp := as.numeric(temp)]
wp[, sal := as.numeric(`salinity (surface, ppt)`)]



mod <- gam(list(length ~ s(wk, k = 15) + 
                  s(yr_fac, bs = 're') +
                  s(temp) +
                  s(sal),
                ~ s(wk, k = 15)),
           data = wp, 
           family = 'gaulss',
           control = list(nthreads = 16))
plot(mod, scale = 0, shade = T, pages = 1)

mod <- gam(length ~ s(wk, by = yr_fac, k = 5) +
             yr_fac,
           data = wp)
library(gratia)
k <- derivatives(mod)

library(ggplot2)
ggplot(data = d) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = data), fill = 'lightgray') +
  geom_line(aes(x = data, y = derivative)) +
  facet_wrap(~smooth)
plot(mod)



set.seed(1)
dat <- gamSim(4, n = 400, verbose = FALSE)
m <- gam(y ~ s(fac, bs = 're') + s(x2), data = dat)
d <- derivatives(m, 's(x2)')














library(ggplot2); library(mgcv); library(gratia); library(data.table)

lengths <- fread('data/derived/lengths.csv')
lengths[, yr_fac := as.factor(year)]
lengths[, record_num := as.factor(record_num)]


atl <- lengths[grepl(' men', scientific) & length < 700]

atl_mod <- bam(length ~
                 s(wk, k = 15, bs = 'cs') +
                 s(temp, bs = 'cs')+
                 s(sal, bs = 'cs') +
                 s(record_num, bs = 're'),
               data = atl,
               discrete = T,
               control = gam.control(nthreads = 16))

summary(atl_mod)

atl_mod2 <- bam(length ~
                  s(wk, k = 15, bs = 'cs') +
                  sal +
                  s(record_num, bs = 're'),
                data = atl,
                discrete = T,
                control = gam.control(nthreads = 16))


library(gamm4)
k <- gamm4(length ~
             s(wk, k = 15, bs = 'cs') +
             sal,
           random = ~(1|record_num/yr_fac),
           data = atl)

draw(k$gam)
