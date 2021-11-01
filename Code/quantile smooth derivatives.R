library(data.table); library(qgam); library(gratia); library(ggplot2)

derv_calc <- function(model, data, eps){
  # Find range of weeks sampled for each year
  wks <- data[, .(mn = min(wk), mx = max(wk)), by = yr_fac]
  
  dervs <- lapply(levels(data$yr_fac), function(yr){
    min_wk <- wks[yr_fac == yr]$mn
    max_wk <- wks[yr_fac == yr]$mx
    
    newdat <- data.table(
      yr_fac = yr,
      wk = c(seq(min_wk, max_wk, length.out = 100),
             #Nudge a bit to calculate 1st derivative
             seq(min_wk, max_wk, length.out = 100) + eps,
             #Nudge a bit more to calculate 2nd derivative
             seq(min_wk, max_wk, length.out = 100) + eps*2)
    )
    
    # Sample linear combination of terms from posterior
    samples <- fitted_samples(model, n = 1000, seed = 42,
                              terms = c('s(wk)', 's(yr_fac)',
                                        paste0('s(wk):yr_fac', yr)),
                              newdata = newdat,
                              scale = 'linear_predictor')
    
    
    samples <- split(samples, samples$draw)
    # Rearrange data so fitted and nudged fitted values line up
    samples <- lapply(samples, function(.){
      data.table(.[1:100,],
                 fitted_nudge = .$fitted[101:200],
                 fitted_nudge2 = .$fitted[201:300],
                 wk = newdat$wk[1:100])
    })
    
    # Apply finite differences. don't really understand this, but it's from the 
    #   gratia package code
    samples <- lapply(samples, function(.){
      data.table(.,
                 d1 = (.$fitted_nudge - .$fitted) / 1.2,
                 d2 = ((.$fitted_nudge2 - (2*.$fitted_nudge) + .$fitted) / 1.2^2))
    })
    
    samples <- rbindlist(samples)
    
    # Calculate median and confidence intervals from draws
    wk_ts <- samples[, .(lci_pred = quantile(fitted, 0.025),
                         med_pred = quantile(fitted, 0.5),
                         uci_pred = quantile(fitted, 0.975),
                         lci_d1 = quantile(d1, 0.025),
                         med_d1 = quantile(d1, 0.5),
                         uci_d1 = quantile(d1, 0.975),
                         lci_d2 = quantile(d2, 0.025, na.rm = T),
                         med_d2 = quantile(d2, 0.5, na.rm = T),
                         uci_d2 = quantile(d2, 0.975, na.rm = T)), by = wk]
    
    # Calculate median and confidence intervals of week of recruitment
    
    ##  Select the first week where the sign of the first derivative flips from 
    ##  negative to positive (first local minimum)
    trans_wk <- samples[samples[, .I[c(0, diff(sign(d1))) > 0][1], by = draw]$V1]
    
    ## Calculate the CI
    trans_wk <- trans_wk[, quantile(wk, c(0.025, 0.5, 0.975))]
    
    list(wk_ts, trans_wk)
  })
  
  names(dervs) <- levels(data$yr_fac)
  recruit_ts <- data.table(t(sapply(dervs, function(.) .[[2]])), keep.rownames = T)
  setnames(recruit_ts, 'rn', 'year')
  recruit_ts[, year := as.numeric(year)]
  
  dervs <- rbindlist(lapply(dervs, function(.) .[[1]]), idcol = 'yr_fac')
  dervs <- data.table(dervs)[wks, on = 'yr_fac']
  dervs <- dervs[between(wk, mn, mx)]
  
  list(recruit_ts = recruit_ts,
       derivatives = dervs)
}

lengths <- fread('data/derived/lengths-edata.csv')


#### SPOT ----
spot <- lengths[scientific == 'leiostomus xanthurus']
spot <- spot[year %in% unique(spot, by = c('year', 'wk'))[, .N >= 2, by = year][V1 == T]$year,]
spot[, yr_fac := factor(year)]


# model names follow types in Pedersen et al 2019
# Global trend of week -- helps with data-poor years
# job::job({
#   spot_modGI <- qgam(length ~ 
#                   s(wk, k = 11, m = 2) +
#                   s(yr_fac, bs = 're') +
#                   s(wk, by = yr_fac, k = 11, m = 1), 
#                 data = spot,
#                 qu = 0.05)
#   saveRDS(spot_modGI, 'fitted models/spot_modGI.rds')
# })
# job::job({
#   modI <- qgam(length ~ 
#                   s(yr_fac, bs = 're') +
#                   s(wk, by = yr_fac, k = 11, m = 2), 
#                 data = spot,
#                 qu = 0.05)
# })

job::job({
  spot_modGI <- qgam(length ~
                  s(wk, k = 11, m = 2) +
                  s(yr_fac, bs = 're') +
                  s(wk, by = yr_fac, k = 11, m = 1),
                data = spot,
                qu = 0.95)
  saveRDS(spot_modGI, 'fitted models/spot_modGI_95.rds')
})


spot_mod <- readRDS('fitted models/spot_modGI_95.rds')
job::job(
  {
    spot_derv <- derv_calc(spot_mod, spot, 1.1)
  },
  packages = c('data.table', 'gratia')
)


ggplot(data= spot_derv$recruit_ts, aes(x = year, y = `50%`)) +
  geom_pointrange(aes(ymin = `2.5%`, ymax = `97.5%`)) +
  geom_line() +
  labs(x = NULL, y = 'Week of recruitment', title = 'Spot') +
  theme_minimal()

ggplot(data = data.frame(predict(spot_mod,
                                 newdata = data.frame(wk = seq(19, 42, length.out = 100)),
                                 newdata.guaranteed = T, terms = 's(wk)', se.fit = T),
                         week = seq(19, 42, length.out=100))) +
  geom_ribbon(aes(x = week, ymin = fit - 1.96 * se.fit, ymax = fit + 1.96 * se.fit),
              fill = 'lightgray') +
  geom_line(aes(x = week, y = fit)) +
  labs(x = 'Week', y = 'Length (mm)', title = 'Spot, global effect of week') +
  theme_minimal()

ggplot(data = spot_derv$derivatives) +
  geom_ribbon(aes(x = wk, ymin = lci_pred, ymax = uci_pred), fill = 'lightgray') +
  geom_line(aes(x = wk, y = med_pred)) +
  labs(x = 'Week', y = 'Length (mm)', title = 'Spot, yearly trend') +
  facet_wrap(~yr_fac, scales = 'free_y') +
  theme_minimal()


ggplot(data = spot_derv$derivatives, aes(x = wk)) +
  geom_ribbon(aes(ymin = lci_d1, ymax = uci_d1), fill = 'lightgray') +
  geom_line(aes(y = med_d1)) +
  labs(x = 'Week', y = 'Growth rate (mm/week)',
       title = 'Spot, first derivative of yearly trend') +
  facet_wrap(~yr_fac, scales = 'free_y') +
  theme_minimal()

ggplot(data = spot_derv$derivatives, aes(x = wk)) +
  geom_ribbon(aes(ymin = lci_d2, ymax = uci_d2), fill = 'lightgray') +
  geom_line(aes(y = med_d2)) +
  labs(x = 'Week', y = 'Change in growth rate (mm/week)',
       title = 'Spot, second derivative of yearly trend') +
  facet_wrap(~yr_fac, scales = 'free_y') +
  theme_minimal()



#### ATLANTIC SILVERSIDE ----
atl <- lengths[scientific == 'menidia menidia']
atl <- atl[year %in% unique(atl, by = c('year', 'wk'))[, .N >= 2, by = year][V1 == T]$year,]
atl[, yr_fac := factor(year)]

# job::job({
#   atl_modGI <- qgam(length ~
#                       s(wk, k = 11, m = 2) +
#                       s(yr_fac, bs = 're') +
#                       s(wk, by = yr_fac, k = 11, m = 1),
#                     data = atl,
#                     qu = 0.05)
#   saveRDS(atl_modGI, 'fitted models/atl_modGI.rds')
# }, packages = 'qgam')

# job::job({
#   atl_modGI <- qgam(length ~
#                       s(wk, k = 11, m = 2) +
#                       s(yr_fac, bs = 're') +
#                       s(wk, by = yr_fac, k = 11, m = 1),
#                     data = atl,
#                     qu = 0.95)
#   saveRDS(atl_modGI, 'fitted models/atl_modGI_95.rds')
# }, packages = 'qgam')

atl_mod <- readRDS('fitted models/atl_modGI.rds')
job::job(
  {
    atl_derv <- derv_calc(atl_mod, atl, 1.1)
  },
  packages = c('data.table', 'gratia')
)

ggplot(data= atl_derv$recruit_ts, aes(x = year, y = `50%`)) +
  geom_pointrange(aes(ymin = `2.5%`, ymax = `97.5%`)) +
  geom_line() +
  labs(x = NULL, y = 'Week of recruitment', title = 'Atlantic silverside') +
  theme_minimal()


ggplot(data = data.frame(predict(atl_mod,
                                 newdata = data.frame(wk = seq(19, 42, length.out = 100)),
                                 newdata.guaranteed = T, terms = 's(wk)', se.fit = T),
                         week = seq(19, 42, length.out=100))) +
  geom_ribbon(aes(x = week, ymin = fit - 1.96 * se.fit, ymax = fit + 1.96 * se.fit),
              fill = 'lightgray') +
  geom_line(aes(x = week, y = fit)) +
  labs(x = 'Week', y = 'Length (mm)', title = 'Atlantic silverside, global effect of week') +
  theme_minimal()


ggplot(data = atl_derv$derivatives) +
  geom_ribbon(aes(x = wk, ymin = lci_pred, ymax = uci_pred), fill = 'lightgray') +
  geom_line(aes(x = wk, y = med_pred)) +
  labs(x = 'Week', y = 'Length (mm)') +
  facet_wrap(~yr_fac) +
  theme_minimal() +
  theme(strip.text = element_text(size = 12),
        axis.title = element_text(size = 12))


ggplot(data = atl_derv$derivatives, aes(x = wk)) +
  geom_ribbon(aes(ymin = lci_d1, ymax = uci_d1), fill = 'lightgray') +
  geom_line(aes(y = med_d1)) +
  labs(x = 'Week', y = 'First derivative (mm/week)') +
  geom_hline(yintercept = 0) +
  facet_wrap(~yr_fac) +
  theme_minimal() +
  theme(strip.text = element_text(size = 12),
        axis.title = element_text(size = 12))


ggplot(data = atl_derv$derivatives, aes(x = wk)) +
  geom_ribbon(aes(ymin = lci_d2, ymax = uci_d2), fill = 'lightgray') +
  geom_line(aes(y = med_d2)) +
  labs(x = 'Week', y = 'Change in growth rate (mm/week)',
       title = 'Atlantic silverside, second derivative of yearly trend') +
  facet_wrap(~yr_fac, scales = 'free_y') +
  theme_minimal()




#### BLUEFISH ----
bf <- lengths[scientific == 'pomatomus saltatrix']
bf <- bf[year %in% unique(bf, by = c('year', 'wk'))[, .N >= 2, by = year][V1 == T]$year,]
bf[, yr_fac := factor(year)]


# job::job({
#   bf_modGI <- qgam(length ~
#                      s(wk, k = 11, m = 2) +
#                      s(yr_fac, bs = 're') +
#                      s(wk, by = yr_fac, k = 11, m = 1),
#                    data = bf,
#                    qu = 0.05)
#   saveRDS(bf_modGI, 'fitted models/bf_modGI.rds')
# }, packages = 'qgam')

job::job({
  bf_modGI <- qgam(length ~
                     s(wk, k = 11, m = 2) +
                     s(yr_fac, bs = 're') +
                     s(wk, by = yr_fac, k = 11, m = 1),
                   data = bf,
                   qu = 0.95)
  saveRDS(bf_modGI, 'fitted models/bf_modGI_95.rds')
}, packages = 'qgam')


bf_mod <- readRDS('fitted models/bf_modGI_95.rds')
job::job({
  bf_derv <- derv_calc(bf_mod, bf, 1.1)
}, packages = c('data.table', 'gratia'))


ggplot(data= bf_derv$recruit_ts, aes(x = year, y = `50%`)) +
  geom_pointrange(aes(ymin = `2.5%`, ymax = `97.5%`)) +
  geom_line() +
  labs(x = NULL, y = 'Week of recruitment') +
  theme_minimal()


ggplot(data = data.frame(predict(bf_mod,
                                 newdata = data.frame(wk = seq(19, 42, length.out = 100)),
                                 newdata.guaranteed = T, terms = 's(wk)', se.fit = T),
                         week = seq(19, 42, length.out=100))) +
  geom_ribbon(aes(x = week, ymin = fit - 1.96 * se.fit, ymax = fit + 1.96 * se.fit),
              fill = 'lightgray') +
  geom_line(aes(x = week, y = fit)) +
  labs(x = 'Week', y = 'Length (mm)', title = 'Bluefish, global effect of week') +
  theme_minimal()

ggplot(data = bf_derv$derivatives) +
  geom_ribbon(aes(x = wk, ymin = lci_pred, ymax = uci_pred), fill = 'lightgray') +
  geom_line(aes(x = wk, y = med_pred)) +
  labs(x = 'Week', y = 'Length (mm)', title = 'Bluefish, yearly trend') +
  facet_wrap(~yr_fac, scales = 'free_y') +
  theme_minimal()


ggplot(data = bf_derv$derivatives, aes(x = wk)) +
  geom_ribbon(aes(ymin = lci_d1, ymax = uci_d1), fill = 'lightgray') +
  geom_line(aes(y = med_d1)) +
  labs(x = 'Week', y = 'Growth rate (mm/week)',
       title = 'Bluefish, first derivative of yearly trend') +
  facet_wrap(~yr_fac, scales = 'free_y') +
  theme_minimal()

ggplot(data = bf_derv$derivatives, aes(x = wk)) +
  geom_ribbon(aes(ymin = lci_d2, ymax = uci_d2), fill = 'lightgray') +
  geom_line(aes(y = med_d2)) +
  labs(x = 'Week', y = 'Change in growth rate (mm/week)',
       title = 'Bluefish, second derivative of yearly trend') +
  facet_wrap(~yr_fac, scales = 'free_y') +
  theme_minimal()



#### BAY ANCHOVY -----
banch <- lengths[scientific == 'anchoa mitchilli']
banch <- banch[year %in% unique(banch, by = c('year', 'wk'))[, .N >= 2, by = year][V1 == T]$year,]
banch[, yr_fac := factor(year)]

# job::job({
#   banch_modGI <- qgam(length ~
#                      s(wk, k = 11, m = 2) +
#                      s(yr_fac, bs = 're') +
#                      s(wk, by = yr_fac, k = 11, m = 1),
#                    data = banch,
#                    qu = 0.05)
#   saveRDS(banch_modGI, 'fitted models/banch_modGI.rds')
# }, packages = 'qgam')

job::job({
  banch_modGI <- qgam(length ~
                     s(wk, k = 11, m = 2) +
                     s(yr_fac, bs = 're') +
                     s(wk, by = yr_fac, k = 11, m = 1),
                   data = banch,
                   qu = 0.95)
  saveRDS(banch_modGI, 'fitted models/banch_modGI_95.rds')
}, packages = 'qgam')

banch_mod <- readRDS('fitted models/banch_modGI_95.rds')
job::job({
  banch_derv <- derv_calc(banch_mod, banch, 1.1)
}, packages = c('data.table', 'gratia'))


ggplot(data= banch_derv$recruit_ts, aes(x = year, y = `50%`)) +
  geom_pointrange(aes(ymin = `2.5%`, ymax = `97.5%`)) +
  geom_line() +
  labs(x = NULL, y = 'Week of recruitment') +
  theme_minimal()


ggplot(data = data.frame(predict(banch_mod,
                                 newdata = data.frame(wk = seq(19, 42, length.out = 100)),
                                 newdata.guaranteed = T, terms = 's(wk)', se.fit = T),
                         week = seq(19, 42, length.out=100))) +
  geom_ribbon(aes(x = week, ymin = fit - 1.96 * se.fit, ymax = fit + 1.96 * se.fit),
              fill = 'lightgray') +
  geom_line(aes(x = week, y = fit)) +
  labs(x = 'Week', y = 'Length (mm)', title = 'Bay anchovy, global effect of week') +
  theme_minimal()

ggplot(data = banch_derv$derivatives) +
  geom_ribbon(aes(x = wk, ymin = lci_pred, ymax = uci_pred), fill = 'lightgray') +
  geom_line(aes(x = wk, y = med_pred)) +
  labs(x = 'Week', y = 'Length (mm)', title = 'Bay anchovy, yearly trend') +
  facet_wrap(~yr_fac, scales = 'free_y') +
  theme_minimal()


ggplot(data = banch_derv$derivatives, aes(x = wk)) +
  geom_ribbon(aes(ymin = lci_d1, ymax = uci_d1), fill = 'lightgray') +
  geom_line(aes(y = med_d1)) +
  labs(x = 'Week', y = 'Growth rate (mm/week)',
       title = 'Bay anchovy, first derivative of yearly trend') +
  facet_wrap(~yr_fac, scales = 'free_y') +
  theme_minimal()

ggplot(data = banch_derv$derivatives, aes(x = wk)) +
  geom_ribbon(aes(ymin = lci_d2, ymax = uci_d2), fill = 'lightgray') +
  geom_line(aes(y = med_d2)) +
  labs(x = 'Week', y = 'Change in growth rate (mm/week)',
       title = 'Bay anchovy, second derivative of yearly trend') +
  facet_wrap(~yr_fac, scales = 'free_y') +
  theme_minimal()



#### STRIPED BASS -----
sb <- lengths[scientific == 'morone saxatilis']
sb <- sb[year %in% unique(sb, by = c('year', 'wk'))[, .N >= 2, by = year][V1 == T]$year,]
sb[, yr_fac := factor(year)]

# job::job({
#   sb_modGI <- qgam(length ~
#                      s(wk, k = 11, m = 2) +
#                      s(yr_fac, bs = 're') +
#                      s(wk, by = yr_fac, k = 11, m = 1),
#                    data = sb,
#                    qu = 0.05)
#   saveRDS(sb_modGI, 'fitted models/sb_modGI.rds')
# }, packages = 'qgam')


# job::job({
#   sb_modGI <- qgam(length ~
#                      s(wk, k = 11, m = 2) +
#                      s(yr_fac, bs = 're') +
#                      s(wk, by = yr_fac, k = 11, m = 1),
#                    data = sb,
#                    qu = 0.95)
#   saveRDS(sb_modGI, 'fitted models/sb_modGI_95.rds')
# }, packages = 'qgam')

sb_mod <- readRDS('fitted models/sb_modGI.rds')
job::job({
  sb_derv <- derv_calc(sb_mod, sb, 0.9)
}, packages = c('data.table', 'gratia'))


ggplot(data= sb_derv$recruit_ts, aes(x = year, y = `50%`)) +
  geom_pointrange(aes(ymin = `2.5%`, ymax = `97.5%`)) +
  geom_line() +
  labs(x = NULL, y = 'Week of recruitment') +
  theme_minimal()


ggplot(data = data.frame(predict(sb_mod,
                                 newdata = data.frame(wk = seq(19, 42, length.out = 100)),
                                 newdata.guaranteed = T, terms = 's(wk)', se.fit = T),
                         week = seq(19, 42, length.out=100))) +
  geom_ribbon(aes(x = week, ymin = fit - 1.96 * se.fit, ymax = fit + 1.96 * se.fit),
              fill = 'lightgray') +
  geom_line(aes(x = week, y = fit)) +
  labs(x = 'Week', y = 'Length (mm)', title = 'Striped bass, global effect of week') +
  theme_minimal()

ggplot(data = sb_derv$derivatives) +
  geom_ribbon(aes(x = wk, ymin = lci_pred, ymax = uci_pred), fill = 'lightgray') +
  geom_line(aes(x = wk, y = med_pred)) +
  labs(x = 'Week', y = 'Length (mm)') +
  facet_wrap(~yr_fac) +
  theme_minimal() +
  theme(strip.text = element_text(size = 12),
        axis.text = element_text(size = 12))


ggplot(data = sb_derv$derivatives, aes(x = wk)) +
  geom_ribbon(aes(ymin = lci_d1, ymax = uci_d1), fill = 'lightgray') +
  geom_line(aes(y = med_d1)) +
  labs(x = 'Week', y = 'First derivative (mm/week)') +
  geom_hline(yintercept = 0) +
  facet_wrap(~yr_fac) +
  theme(strip.text = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  theme_minimal()

  ggplot(data = sb_derv$derivatives, aes(x = wk)) +
  geom_ribbon(aes(ymin = lci_d2, ymax = uci_d2), fill = 'lightgray') +
  geom_line(aes(y = med_d2)) +
  labs(x = 'Week', y = 'Change in growth rate (mm/week)',
       title = 'Striped bass, second derivative of yearly trend') +
  facet_wrap(~yr_fac, scales = 'free_y') +
  theme_minimal()
