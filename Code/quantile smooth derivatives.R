library(data.table); library(qgam); library(mgcViz); library(ggplot2)

lengths <- fread('data/derived/lengths2.csv')
banch <- lengths[scientific == 'anchoa mitchilli']
banch[, yr_fac := factor(year, ordered = T)]

library(parallel)
cl <- makeCluster(detectCores(logical = F) - 1)
clusterEvalQ(cl, library(qgam))
clusterExport(cl, 'banch')


# test.05_m3 <- qgam(list(length ~ 
#                       s(wk, m = 3) +
#                       s(wk, yr_fac, bs = 'fs', xt = list(bs = 'ad'), m = 3), 
#                     ~ s(wk)), 
#                data = banch,
#                qu = 0.05, 
#                multicore = T,
#                cluster = cl)

test.05_m3 <- qgam(list(length ~ 
                          # s(wk, bs ='cr', k = 24, m = 2) +
                          s(wk, yr_fac, bs = 'fs', xt = list(bs = 'cr'),
                            k = 24, m = 3), 
                        ~ s(wk)), 
                   data = banch,
                   qu = 0.05, 
                   multicore = T,
                   cluster = cl)

stopCluster(cl)

plot(test.1)
new_data <- expand.grid(wk = seq(min(banch$wk), max(banch$wk), 1),
                        yr_fac = levels(banch$yr_fac))


test_pred <- data.frame(new_data,
                        predict(test.05_m3, new_data,
                                type = 'response', se.fit = TRUE))
test_pred$lci <- test_pred$fit - 1.96 * test_pred$se.fit
test_pred$uci <- test_pred$fit + 1.96 * test_pred$se.fit
head(test_pred)

ggplot(data=test_pred, aes(x=wk, y=fit)) +
  facet_wrap(~yr_fac) +
  geom_ribbon(aes(ymin=lci,
                  ymax=uci), alpha=0.25) +
  geom_line(aes(y=fit))






## 2000 ----
draw(test.1, select = 1) + 
  draw(derivatives(test.1, 1, order = 1)) + 
  draw(derivatives(test.1, 1, order = 2, eps = 0.9)) + 
  plot_layout(2,2)

## 2010 ----
draw(test.1, select = 11) + 
  draw(derivatives(test.1, 11, order = 1)) + 
  draw(derivatives(test.1, 11, order = 2, eps = 0.00001)) + 
  plot_layout(2,2)


## 2019 ----
draw(test.9, select = 20) + 
  draw(derivatives(test.9, 20, order = 1)) + 
  draw(derivatives(test.9, 20, order = 2, eps = 0.00001)) + 
  plot_layout(2,2)




test.1 <- qgam(list(length ~ s(wk, by = yr_fac,  m = 3) +
                      s(yr_fac, bs = 'fs', m = 3), 
                    ~s(wk)), 
               data = temp, qu = 0.1, 
               multicore = T)


## 2000 ----
draw(test.1, select = 1) + 
  draw(derivatives(test.1, 1, order = 1)) + 
  draw(derivatives(test.1, 1, order = 2, eps = 0.00001)) + 
  plot_layout(2,2)

## 2010 ----
draw(test.1, select = 11) + 
  draw(derivatives(test.1, 11, order = 1)) + 
  draw(derivatives(test.1, 11, order = 2, eps = 0.00001)) + 
  plot_layout(2,2)


## 2019 ----
draw(test.1, select = 20) + 
  draw(derivatives(test.1, 20, order = 1)) + 
  draw(derivatives(test.1, 20, order = 2, eps = 0.00001)) + 
  plot_layout(2,2)
