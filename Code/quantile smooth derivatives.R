library(data.table); library(qgam); library(mgcViz); library(ggplot2)

lengths <- fread('data/derived/lengths2.csv')
temp <- lengths[scientific == 'anchoa mitchilli']
temp[, yr_fac := factor(year)]
test.1 <- qgam(list(length ~ s(wk, by = yr_fac, bs = 'ad', m = 5) +
                      s(yr_fac, bs = 'fs', m = 5), 
                    ~s(wk, bs = 'ad')), 
               data = temp, qu = 0.1, 
               multicore = T)

## 2000 ----
draw(test.9, select = 1) + 
  draw(derivatives(test.9, 1, order = 1)) + 
  draw(derivatives(test.9, 1, order = 2, eps = 0.00001)) + 
  plot_layout(2,2)

## 2010 ----
draw(test.9, select = 11) + 
  draw(derivatives(test.9, 11, order = 1)) + 
  draw(derivatives(test.9, 11, order = 2, eps = 0.00001)) + 
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
