library(ggplot2); library(mgcv); library(gratia); library(data.table)
lengths <- fread('data/derived/lengths (QAQC).csv')
lengths[, yr_fac := as.factor(year)]
lengths[, record_num := as.factor(record_num)]

head(lengths)

unique(lengths$scientific)
