
set.seed(8675309)

adults_t0 <- data.frame(
  age = 'adult',
  time = 0,
  length = rnorm(10000, 85, 10)
)
adults_t1 <- data.frame(
  age = 'adult',
  time = 1,
  length = rnorm(10000, 90, 10)
)
adults_t2 <- data.frame(
  age = 'adult',
  time = 2,
  length = rnorm(10000, 95, 10)
)


yoy_t0 <- data.frame(
  NULL
)
yoy_t1 <- data.frame(
  age = 'yoy',
  time = 1,
  length = rnorm(10000, 20, 3)
)
yoy_t2 <- data.frame(
  age = 'yoy',
  time = 2,
  length = rnorm(10000, 35, 6)
)



whole_pop <- rbind(
  adults_t0,
  adults_t1,
  adults_t2,
  yoy_t0,
  yoy_t1,
  yoy_t2
)

whole_pop$length <- round(whole_pop$length)

nhauls <- 3
seine_db <- data.frame(NULL)

for(t in seq(0, 2, 1)){
  pop_t <- whole_pop[whole_pop$time == t,]
  
  hauls_t <- pop_t[sample(nrow(pop_t), 30 * nhauls, replace = F),]
  
  seine_db <- rbind(seine_db, hauls_t)
}


library(ggplot2)
ggplot(data = seine_db, aes(x = length, fill = age)) +
  geom_histogram() +
  facet_wrap(~ time, nrow = 1) +
  geom_density(aes(y = after_stat(count)),
               adjust = 3)



m_adult <- lm(length ~ time,
              data = seine_db,
              subset = (age == 'adult'))
coef(m_adult)
# (Intercept)        time 
# 85.338920    5.313131 
summary(m_adult)$sigma
# [1] 10.30058

model_mind <- data.frame(
  time = rep(seq(0, 2, 1), each = 500)
)

model_mind$length <- 
  c(
    rnorm(500,
          coef(m_adult)['(Intercept)'] + coef(m_adult)['time'] * 0,
          summary(m_adult)$sigma),
    rnorm(500,
          coef(m_adult)['(Intercept)'] + coef(m_adult)['time'] * 1,
          summary(m_adult)$sigma),
    rnorm(500,
          coef(m_adult)['(Intercept)'] + coef(m_adult)['time'] * 2,
          summary(m_adult)$sigma)
  )



ggplot(data = seine_db[seine_db$age == 'adult',]) +
  geom_point(aes(x = time, y = length, color = age)) +
  geom_density(data = model_mind,
               aes(y = length, x = after_stat(scaled) * 0.3, group = as.factor(time)),
               adjust = 2,
               position = position_nudge(c(rep(0, 512),
                                           rep(1, 512),
                                           rep(2, 512))))+
  geom_abline(slope = coef(m_adult)['time'],
              intercept = coef(m_adult)['(Intercept)'])




# Biased
m_all <- lm(length ~ time,
            data = seine_db)
coef(m_all)
# (Intercept)        time 
# 79.21296   -10.76111 
summary(m_all)$sigma
# 29.85179

model_mind <- data.frame(
  time = rep(seq(0, 2, 1), each = 500)
)

model_mind$length <- 
  c(
    rnorm(500,
          coef(m_all)['(Intercept)'] + coef(m_all)['time'] * 0,
          summary(m_all)$sigma),
    rnorm(500,
          coef(m_all)['(Intercept)'] + coef(m_all)['time'] * 1,
          summary(m_all)$sigma),
    rnorm(500,
          coef(m_all)['(Intercept)'] + coef(m_all)['time'] * 2,
          summary(m_all)$sigma)
  )




ggplot(data = seine_db) +
  geom_point(aes(x = time, y = length, color = age),
             show.legend = F) +
  geom_density(data = model_mind,
               aes(y = length, x = after_stat(scaled) * 0.3, group = as.factor(time)),
               adjust = 2,
               position = position_nudge(c(rep(0, 512),
                                           rep(1, 512),
                                           rep(2, 512))))+
  geom_abline(slope = coef(m_all)['time'],
              intercept = coef(m_all)['(Intercept)'])




# Account mean
m_acct_mean <- lm(length ~ time + age,
                  data = seine_db)
coef(m_acct_mean)
# (Intercept)        time      ageyoy 
# 84.297630    6.725183  -66.968787 
summary(m_acct_mean)$sigma
# 9.340391


model_mind <- data.frame(
  time = c(
    rep(seq(0, 2, 1), each = 500),
    rep(seq(1, 2, 1), each = 500)
  ),
  age = c(rep('adult', 1500),
          rep('yoy', 1000))
)

model_mind$length <- 
  c(
    rnorm(500,
          coef(m_acct_mean)['(Intercept)'] + coef(m_acct_mean)['time'] * 0 +
            coef(m_acct_mean)['ageyoy'] * 0,
          summary(m_acct_mean)$sigma),
    rnorm(500,
          coef(m_acct_mean)['(Intercept)'] + coef(m_acct_mean)['time'] * 1 +
            coef(m_acct_mean)['ageyoy'] * 0,
          summary(m_acct_mean)$sigma),
    rnorm(500,
          coef(m_acct_mean)['(Intercept)'] + coef(m_acct_mean)['time'] * 2 +
            coef(m_acct_mean)['ageyoy'] * 0,
          summary(m_acct_mean)$sigma),
    
    rnorm(500,
          coef(m_acct_mean)['(Intercept)'] + coef(m_acct_mean)['time'] * 1 +
            coef(m_acct_mean)['ageyoy'] * 1,
          summary(m_acct_mean)$sigma),
    rnorm(500,
          coef(m_acct_mean)['(Intercept)'] + coef(m_acct_mean)['time'] * 2 +
            coef(m_acct_mean)['ageyoy'] * 1,
          summary(m_acct_mean)$sigma)
  )




ggplot(data = seine_db) +
  geom_point(aes(x = time, y = length, color = age),
             show.legend = F) +
  geom_density(data = model_mind,
               aes(y = length, x = after_stat(scaled) * 0.3,
                   group = interaction(age, as.factor(time))),
               adjust = 2,
               position = position_nudge(c(rep(0, 512),
                                           rep(1, 512),
                                           rep(1, 512),
                                           rep(2, 512),
                                           rep(2, 512)))) +
  geom_abline(slope = coef(m_acct_mean)['time'],
              intercept = c(coef(m_acct_mean)['(Intercept)'],
                            coef(m_acct_mean)['(Intercept)'] + coef(m_acct_mean)['ageyoy']))




# Account both
m_acct_both <- lm(length ~ time + age + time:age,
                  data = seine_db)
coef(m_acct_both)
# (Intercept)        time      ageyoy time:ageyoy 
# 85.338920    5.313131  -79.379539    8.909306 
summary(m_acct_both)$sigma
# 9.047891


model_mind <- data.frame(
  time = c(
    rep(seq(0, 2, 1), each = 500),
    rep(seq(1, 2, 1), each = 500)
  ),
  age = c(rep('adult', 1500),
          rep('yoy', 1000))
)

model_mind$length <- 
  c(
    rnorm(500,
          coef(m_acct_both)['(Intercept)'] +
            coef(m_acct_both)['ageyoy'] * 0 +
            
            (coef(m_acct_both)['time'] +
               (coef(m_acct_both)['time:ageyoy'] * 0 )) * 0,
          summary(m_acct_both)$sigma),
    rnorm(500,
          coef(m_acct_both)['(Intercept)'] +
            coef(m_acct_both)['ageyoy'] * 0 +
            
            (coef(m_acct_both)['time'] +
               (coef(m_acct_both)['time:ageyoy'] * 0 )) * 1,
          summary(m_acct_both)$sigma),
    rnorm(500,
          coef(m_acct_both)['(Intercept)'] +
            coef(m_acct_both)['ageyoy'] * 0 +
            
            (coef(m_acct_both)['time'] +
               (coef(m_acct_both)['time:ageyoy'] * 0 )) * 2,
          summary(m_acct_both)$sigma),
    
    rnorm(500,
          coef(m_acct_both)['(Intercept)'] +
            coef(m_acct_both)['ageyoy'] * 1 +
            
            (coef(m_acct_both)['time'] +
               (coef(m_acct_both)['time:ageyoy'] * 1 )) * 1,
          summary(m_acct_both)$sigma),
    rnorm(500,
          coef(m_acct_both)['(Intercept)'] +
            coef(m_acct_both)['ageyoy'] * 1 +
            
            (coef(m_acct_both)['time'] +
               (coef(m_acct_both)['time:ageyoy'] * 1 )) * 2,
          summary(m_acct_both)$sigma)
  )




ggplot(data = seine_db) +
  geom_point(aes(x = time, y = length, color = age),
             show.legend = F) +
  geom_density(data = model_mind,
               aes(y = length, x = after_stat(scaled) * 0.3,
                   group = interaction(age, as.factor(time))),
               adjust = 2,
               position = position_nudge(c(rep(0, 512),
                                           rep(1, 512),
                                           rep(1, 512),
                                           rep(2, 512),
                                           rep(2, 512)))) +
  geom_abline(slope = c(coef(m_acct_both)['time'],
                        coef(m_acct_both)['time'] + coef(m_acct_both)['time:ageyoy']),
              intercept = c(coef(m_acct_both)['(Intercept)'],
                            coef(m_acct_both)['(Intercept)'] + coef(m_acct_both)['ageyoy']))




#LSS
library(mgcv)
m_lss <- gam(list(length ~ time,
                  ~ time),
             data = seine_db,
             family = 'gaulss')
coef(m_lss)
# (Intercept)          time (Intercept).1        time.1 
# 83.6642988   -17.8689718     2.6514781     0.6338472 



model_mind <- data.frame(
  time = rep(seq(0, 2, 1), each = 500)
)

model_mind$length <- 
  c(
    rnorm(500,
          coef(m_lss)['(Intercept)'] + coef(m_lss)['time'] * 0,
          exp(coef(m_lss)['(Intercept).1'] + coef(m_lss)['time.1'] * 0) + 0.01),
    rnorm(500,
          coef(m_lss)['(Intercept)'] + coef(m_lss)['time'] * 1,
          exp(coef(m_lss)['(Intercept).1'] + coef(m_lss)['time.1'] * 1) + 0.01),
    rnorm(500,
          coef(m_lss)['(Intercept)'] + coef(m_lss)['time'] * 2,
          exp(coef(m_lss)['(Intercept).1'] + coef(m_lss)['time.1'] * 2) + 0.01)
  )

ggplot(data = seine_db) +
  geom_point(aes(x = time, y = length, color = age),
             show.legend = F) +
  geom_density(data = model_mind,
               aes(y = length, x = after_stat(scaled) * 0.3, group = as.factor(time)),
               adjust = 2,
               position = position_nudge(c(rep(0, 512),
                                           rep(1, 512),
                                           rep(2, 512))))+
  geom_abline(slope = coef(m_lss)['time'],
              intercept = coef(m_lss)['(Intercept)']) 




library(flexmix)
m_mix <- flexmix(length ~ time, data = seine_db, k = 2)

parameters(m_mix)
#                     Comp.1    Comp.2
# coef.(Intercept)  5.959678 85.339021
# coef.time        14.222140  5.312635
# sigma             5.758945 10.281611


model_mind <- data.frame(
  time = c(
    rep(seq(0, 2, 1), each = 500),
    rep(seq(1, 2, 1), each = 500)
  ),
  age = c(rep('adult', 1500),
          rep('yoy', 1000))
)

model_mind$length <- 
  c(
    rnorm(500,
          parameters(m_mix)['coef.(Intercept)', 'Comp.2'] + 
            parameters(m_mix)['coef.time', 'Comp.2'] * 0,
          parameters(m_mix)['sigma', 'Comp.2']),
    rnorm(500,
          parameters(m_mix)['coef.(Intercept)', 'Comp.2'] + 
            parameters(m_mix)['coef.time', 'Comp.2'] * 1,
          parameters(m_mix)['sigma', 'Comp.2']),
    rnorm(500,
          parameters(m_mix)['coef.(Intercept)', 'Comp.2'] + 
            parameters(m_mix)['coef.time', 'Comp.2'] * 2,
          parameters(m_mix)['sigma', 'Comp.2']),
    
    rnorm(500,
          parameters(m_mix)['coef.(Intercept)', 'Comp.1'] + 
            parameters(m_mix)['coef.time', 'Comp.1'] * 1,
          parameters(m_mix)['sigma', 'Comp.1']),
    rnorm(500,
          parameters(m_mix)['coef.(Intercept)', 'Comp.1'] + 
            parameters(m_mix)['coef.time', 'Comp.1'] * 2,
          parameters(m_mix)['sigma', 'Comp.1'])
  )
  

ggplot(data = seine_db) +
  geom_point(aes(x = time, y = length, color = age),
             show.legend = F) +
  geom_density(data = model_mind,
               aes(y = length, x = after_stat(scaled) * 0.3,
                   group = interaction(age, as.factor(time))),
               adjust = 2,
               position = position_nudge(c(rep(0, 512),
                                           rep(1, 512),
                                           rep(1, 512),
                                           rep(2, 512),
                                           rep(2, 512)))) +
  geom_abline(slope = c(coef(m_acct_both)['time'],
                        coef(m_acct_both)['time'] + coef(m_acct_both)['time:ageyoy']),
              intercept = c(coef(m_acct_both)['(Intercept)'],
                            coef(m_acct_both)['(Intercept)'] + coef(m_acct_both)['ageyoy']))






library(qgam)
m_q <- mqgam(length ~ time,
             data = seine_db,
             qu = c(0.1, 0.5, 0.9))



qdo(m_q, c(0.1, 0.5, 0.9), coef)
# [[1]]
# (Intercept)        time 
# 45.16160   -23.06907 
# 
# [[2]]
# (Intercept)        time 
# 82.63123   -12.90910 
# 
# [[3]]
# (Intercept)        time 
# 99.753265    2.419664 
model_mind <- do.call(rbind, qdo(m_q, c(0.1, 0.5, 0.9), coef))


ggplot(data = seine_db) +
  geom_point(aes(x = time, y = length, color = age),
             show.legend = F) +
  geom_abline(slope = model_mind[, 2],
              intercept = model_mind[, 1])
