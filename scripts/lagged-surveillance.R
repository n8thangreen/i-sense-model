

library(ggplot2)
library(magrittr)
library(dplyr)
library(reshape2)


load("../../data raw/data_positiveILI_NPFS_perrine_march2017.RData")
load("../../data raw/data_positiveILI_GP_perrine_march2017.RData")

load("C:/Users/Nathan/Dropbox/i-sense/R/Ilarias-model/H1N1model/data/dates_lookup.RData")

##TODO##

# use previous swabbing data set os dont use CIS anymore



dat <-
  dat.posILI.NPFS %>%
  filter(ageGP.fac == "25-44",
         w < 24) %>%
  select(-ageGP.fac) %>%
  mutate(negILI = authorisations - posILI,
         hi_PCTposILI = qbinom(p = 0.975, size = round(posILI), prob = 0.9), #upper-bound positive after test
         lo_PCTposILI = qbinom(p = 0.025, size = round(posILI), prob = 0.9), #lower-bound positive after test
         hi_PCTnegILI = qbinom(p = 0.975, size = round(negILI), prob = 0.3), #upper-bound positive after test
         lo_PCTnegILI = qbinom(p = 0.025, size = round(negILI), prob = 0.3), #lower-bound positive after test
         hi_PCT = hi_PCTposILI + hi_PCTnegILI,
         lo_PCT = lo_PCTposILI + lo_PCTnegILI)

# join with week lookup
dat <-
  dat %>%
  rename(NPFS_weeks = w) %>%
  merge(dates_lookup) %>%
  mutate(week_end = as.Date(week_end))


# sample
# rbinom(n = 10000, size = 56332, prob = 0.3) %>%
#   quantile(c(0.025, 0.975))




dat2 <- dat
dat2[dat2$NPFS_weeks > 13, c("authorisations", "hi_PCTposILI", "lo_PCTposILI",
                             "hi_PCTnegILI", "lo_PCTnegILI", "hi_PCT", "lo_PCT")] <- NA

dat2[dat2$NPFS_weeks > 11, c("posILI", "posILI.lo", "posILI.hi", "negILI")] <- NA

# dat2[dat2$NPFS_weeks < 11, c("hi_PCTposILI", "lo_PCTposILI",
#                              "hi_PCTnegILI", "lo_PCTnegILI", "hi_PCT", "lo_PCT")] <- NA


# plots


ggplot(data = dat2) + theme_bw() +
  # geom_ribbon(aes(x = w, ymin = posILI.lo, ymax = posILI.hi),
  #             fill = "blue", alpha = 0.25) +
  geom_line(aes(x = week_end, y = posILI), color = "black", linetype = "dashed") +
  # geom_line(aes(x = w, y = negILI), color = "grey", linetype = "dashed") +
  geom_line(aes(x = week_end, y = authorisations), color = "black") +
  # geom_ribbon(aes(x = w, ymin = posILI, ymax = hi_PCTnegILI + hi_PCTposILI),
  #             fill = "red", alpha = 0.25)
  geom_ribbon(aes(x = week_end, ymin = lo_PCTposILI, ymax = lo_PCTnegILI + lo_PCTposILI),
              fill = "blue", alpha = 0.25) +
  geom_ribbon(aes(x = week_end, ymin = 0, ymax = lo_PCTposILI),
              fill = "red", alpha = 0.25) +
  geom_vline(xintercept = 11, linetype = "dotted")
  # geom_ribbon(aes(x = w, ymin = lo_PCT, ymax = hi_PCT),
  #             fill = "red", alpha = 0.25) +
  # geom_ribbon(aes(x = w, ymin = 0, ymax = hi_PCTnegILI),
  #             fill = "blue", alpha = 0.25) +
  # geom_ribbon(aes(x = w, ymin = lo_PCTnegILI, ymax = hi_PCTnegILI),
  #             fill = "blue", alpha = 0.25) +
  # geom_ribbon(aes(x = w, ymin = lo_PCTposILI, ymax = posILI),
  #             fill = "green", alpha = 0.25) +
  # geom_ribbon(aes(x = w, ymin = lo_PCTposILI, ymax = hi_PCTposILI),
  #             fill = "green", alpha = 0.25) #+ ylim(0, 25000)

