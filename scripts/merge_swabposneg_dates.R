
library(plyr)
library(magrittr)


load("C:/Users/Nathan/Dropbox/i-sense/R/Ilarias-model/H1N1model/data/dates_lookup.RData")
load("C:/Users/Nathan/Dropbox/i-sense/data raw/data_positiveILI_GP_perrine_feb2017.RData")
load("C:/Users/Nathan/Dropbox/i-sense/data raw/data_positiveILI_NPFS_perrine_feb2017.RData")


dat.posILI.GP <-
  dat.posILI.GP %>%
  dplyr::rename(NPFS_weeks = week,
                age = ageGP.fac) %>%
  merge(dates_lookup, by = "NPFS_weeks") %>%
  mutate(age = revalue(age, c("0-4" = "04",
                              "5-14" = "514",
                              "15-24" = "1524",
                              "25-44" = "2544",
                              "45-64" = "4564",
                              "65+" = "65.")))

dat.posILI.NPFS <-
  dat.posILI.NPFS %>%
  dplyr::rename(NPFS_weeks = w,
                age = ageGP.fac) %>%
  merge(dates_lookup, by = "NPFS_weeks") %>%
  mutate(age = revalue(age, c("1-4" = "04",
                              "5-14" = "514",
                              "15-24" = "1524",
                              "25-44" = "2544",
                              "45-64" = "4564",
                              "65+" = "65.")))


save(dat.posILI.GP, file = "C:/Users/Nathan/Dropbox/i-sense/data cleaned/data_positiveILI_GP_perrine_feb2017.RData")
save(dat.posILI.NPFS, file = "C:/Users/Nathan/Dropbox/i-sense/data cleaned/data_positiveILI_NPFS_perrine_feb2017.RData")


