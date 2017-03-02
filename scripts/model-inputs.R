#
# project: i-sense
# N Green
# Sept 2016
#
# create model inputs for decision tree


library(dplyr)
library(tidyr)


# read data

load(file = "C:/Users/Nathan/Dropbox/i-sense/R/Ilarias-model/H1N1model/data cleaned/ILI_2009_2011_long.RData")
load(file = "C:/Users/Nathan/Dropbox/i-sense/R/Ilarias-model/H1N1model/data cleaned/H1N1_long.RData")
load(file = "C:/Users/Nathan/Dropbox/i-sense/R/Ilarias-model/H1N1model/data cleaned/ILI_diag_long.RData")
load(file = "C:/Users/Nathan/Dropbox/i-sense/R/NPFS_access-auth-coll/data cleaned/dat.npfs.RData")
load(file = "C:/Users/Nathan/Dropbox/i-sense/R/user-survey/data cleaned/usersurvey.RData")
load(file = "C:/Users/Nathan/Dropbox/i-sense/data cleaned/data_positiveILI_GP_perrine_feb2017.RData")
load(file = "C:/Users/Nathan/Dropbox/i-sense/data cleaned/data_positiveILI_NPFS_perrine_feb2017.RData")


## are the start and end dates the same for all data sets?
# testthat::is_equivalent_to()

# use the start and end time shortest
ILI_2009_2011_long <- filter(ILI_2009_2011_long, week >= 31, week <= 57)
H1N1_long <- filter(H1N1_long, week >= 31, week <= 57)
ILI_diag_long <- filter(ILI_diag_long, week >= 31, week <= 57)
usersurvey <- filter(usersurvey, week >= 31, week <= 57)
dat.posILI.GP <- filter(dat.posILI.GP, week >= 31, week <= 57)
dat.posILI.NPFS <- filter(dat.posILI.NPFS, week >= 31, week <= 57)
dat.npfs <- filter(dat.npfs, week >= 31, week <= 57)


# swabbing positivity/negativity ------------------------------------------

GP_swab_pos <- dat.posILI.GP %>%
  group_by(NPFS_weeks_window, age) %>%
  dplyr::summarise(posILI = sum(posILI),
                   estim.consult = sum(estim.consult)) %>%
  mutate(p.GP_swab_pos = posILI/estim.consult) %>%
  select(-posILI, -estim.consult)

NPFS_swab_pos <- dat.posILI.NPFS %>%
  group_by(NPFS_weeks_window, age) %>%
  dplyr::summarise(posILI = sum(posILI),
                   authorisations = sum(authorisations)) %>%
  mutate(p.NPFS_swab_pos = posILI/authorisations) %>%
  select(-posILI, -authorisations)


#  ------------------------------------------------------------------------
#  absolute numbers
#  ------------------------------------------------------------------------

# number ILI in England by time windows -----------------------------------

ILI <-
  ILI_2009_2011_long %>%
  group_by(NPFS_weeks_window, age) %>%
  dplyr::summarise(ILI = sum(ILI_England))


# number ILI H1N1 to GP ---------------------------------------------------

H1N1_GP <-
  ILI_diag_long %>%
  group_by(NPFS_weeks_window, age) %>%
  dplyr::summarise(H1N1_GP = sum(num_ILI_H1N1_GP))


# number authorised-ILI NPFS ------------------------------------------------

auth_NPFS <-
  dat.npfs %>%
  group_by(NPFS_weeks_window, age) %>%
  dplyr::summarise(auth_NPFS = sum(auth))


num_dat <-
  ILI %>%
  merge(H1N1_GP) %>%
  merge(auth_NPFS) %>%
  merge(GP_swab_pos) %>%
  merge(NPFS_swab_pos)



# number ILI not H1N1 to GP ------------------------------------------------

num_dat <-
  num_dat %>%
  mutate(notH1N1_GP = H1N1_GP * (1 - p.GP_swab_pos)/p.GP_swab_pos,
         H1N1_NPFS = auth_NPFS * p.NPFS_swab_pos,
         notH1N1_NPFS = auth_NPFS * (1 - p.NPFS_swab_pos))



#  ------------------------------------------------------------------------
#  transition probability matrix
#  ------------------------------------------------------------------------

num_ILI <-
  num_dat %>%
  reshape2::melt(id.vars = c("age", "NPFS_weeks_window"),
                                  variable.name = "to",
                                  value.name = "num")

trans_mat_ILI <-
  num_dat %>%
  mutate(GP.H1N1 = H1N1_GP/ILI,
         NPFS.H1N1 = H1N1_NPFS/ILI,
         NPFS.notH1N1 = notH1N1_NPFS/ILI,
         GP.notH1N1 = notH1N1_GP/ILI) %>%
  reshape2::melt(id.vars = c("age", "NPFS_weeks_window"),
                 variable.name = "to",
                 value.name = "prob",
                 measure.vars = c("GP.H1N1",
                                  "NPFS.H1N1",
                                  "NPFS.notH1N1",
                                  "GP.notH1N1")) %>%
  data.frame(from = "ILI") %>%
  select(from, to, everything()) %>%
  arrange(to)






