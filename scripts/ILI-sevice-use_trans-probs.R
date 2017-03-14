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
load("C:/Users/Nathan/Dropbox/i-sense/R/Ilarias-model/H1N1model/data/pop_age.RData")


## are the start and end dates the same for all data sets?
# testthat::is_equivalent_to()

# use the start and end time shortest
# week >= 31, week <= 57)
FIRST_WEEK <- 20
LAST_WEEK <- 57

ILI_2009_2011_long <- filter(ILI_2009_2011_long, week >= FIRST_WEEK, week <= LAST_WEEK)
H1N1_long <- filter(H1N1_long, week >= FIRST_WEEK, week <= LAST_WEEK)
ILI_diag_long <- filter(ILI_diag_long, week >= FIRST_WEEK, week <= LAST_WEEK)
usersurvey <- filter(usersurvey, week >= FIRST_WEEK, week <= LAST_WEEK)
dat.posILI.GP <- filter(dat.posILI.GP, week >= FIRST_WEEK, week <= LAST_WEEK)
dat.posILI.NPFS <- filter(dat.posILI.NPFS, week >= FIRST_WEEK, week <= LAST_WEEK)
dat.npfs <- filter(dat.npfs, week >= FIRST_WEEK, week <= LAST_WEEK)


p.seekcare <- data.frame(NPFS_weeks_window = c(1,2,3),
                         p.seekcare = c(0.3, 0.5, 0.5))


# swabbing positivity/negativity ------------------------------------------

GP_swab_pos <-
  dat.posILI.GP %>%
  group_by(NPFS_weeks_window, age) %>%
  dplyr::summarise(posILI = sum(posILI),
                   estim.consult = sum(estim.consult)) %>%
  mutate(p.GP_swab_pos = posILI/estim.consult) %>%
  select(-posILI, -estim.consult)

NPFS_swab_pos <-
  dat.posILI.NPFS %>%
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


# number estimated consultations-ILI GP ------------------------------------

estim.consult <-
  dat.posILI.GP %>%
  group_by(NPFS_weeks_window, age) %>%
  dplyr::summarise(estim.consult = sum(estim.consult))


# 'fudge' so that denominator is total ILI at GP, NPFS ----------------------

total_service <-
  merge(dat.posILI.GP, dat.posILI.NPFS,
        by = c("age", "week", "NPFS_weeks_window")) %>%
  mutate(total_service = authorisations + estim.consult) %>%
  group_by(NPFS_weeks_window, age) %>%
  dplyr::summarise(total_service = sum(total_service))


# number ILI, all flu in pop -----------------------------------------------

auth_NPFS <-
  auth_NPFS %>%
  merge(p.seekcare) %>%
  mutate(Sx_NPFS = auth_NPFS/p.seekcare,
         flu_NPFS = Sx_NPFS/0.669) %>%
  select(-p.seekcare)

estim.consult <-
  estim.consult %>%
  merge(p.seekcare) %>%
  mutate(Sx_GP = estim.consult/p.seekcare,
         flu_GP = Sx_GP/0.669) %>%
  select(-p.seekcare)


# join all  ---------------------------------------------------------------

num_dat <-
  ILI %>%
  merge(H1N1_GP, all = TRUE) %>%
  merge(auth_NPFS, all = TRUE) %>%
  merge(estim.consult, all = TRUE) %>%
  merge(GP_swab_pos, all = TRUE) %>%
  merge(NPFS_swab_pos, all = TRUE) %>%
  merge(pop_age, all = TRUE)


num_dat[is.na(num_dat)] <- 0


# number ILI not/H1N1 to GP/NPFS -------------------------------------------

num_dat <-
  num_dat %>%
  mutate(H1N1_GP = estim.consult * p.GP_swab_pos,
         notH1N1_GP = estim.consult * (1 - p.GP_swab_pos),
         H1N1_NPFS = auth_NPFS * p.NPFS_swab_pos,
         notH1N1_NPFS = auth_NPFS * (1 - p.NPFS_swab_pos))


num_dat <-
  num_dat %>%
  mutate(Sx = Sx_GP + Sx_NPFS,
         flu = flu_GP + flu_NPFS)


#  ------------------------------------------------------------------------
#  transition probability matrix
#  ------------------------------------------------------------------------


trans_mat_ILI <-
  num_dat %>%
  mutate(GP.H1N1 = H1N1_GP/Sx,
         NPFS.H1N1 = H1N1_NPFS/Sx,
         NPFS.notH1N1 = notH1N1_NPFS/Sx,
         GP.notH1N1 = notH1N1_GP/Sx,
         p.pop_flu = flu/pop) %>%
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


