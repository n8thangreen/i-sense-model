#
# project: i-sense
# N Green
# Sept 2016
#
# create model inputs for decision tree


library(dplyr)
library(tidyr)


# read data

load(file = "../../R/Ilarias-model/H1N1model/data cleaned/ILI_2009_2011_long.RData")
load(file = "../../R/Ilarias-model/H1N1model/data cleaned/H1N1_long.RData")
load(file = "../../R/Ilarias-model/H1N1model/data cleaned/ILI_diag_long.RData")
load(file = "../../R/NPFS_access-auth-coll/data cleaned/dat.npfs.RData")
load(file = "../../R/user-survey/data cleaned/usersurvey.RData")
load(file = "../../data cleaned/data_positiveILI_GP_perrine_feb2017.RData")
load(file = "../../data cleaned/data_positiveILI_NPFS_perrine_feb2017.RData")
load(file = "../../R/Ilarias-model/H1N1model/data/pop_age.RData")


## are the start and end dates the same for all data sets?
# testthat::is_equivalent_to()

# use the start and end time shortest
FIRST_WEEK <- 20
LAST_WEEK <- 57

ILI_2009_2011_long <- filter(ILI_2009_2011_long, week >= FIRST_WEEK, week <= LAST_WEEK)
H1N1_long <- filter(H1N1_long, week >= FIRST_WEEK, week <= LAST_WEEK)
ILI_diag_long <- filter(ILI_diag_long, week >= FIRST_WEEK, week <= LAST_WEEK)
usersurvey <- filter(usersurvey, week >= FIRST_WEEK, week <= LAST_WEEK)
dat.posILI.GP <- filter(dat.posILI.GP, week >= FIRST_WEEK, week <= LAST_WEEK)
dat.posILI.NPFS <- filter(dat.posILI.NPFS, week >= FIRST_WEEK, week <= LAST_WEEK)
dat.npfs <- filter(dat.npfs, week >= FIRST_WEEK, week <= LAST_WEEK)


##TODO## Using an online survey of healthcare-seeking behaviour to estimate () Brooks-Pollock

# FluWatch. (Hayward et al., 2014)
p.seekcare <- data.frame(NPFS_weeks_window = c(1, 2, 3),
                         p.seekcare = c(0.172, 0.172, 0.172))


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


# number ILI, all flu in pop -----------------------------------------------

PROP_ILI_SYMP <- 0.669 # (58.3, 74.5) # Time lines of infection..., Carrat (2008)


auth_NPFS <-
  auth_NPFS %>%
  merge(p.seekcare) %>%
  mutate(Sx_NPFS = auth_NPFS/p.seekcare,
         flu_NPFS = Sx_NPFS/PROP_ILI_SYMP) %>%
  select(-p.seekcare)

estim.consult <-
  estim.consult %>%
  merge(p.seekcare) %>%
  mutate(Sx_GP = estim.consult/p.seekcare,
         flu_GP = Sx_GP/PROP_ILI_SYMP) %>%
  select(-p.seekcare)



# prop H1N1 Sx who don't seek care -------------------------------------------

notseekcare_H1N1 <-
  p.seekcare %>%
  merge(GP_swab_pos,
        by = "NPFS_weeks_window") %>%
  mutate(Sx.notseekcare_H1N1 = (1 - p.seekcare)*p.GP_swab_pos) %>%
  select(-p.seekcare, -p.GP_swab_pos)



# join all  ---------------------------------------------------------------

num_dat_ILI <-
  ILI %>%
  merge(H1N1_GP, all = TRUE) %>%
  merge(auth_NPFS, all = TRUE) %>%
  merge(estim.consult, all = TRUE) %>%
  merge(GP_swab_pos, all = TRUE) %>%
  merge(NPFS_swab_pos, all = TRUE) %>%
  merge(notseekcare_H1N1, all = TRUE) %>%
  merge(pop_age, all = TRUE)


num_dat_ILI[is.na(num_dat_ILI)] <- 0


# number ILI not/H1N1 to GP/NPFS -------------------------------------------

num_dat_ILI <-
  num_dat_ILI %>%
  mutate(H1N1_GP = estim.consult * p.GP_swab_pos,
         notH1N1_GP = estim.consult * (1 - p.GP_swab_pos),
         H1N1_NPFS = auth_NPFS * p.NPFS_swab_pos,
         notH1N1_NPFS = auth_NPFS * (1 - p.NPFS_swab_pos),
         Sx = Sx_GP + Sx_NPFS,
         flu = flu_GP + flu_NPFS,
         Sx.GP_H1N1 = H1N1_GP/Sx,
         Sx.NPFS_H1N1 = H1N1_NPFS/Sx,
         Sx.NPFS_notH1N1 = notH1N1_NPFS/Sx,
         Sx.GP_notH1N1 = notH1N1_GP/Sx,
         flu.Sx = Sx/flu,
         pop.flu = flu/pop)


#  ------------------------------------------------------------------------
#  transition probability matrix
#  ------------------------------------------------------------------------

trans_mat_ILI <-
  num_dat_ILI %>%
  reshape2::melt(id.vars = c("age", "NPFS_weeks_window"),
                 variable.name = "fromto",
                 value.name = "prob",
                 measure.vars = c("Sx.GP_H1N1",
                                  "Sx.NPFS_H1N1",
                                  "Sx.NPFS_notH1N1",
                                  "Sx.GP_notH1N1",
                                  "Sx.notseekcare_H1N1",
                                  "flu.Sx",
                                  "pop.flu")) %>%
  separate(fromto, c("from", "to"), "\\.") %>%
  select(from, to, everything()) %>%
  arrange(to)


