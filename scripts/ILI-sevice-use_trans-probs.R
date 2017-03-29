#
# project: i-sense
# N Green
# Sept 2016
#
# create model inputs for decision tree: upto service use


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


ageGroups <- c("04", "514", "1524", "2544", "4564", "65.")


# duplicate for each week window 1,2,3
pop_age_window <-
  pop_age %>%
  slice(rep(1:n(), each = 3)) %>%
  mutate(NPFS_weeks_window = rep(1:3, times = n()/3))


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

#test array lengths
# lapply(list(ILI_2009_2011_long,
#             H1N1_long,
#             ILI_diag_long,
#             usersurvey,
#             dat.posILI.GP,
#             dat.posILI.NPFS,
#             dat.npfs),
#        FUN = function(x) c(min(x['week']), max(x['week'])))

##TODO## Using an online survey of healthcare-seeking behaviour to estimate () Brooks-Pollock

# FluWatch. (Hayward et al., 2014)
# suppl material p. 7 section 4) (file:///C:/Users/nathan.green.PHE/Dropbox/docs/mmc1%20(1).pdf)
p.seekcare <- data.frame(NPFS_weeks_window = c(1, 2, 3),
                         p.seekcare = c(0.172, 0.172, 0.172))


dat.posILI.GP <-
  dat.posILI.GP %>%
  rename(auth_GP = estim.consult)


# use serological data
p.H1N1 <-
  data.frame(age = ageGroups,
             p.H1N1_baseline = c(0.018, 0.037, 0.175, 0.089, 0.143, 0.23),      # Incidence of 2009 pandemic influenza A H1N1 infection in England: a cross-sectional serological study, Elizabeth Miller
             post_2nd_wave = c(0.37, 0.62, 0.44, 0.33, 0.27, 0.25)) %>%   # Seroprevalence of Influenza A(H1N1) pdm09 Virus Antibody, England, 2010 and 2011, Katja Hoschler
  mutate(p.H1N1 = post_2nd_wave - p.H1N1_baseline)




# swabbing positivity/negativity ------------------------------------------

GP_swab_pos <-
  dat.posILI.GP %>%
  group_by(NPFS_weeks_window, age) %>%
  dplyr::summarise(posILI = sum(posILI),
                   auth_GP = sum(auth_GP)) %>%
  mutate(p.GP_swab_pos = posILI/auth_GP) %>%
  select(-posILI, -auth_GP)

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

ILI_Dorigatti <-
  ILI_2009_2011_long %>%
  group_by(NPFS_weeks_window, age) %>%
  dplyr::summarise(ILI_Dorigatti = sum(ILI_England))


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

auth_GP <-
  dat.posILI.GP %>%
  group_by(NPFS_weeks_window, age) %>%
  dplyr::summarise(auth_GP = sum(auth_GP))


PROP_ILI_SYMP <- 0.16 # FluWatch suppl material p. 6 section 2) (file:///C:/Users/nathan.green.PHE/Dropbox/docs/mmc1%20(1).pdf)
# PROP_ILI_SYMP <- 0.669 # (58.3, 74.5) # Time lines of infection..., Carrat (2008) Human challenge study #potentially bias



# join all  ---------------------------------------------------------------

num_dat_ILI <-
  ILI_Dorigatti %>%
  merge(H1N1_GP, all = TRUE) %>%
  merge(auth_NPFS, all = TRUE) %>%
  merge(auth_GP, all = TRUE) %>%
  merge(GP_swab_pos, all = TRUE) %>%
  merge(NPFS_swab_pos, all = TRUE) %>%
  merge(notseekcare_H1N1, all = TRUE) %>%
  merge(pop_age, all = TRUE) %>%
  merge(p.seekcare) %>%
  merge(p.H1N1)


num_dat_ILI[is.na(num_dat_ILI)] <- 0



# combined estimates ------------------------------------------------------

num_dat_ILI <-
  num_dat_ILI %>%
  mutate(H1N1_GP = auth_GP * p.GP_swab_pos,
         notH1N1_GP = auth_GP * (1 - p.GP_swab_pos),
         H1N1_NPFS = auth_NPFS * p.NPFS_swab_pos,
         notH1N1_NPFS = auth_NPFS * (1 - p.NPFS_swab_pos),

         seekcare = auth_NPFS + auth_GP,
         H1N1_seekcare = H1N1_GP + H1N1_NPFS,
         notH1N1_seekcare = notH1N1_GP + notH1N1_NPFS,

         H1N1 = p.H1N1 * pop,

         Sx_H1N1 = H1N1 * PROP_ILI_SYMP,
         p.seekcare = H1N1_seekcare/Sx_H1N1,
         p.seekcare_Sx = H1N1_seekcare/H1N1,

         Sx = seekcare/p.seekcare,

         Sx.notseekcare_H1N1 = (1 - p.seekcare)*p.GP_swab_pos,
         Sx.notseekcare_notH1N1 = (1 - p.seekcare)*(1 - p.GP_swab_pos),

         flu = Sx/PROP_ILI_SYMP,
         Sx.GP_H1N1 = H1N1_GP/Sx,
         Sx.NPFS_H1N1 = H1N1_NPFS/Sx,
         Sx.NPFS_notH1N1 = notH1N1_NPFS/Sx,
         Sx.GP_notH1N1 = notH1N1_GP/Sx,
         notseekcare_H1N1 = Sx.notseekcare_H1N1*Sx,
         notseekcare_notH1N1 = Sx.notseekcare_notH1N1*Sx,
         flu.Sx = PROP_ILI_SYMP, #Sx/flu,
         pop.flu = p.H1N1) #flu/pop)  #assume that risk of H1N1 and other flu is the same ... ##TODO
                                      #Haywood Flu Watch suppl material says both ~18/19%


