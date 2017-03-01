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
load(file = "C:/Users/Nathan/Dropbox/i-sense/R/Qsurveillance-GP/data/Qsurveillance.RData")
load(file = "C:/Users/Nathan/Dropbox/i-sense/R/NPFS_access-auth-coll/data cleaned/dat.npfs.RData")
load(file = "C:/Users/Nathan/Dropbox/i-sense/R/user-survey/data cleaned/usersurvey.RData")
load(file = "C:/Users/Nathan/Dropbox/i-sense/data cleaned/data_positiveILI_GP_perrine_feb2017.RData")
load(file = "C:/Users/Nathan/Dropbox/i-sense/data cleaned/data_positiveILI_NPFS_perrine_feb2017.RData")


## are the start and end dates the same for all data sets?
# testthat::is_equivalent_to()

# use the start and end time from Qsurveillance since this is the shortest
# ILI_2009_2011_long <- filter(ILI_2009_2011_long, week >= 18, week <= 57)
# H1N1_long <- filter(H1N1_long, week >= 18, week <= 57)
# ILI_diag_long <- filter(ILI_diag_long, week >= 18, week <= 57)


# swabbing positivity/negativity ------------------------------------------

GP_swab_pos <- dat.posILI.GP %>%
  group_by(NPFS_weeks_window, age) %>%
  dplyr::summarise(posILI = sum(posILI),
                   estim.consult = sum(estim.consult)) %>%
  mutate(GP_swab_pos = posILI/estim.consult)

NPFS_swab_pos <- dat.posILI.NPFS %>%
  group_by(NPFS_weeks_window, age) %>%
  dplyr::summarise(posILI = sum(posILI),
                   authorisations = sum(authorisations)) %>%
  mutate(NPFS_swab_pos = posILI/authorisations)


#  ------------------------------------------------------------------------
#  absolute numbers
#  ------------------------------------------------------------------------

# number ILI in England by time windows -----------------------------------

ILI <- ILI_2009_2011_long %>%
            group_by(NPFS_weeks_window, age) %>%
            dplyr::summarise(ILI = sum(ILI_England))


# number ILI H1N1 to GP ---------------------------------------------------

H1N1_GP <- ILI_diag_long %>%
                        group_by(NPFS_weeks_window, age) %>%
                        dplyr::summarise(H1N1_GP = sum(num_ILI_H1N1_GP))


# number authorised ILI NPFS ------------------------------------------------

auth_NPFS <- dat.npfs %>%
                      group_by(NPFS_weeks_window, age) %>%
                      dplyr::summarise(auth_NPFS = sum(auth))


num_dat <- merge(ILI,
                  H1N1_GP) %>%
                  merge(auth_NPFS) %>%
                  merge(GP_swab_pos) %>%
                  merge(NPFS_swab_pos)



# number ILI not H1N1 to GP ------------------------------------------------

num_dat <- num_dat %>%
  mutate(notH1N1_GP = H1N1_GP * (1 - GP_swab_pos)/GP_swab_pos,
         H1N1_NPFS = auth_NPFS * NPFS_swab_pos/authorisations,
         notH1N1_NPFS = auth_NPFS * (1 - NPFS_swab_pos)/authorisations)



#  ------------------------------------------------------------------------
#  transition probability matrix
#  ------------------------------------------------------------------------

trans_mat_ILI <- num_dat %>% melt(id.vars = c("age", "NPFS_weeks_window"),
                                  variable.name = "to",
                                  value.name = "num")

trans_mat_ILI <-
  num_dat %>%
  mutate(H1N1_GP = H1N1_GP/ILI,
         H1N1_NPFS = H1N1_NPFS/ILI,
         H1N1_NPFS = H1N1_NPFS/ILI,
         notH1N1_NPFS = notH1N1_NPFS/ILI,
         notH1N1_GP = notH1N1_GP/ILI) %>%
  melt(id.vars = c("age", "NPFS_weeks_window"),
       variable.name = "to",
       value.name = "prob") %>%
  merge(trans_mat,
        by = c("age", "NPFS_weeks_window", "to"),
        all.x = TRUE, all.y = FALSE) %>%
  data.frame(from = "ILI") %>%
  select(from, to, everything()) %>%
  arrange(to)

