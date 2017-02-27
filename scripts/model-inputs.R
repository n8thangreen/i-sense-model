#
# project: i-sense
# N Green
# Sept 2016
#
# create model inputs for decision tree


library(dplyr)


# read data

load(file = "C:/Users/Nathan/Dropbox/i-sense/R/Ilarias-model/H1N1model/data cleaned/ILI_2009_2011_long.RData")
load(file = "C:/Users/Nathan/Dropbox/i-sense/R/Ilarias-model/H1N1model/data cleaned/H1N1_long.RData")
load(file = "C:/Users/Nathan/Dropbox/i-sense/R/Ilarias-model/H1N1model/data cleaned/ILI_diag_long.RData")
load(file = "C:/Users/Nathan/Dropbox/i-sense/R/Qsurveillance-GP/data/Qsurveillance.RData")
load(file = "C:/Users/Nathan/Dropbox/i-sense/R/NPFS_access-auth-coll/data cleaned/dat.npfs.RData")


## are the start and end dates the same for all data sets?
# testthat::is_equivalent_to()

# use the start and end time from Qsurveillance since this is the shortest
# ILI_2009_2011_long <- filter(ILI_2009_2011_long, week >= 18, week <= 57)
# H1N1_long <- filter(H1N1_long, week >= 18, week <= 57)
# ILI_diag_long <- filter(ILI_diag_long, week >= 18, week <= 57)

GP_swab_neg <- 1
GP_swab_pos <- 1
NPFS_swab_pos <- 1
NPFS_swab_neg <- 1
NPFS_swab <- 2


#  ------------------------------------------------------------------------
#  absolute numbers
#  ------------------------------------------------------------------------

# number ILI in England by time windows -----------------------------------

num_ILI_England_windows <- filter(ILI_2009_2011_long, age == "overall") %>%
                            group_by(NPFS_weeks_window) %>%
                            dplyr::summarise(num_ILI_windows = sum(ILI_England))


# number ILI H1N1 to GP ---------------------------------------------------

num_ILI_H1N1_GP_windows <- filter(ILI_diag_long, age == "overall") %>%
                            group_by(NPFS_weeks_window) %>%
                            dplyr::summarise(num_ILI_H1N1_GP_windows = sum(num_ILI_H1N1_GP))


# number authorised ILI NPFS ------------------------------------------------

num_auth_NPFS_windows <- filter(dat.npfs, age == "total") %>%
                          group_by(NPFS_weeks_window) %>%
                          dplyr::summarise(num_auth_NPFS_windows = sum(auth))


# number ILI not H1N1 to GP ------------------------------------------------

num_ILI_notH1N1_GP_windows <- num_ILI_H1N1_GP_windows * GP_swab_neg/GP_swab_pos


# number auth ILI H1N1 NPFS -----------------------------------------------

num_ILI_H1N1_NPFS_windows <- num_auth_NPFS_windows * NPFS_swab_pos/NPFS_swab


# number auth ILI not H1N1 NPFS -------------------------------------------

num_ILI_notH1N1_NPFS_windows <- num_auth_NPFS_windows * NPFS_swab_neg/NPFS_swab


#  ------------------------------------------------------------------------
#  probabilities
#  ------------------------------------------------------------------------

# ILI in England by time windows ------------------------------------------

# p_ILI_England_windows <- num_ILI_England_windows/num_England


# ILI H1N1 to GP ----------------------------------------------------------

p_ILI_H1N1_GP_windows <- num_ILI_H1N1_GP_windows/num_ILI_England_windows


# authorised ILI H1N1 NPFS ---------------------------------------------------

p_ILI_H1N1_NPFS_windows <- num_ILI_H1N1_NPFS_windows/num_ILI_England_windows


# authorised ILI not H1N1 NPFS -----------------------------------------------

p_ILI_notH1N1_NPFS_windows <- num_ILI_notH1N1_NPFS_windows/num_ILI_England_windows


# number ILI not H1N1 to GP ------------------------------------------------

p_ILI_notH1N1_GP_windows <- num_ILI_notH1N1_GP_windows/num_ILI_England_windows


