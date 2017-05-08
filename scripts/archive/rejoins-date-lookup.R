
ILI_2009_2011_long <-
  ILI_2009_2011_long %>%
  select(-weeks_window, -NPFS_weeks_window) %>%
  merge(dates_lookup)

H1N1_long <-
  H1N1_long %>%
  select(-weeks_window, -NPFS_weeks_window) %>%
  merge(dates_lookup)

ILI_diag_long <-
  ILI_diag_long  %>%
  select(-weeks_window, -NPFS_weeks_window) %>%
  merge(dates_lookup)

usersurvey <-
  usersurvey  %>%
  select(-weeks_window, -NPFS_weeks_window) %>%
  merge(dates_lookup)

dat.posILI.GP <-
  dat.posILI.GP  %>%
  select(-weeks_window, -NPFS_weeks_window) %>%
  merge(dates_lookup)

dat.posILI.NPFS <-
  dat.posILI.NPFS  %>%
  select(-weeks_window, -NPFS_weeks_window) %>%
  merge(dates_lookup)

dat.npfs <-
  dat.npfs  %>%
  select(-weeks_window, -NPFS_weeks_window) %>%
  merge(dates_lookup)


save(ILI_2009_2011_long, file = "C:/Users/Nathan/Dropbox/i-sense/R/Ilarias-model/H1N1model/data cleaned/ILI_2009_2011_long.RData")
save(H1N1_long, file = "C:/Users/Nathan/Dropbox/i-sense/R/Ilarias-model/H1N1model/data cleaned/H1N1_long.RData")
save(ILI_diag_long, file = "C:/Users/Nathan/Dropbox/i-sense/R/Ilarias-model/H1N1model/data cleaned/ILI_diag_long.RData")
save(usersurvey, file = "C:/Users/Nathan/Dropbox/i-sense/R/user-survey/data cleaned/usersurvey.RData")
save(dat.posILI.GP, file = "C:/Users/Nathan/Dropbox/i-sense/data cleaned/data_positiveILI_GP_perrine_feb2017.RData")
save(dat.posILI.NPFS, file = "C:/Users/Nathan/Dropbox/i-sense/data cleaned/data_positiveILI_NPFS_perrine_feb2017.RData")
save(dat.npfs, file = "C:/Users/Nathan/Dropbox/i-sense/R/NPFS_access-auth-coll/data cleaned/dat.npfs.RData")
