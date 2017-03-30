#
# project: i-sense
# N Green
# March 2017
#
# scenario transition matrices





# scenario 2b -------------------------------------------------------------


trans_mat_2b <-
  trans_mat %>%
  mutate(prob = ifelse(from == "auth_NPFS" & to == "coll",
                       prob + (1 - prob)/2,
                       prob))



# scenario 2c -------------------------------------------------------------

trans_mat2 <-
  trans_mat %>%
  dcast(from + age + NPFS_weeks_window ~ to,
        value.var = "prob") %>%
  mutate(NPFS_H1N1 = ifelse(NPFS_H1N1 == 0,
                            0,
                            ifelse(is.na(NPFS_H1N1),
                                   NA,
                                   NPFS_H1N1 + GP_H1N1/2)),
         NPFS_notH1N1 = ifelse(NPFS_notH1N1 == 0,
                               0,
                               ifelse(is.na(NPFS_notH1N1),
                                      NA,
                                      NPFS_notH1N1 + GP_notH1N1/2)),
         GP_H1N1 = ifelse(NPFS_H1N1 == 0,
                          GP_H1N1,
                          ifelse(is.na(GP_H1N1),
                                 NA,
                                 GP_H1N1/2)),
         GP_notH1N1 = ifelse(NPFS_notH1N1 == 0,
                             GP_notH1N1,
                             ifelse(is.na(GP_notH1N1),
                                    NA,
                                    GP_notH1N1/2))) %>%
  melt(id.vars = c("from", "age", "NPFS_weeks_window"),
       variable.name = "to",
       value.name = "prob") %>%
  select(from, to, everything()) %>%
  filter(complete.cases(.))


# scenario 2d -------------------------------------------------------------

trans_mat2 <-
  trans_mat %>%
  dcast(from + age + NPFS_weeks_window ~ to,
        value.var = "prob") %>%
  mutate(seekcare_H1N1 = NPFS_H1N1 + GP_H1N1,
         seekcare_notH1N1 = NPFS_notH1N1 + GP_notH1N1,

         NPFS_H1N1 = ifelse(NPFS_H1N1 == 0,
                            0,
                            ifelse(is.na(NPFS_H1N1),
                                   NA,
                                   NPFS_H1N1 + seekcare_H1N1)),
         NPFS_notH1N1 = ifelse(NPFS_notH1N1 == 0,
                               0,
                               ifelse(is.na(NPFS_notH1N1),
                                      NA,
                                      NPFS_notH1N1 + seekcare_notH1N1)),
         notseekcare_H1N1 = ifelse(NPFS_H1N1 == 0,
                                   notseekcare_H1N1,
                                   ifelse(is.na(notseekcare_H1N1),
                                          NA,
                                          notseekcare_H1N1 - seekcare_H1N1)),
         notseekcare_notH1N1 = ifelse(NPFS_notH1N1 == 0,
                                      notseekcare_notH1N1,
                                      ifelse(is.na(notseekcare_notH1N1),
                                             NA,
                                             notseekcare_notH1N1 - seekcare_notH1N1))) %>%
  select(-seekcare_H1N1, -seekcare_notH1N1) %>%
  melt(id.vars = c("from", "age", "NPFS_weeks_window"),
       variable.name = "to",
       value.name = "prob") %>%
  select(from, to, everything()) %>%
  filter(complete.cases(.))


