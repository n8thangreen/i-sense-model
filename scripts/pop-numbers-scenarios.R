#
# project: i-sense
# N Green
# March 2017
#
# population counts for each scenario


library(magrittr)



# scenario 0 (status-quo) -------------------------------------------------

scenario0_counts <-
  trans_mat %>%
  E_num_by_age_window() %>%
  E_num_pop(pop_age_window) %>%
  lapply(function(x)
    data.frame(window = rownames(x),
               x, check.names = FALSE) %>% melt())

scenario0_counts <- multimerge(scenario0_counts, c("variable","window"))

write.csv(scenario0_counts, "../../data cleaned/scenario0_counts_table.csv")


# scenario 1 (test @ GP only) ----------------------------------------------

scenario1_counts <-
  trans_mat %>%
  E_num_by_age_window(spec_GP = 0.96,
                      sens_GP = 0.73,
                      c_testGP = 20,
                      p_GP.Rx = 1) %>%
  E_num_pop(pop_age_window) %>%
  lapply(function(x)
    data.frame(window = rownames(x),
               x, check.names = FALSE) %>% melt())

scenario1_counts <- multimerge(scenario1_counts, c("variable","window"))

write.csv(scenario1_counts, "../../data cleaned/scenario1_counts_table.csv")



# scenario 2a (test @ NPFS only) ---------------------------------------------

scenario2a_counts <-
  trans_mat %>%
  E_num_by_age_window(spec_NPFS = 0.96,
                      sens_NPFS = 0.73,
                      c_testNPFS = 20) %>%
  E_num_pop(pop_age_window) %>%
  lapply(function(x)
    data.frame(window = rownames(x),
               x, check.names = FALSE) %>% melt())

scenario2a_counts <- multimerge(scenario2a_counts, c("variable","window"))

write.csv(scenario2a_counts, "../../data cleaned/scenario2a_counts_table.csv")


# scenario 2b (test @ NPFS only AND obtain Rx increase) ----------------------

trans_mat_2b <-
  trans_mat %>%
  mutate(prob = ifelse(from == "auth_NPFS" & to == "coll",
                       prob + (1 - prob)/2,
                       prob))


scenario2b_counts <-
  trans_mat_2b %>%
  E_num_by_age_window(spec_NPFS = 0.96,
                      sens_NPFS = 0.73,
                      c_testNPFS = 20) %>%
  E_num_pop(pop_age_window) %>%
  lapply(function(x)
    data.frame(window = rownames(x),
               x, check.names = FALSE) %>% melt())

scenario2b_counts <- multimerge(scenario2b_counts, c("variable","window"))

write.csv(scenario2b_counts, "../../data cleaned/scenario2b_counts_table.csv")



# scenario 2c (test @ NPFS only AND switch from GP to NPFS) ---------------------

trans_mat_2c <-
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

scenario2c_counts <-
  trans_mat_2c %>%
  E_num_by_age_window(spec_NPFS = 0.96,
                      sens_NPFS = 0.73,
                      c_testNPFS = 20) %>%
  E_num_pop(pop_age_window) %>%
  lapply(function(x)
    data.frame(window = rownames(x),
               x, check.names = FALSE) %>% melt())

scenario2c_counts <- multimerge(scenario2c_counts, c("variable","window"))

write.csv(scenario2c_counts, "../../data cleaned/scenario2c_counts_table.csv")


# scenario 2d (test @ NPFS only AND NPFS use increase) -----------------------------------------

trans_mat_2d <-
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


scenario2d_counts <-
  trans_mat_2d %>%
  E_num_by_age_window(spec_NPFS = 0.96,
                      sens_NPFS = 0.73,
                      c_testNPFS = 20) %>%
  E_num_pop(pop_age_window) %>%
  lapply(function(x)
    data.frame(window = rownames(x),
               x, check.names = FALSE) %>% melt())

scenario2d_counts <- multimerge(scenario2d_counts, c("variable","window"))


write.csv(scenario2d_counts, "../../data cleaned/scenario2d_counts_table.csv")



##test
scenario_counts <-
  trans_mat_2d %>%
  E_num_by_age_window(spec_NPFS = 0.96,
                      sens_NPFS = 0.7,
                      c_testNPFS = 20) %>%
  E_num_pop(pop_age_window) %>%
  lapply(function(x)
    data.frame(window = rownames(x),
               x, check.names = FALSE) %>% melt())

scenario_counts <- multimerge(scenario_counts, c("variable","window"))
