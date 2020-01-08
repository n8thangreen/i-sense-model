#
# project: i-sense
# N Green
# March 2017
#
# population counts for each scenario


SPEC <- 0.96
SENS <- 0.73


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
  E_num_by_age_window(spec_GP = SPEC,
                      sens_GP = SENS,
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
  E_num_by_age_window(spec_NPFS = SPEC,
                      sens_NPFS = SENS,
                      c_testNPFS = 20) %>%
  E_num_pop(pop_age_window) %>%
  lapply(function(x)
    data.frame(window = rownames(x),
               x, check.names = FALSE) %>% melt())

scenario2a_counts <- multimerge(scenario2a_counts, c("variable","window"))

write.csv(scenario2a_counts, "../../data cleaned/scenario2a_counts_table.csv")


# scenario 2b (test @ NPFS only AND obtain Rx increase) ----------------------


scenario2b_counts <-
  trans_mat_2b %>%
  E_num_by_age_window(spec_NPFS = SPEC,
                      sens_NPFS = SENS,
                      c_testNPFS = 20) %>%
  E_num_pop(pop_age_window) %>%
  lapply(function(x)
    data.frame(window = rownames(x),
               x, check.names = FALSE) %>% melt())

scenario2b_counts <- multimerge(scenario2b_counts, c("variable","window"))

write.csv(scenario2b_counts, "../../data cleaned/scenario2b_counts_table.csv")



# scenario 2c (test @ NPFS only AND switch from GP to NPFS) ---------------------


scenario2c_counts <-
  trans_mat_2c %>%
  E_num_by_age_window(spec_NPFS = SPEC,
                      sens_NPFS = SENS,
                      c_testNPFS = 20) %>%
  E_num_pop(pop_age_window) %>%
  lapply(function(x)
    data.frame(window = rownames(x),
               x, check.names = FALSE) %>% melt())

scenario2c_counts <- multimerge(scenario2c_counts, c("variable","window"))

write.csv(scenario2c_counts, "../../data cleaned/scenario2c_counts_table.csv")


# scenario 2d (test @ NPFS only AND NPFS use increase) -----------------------------------------


scenario2d_counts <-
  trans_mat_2d %>%
  E_num_by_age_window(spec_NPFS = SPEC,
                      sens_NPFS = SENS,
                      c_testNPFS = 20) %>%
  E_num_pop(pop_age_window) %>%
  lapply(function(x)
    data.frame(window = rownames(x),
               x, check.names = FALSE) %>% melt())

scenario2d_counts <- multimerge(scenario2d_counts, c("variable","window"))


write.csv(scenario2d_counts, "../../data cleaned/scenario2d_counts_table.csv")


