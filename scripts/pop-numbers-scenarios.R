#
# project: i-sense
# N Green
# March 2017
#
# population counts for each scenario


multimerge <- function(LIST, BY){

  out <- LIST[[1]]

  for (i in seq_along(LIST)[-1]) {

    out <- merge(out, LIST[[i]], by = BY)
  }

  names(out)[!names(out) %in% c("window","variable")] <- names(LIST)

  return(out)
}


# scenario 0 (status-quo) -------------------------------------------------

nums_list <-
  trans_mat %>%
  E_num_by_age_window() %>%
  E_num_pop(pop_age_window) %>%
  lapply(function(x)
    data.frame(window = rownames(x),
               x, check.names = FALSE) %>% melt())

flat_num <- multimerge(nums_list, c("variable","window"))

write.csv(flat_num, "../../data cleaned/flat_num.csv")


scenario0 <-
  trans_mat %>%
  E_num_by_age_window() %>%
  E_num_pop(pop_age_window) %>%
  sapply(sum, na.rm = TRUE) %>%
  format(digits = 3)


# scenario 1 (test @ GP only) ----------------------------------------------

scenario1 <-
  trans_mat %>%
  E_num_by_age_window(spec_GP = 0.64,
                      sens_GP = 0.96,
                      c_testGP = 20) %>%
  E_num_pop(pop_age_window) %>%
  sapply(sum, na.rm = TRUE) %>%
  format(digits = 3)


# scenario 2a (test @ NPFS only) ---------------------------------------------

scenario2a <-
  trans_mat %>%
  E_num_by_age_window(spec_NPFS = 0.64,
                      sens_NPFS = 0.96,
                      c_testNPFS = 20) %>%
  E_num_pop(pop_age_window) %>%
  sapply(sum, na.rm = TRUE) %>%
  format(digits = 3)


# scenario 2b (test @ NPFS only AND obtain Rx increase) ----------------------

trans_mat2 <-
  trans_mat %>%
  mutate(prob = ifelse(from == "NPFS" & to == "coll",
                       prob + (1 - prob)/2,
                       prob))

scenario2b <-
  trans_mat2 %>%
  E_num_by_age_window(spec_NPFS = 0.64,
                      sens_NPFS = 0.96,
                      c_testNPFS = 20) %>%
  E_num_pop(pop_age_window) %>%
  sapply(sum, na.rm = TRUE) %>%
  format(digits = 3)


# scenario 2c (test @ NPFS only AND switch from GP to NPFS) ---------------------

trans_mat2 <-
  trans_mat %>%
  dcast(from + age + NPFS_weeks_window ~ to,
        value.var = "prob") %>%
  mutate(NPFS_H1N1 = NPFS_H1N1 + GP_H1N1/2,
         NPFS_notH1N1 = NPFS_notH1N1 + GP_notH1N1/2,
         GP_H1N1 = GP_H1N1/2,
         GP_notH1N1 = GP_notH1N1/2) %>%
  melt(id.vars = c("from", "age", "NPFS_weeks_window"),
       variable.name = "to",
       value.name = "prob") %>%
  select(from, to, everything()) %>%
  filter(complete.cases(.))

scenario2c <-
  trans_mat2 %>%
  E_num_by_age_window(spec_NPFS = 0.64,
                      sens_NPFS = 0.96,
                      c_testNPFS = 20) %>%
  E_num_pop(pop_age_window) %>%
  sapply(sum, na.rm = TRUE) %>%
  format(digits = 3)


# scenario 2d (test @ NPFS only AND NPFS use increase) -----------------------------------------

trans_mat2 <-
  trans_mat %>%
  dcast(from + age + NPFS_weeks_window ~ to,
        value.var = "prob") %>%
  mutate(NPFS_H1N1 = NPFS_H1N1 + (1 - NPFS_H1N1 - GP_H1N1 - NPFS_notH1N1 - GP_notH1N1)*(NPFS_H1N1 + GP_H1N1),
         NPFS_notH1N1 = NPFS_notH1N1 + (1 - NPFS_H1N1 - GP_H1N1 - NPFS_notH1N1 - GP_notH1N1)*(NPFS_notH1N1 + GP_notH1N1)) %>%
  melt(id.vars = c("from", "age", "NPFS_weeks_window"),
       variable.name = "to",
       value.name = "prob") %>%
  select(from, to, everything()) %>%
  filter(complete.cases(.))

scenario2d <-
  trans_mat2 %>%
  E_num_by_age_window(spec_NPFS = 0.64,
                      sens_NPFS = 0.96,
                      c_testNPFS = 20) %>%
  E_num_pop(pop_age_window) %>%
  sapply(sum, na.rm = TRUE) %>%
  format(digits = 3)



