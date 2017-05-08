

dat <-
  trans_mat %>%
  filter(age == "04",
         NPFS_weeks_window == 2)


trans_mat %>%
  Ec_by_age_window() %>%
  Ec_pop(pop_age_window) %>%
  sapply(sum, na.rm = TRUE)



