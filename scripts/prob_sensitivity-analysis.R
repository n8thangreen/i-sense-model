#
# project: i-sense
# N Green
# Feb 2017
#
# probabilistic sensitivity analysis


library(triangle)


# duplicate for each week window
pop_age_window <-
  pop_age %>%
  slice(rep(1:n(), each = 3)) %>%
  mutate(NPFS_weeks_window = rep(1:3, times = n()/3))

sim_n <- 50

scenario_sensitivity_TEMPLATE <-
  matrix(data = NA,
         nrow = sim_n,
         ncol = 2) %>%
  set_colnames(c("c", "e")) %>%
  as.data.frame()

PCT_cost <- 10


# scenario 0 (status-quo) -------------------------------------------------

scenario0_sensitivity <- scenario_sensitivity_TEMPLATE

for (i in seq_len(sim_n)){

  scenario0_sensitivity[i, ] <-
    trans_mat %>%
    Ec_by_age_window(c_NPFS = 17,
                     c_GP = rlnorm(1, log(37), log(8.4)),
                     c_collect = 15.41,
                     c_hosp = rlnorm(1, log(839), log(192.1)),
                     c_death = rtriangle(1, 1197, 1900, 1680),

                     # QALY loss
                     Q_hosp = rnorm(1, 0.018, 0.0018),
                     Q_nonhosp = rnorm(1, 0.0082, 0.00085)) %>%
    Ec_pop(pop_age_window) %>%
    sapply(sum, na.rm = TRUE)
}


# scenario 1 (test @ GP only) ----------------------------------------------

scenario1_sensitivity <- scenario_sensitivity_TEMPLATE

for (i in seq_len(sim_n)){

  scenario1_sensitivity[i, ]<-
    trans_mat %>%
    Ec_by_age_window(c_NPFS = 17,
                     c_GP = rlnorm(1, log(37), log(8.4)),
                     c_collect = 15.41,
                     c_hosp = rlnorm(1, log(839), log(192.1)),
                     c_death = rtriangle(1, 1197, 1900, 1680),

                     # QALY loss
                     Q_hosp = rnorm(1, 0.018, 0.0018),
                     Q_nonhosp = rnorm(1, 0.0082, 0.00085),

                     spec_GP = rtriangle(1, 0.93, 0.99),
                     sens_GP = rtriangle(1, 0.67, 0.81),
                     c_testGP = PCT_cost) %>%
    Ec_pop(pop_age_window) %>%
    sapply(sum, na.rm = TRUE)
}





