#
# project: i-sense
# N Green
# March 2017
#
# cost-effectiveness scenarios


spec.seq  <- seq(0, 1, 0.1)#0.05)
sens.seq  <- seq(0, 1, 0.1)#0.05)
c_test.seq <- seq(0, 25, 1)#0.5)

scenario_TEMPLATE <- array(data = NA,
                           dim = c(length(spec.seq), length(sens.seq), length(c_test.seq), 2),
                           dimnames = list(spec.seq, sens.seq, c_test.seq, c("c","e")))

# scenario 0 (status-quo) -------------------------------------------------

scenario0 <-
  trans_mat %>%
  Ec_by_age_window() %>%
  Ec_pop(pop = pop_age_window) %>%
  sapply(sum, na.rm = TRUE)


# scenario 1a (test @ GP only) ----------------------------------------------
# GP prescribe on rapid test only

scenario1a <- scenario_TEMPLATE

for (i in seq_along(spec.seq)) {
  for (j in seq_along(sens.seq)) {
    for (k in seq_along(c_test.seq)) {

      scenario1a[i,j,k, ] <-
        trans_mat %>%
        Ec_by_age_window(spec_GP = spec.seq[i],
                         sens_GP = sens.seq[j],
                         c_testGP = c_test.seq[k],
                         p_GP.Rx = 1) %>%
        Ec_pop(pop_age_window) %>%
        sapply(sum, na.rm = TRUE)
    }
  }
}

save(scenario1a, file = "../../data cleaned/scenario1a_costeffective.RData")


# scenario 1b (test @ GP & obtain Rx increase) --------------------------------
# GP prescribe on rapid test only

scenario1b <- scenario_TEMPLATE

for (i in seq_along(spec.seq)) {
  for (j in seq_along(sens.seq)) {
    for (k in seq_along(c_test.seq)) {

      scenario1b[i,j,k, ] <-
        trans_mat_1b %>%
        Ec_by_age_window(spec_GP = spec.seq[i],
                         sens_GP = sens.seq[j],
                         c_testGP = c_test.seq[k],
                         p_GP.Rx = 1) %>%
        Ec_pop(pop_age_window) %>%
        sapply(sum, na.rm = TRUE)
    }
  }
}

save(scenario1b, file = "../../data cleaned/scenario1b_costeffective.RData")


# scenario 2a (test @ NPFS only) ---------------------------------------------

scenario2a <- scenario_TEMPLATE

for (i in seq_along(spec.seq)) {
  for (j in seq_along(sens.seq)) {
    for (k in seq_along(c_test.seq)) {

      scenario2a[i,j,k, ] <-
        trans_mat %>%
        Ec_by_age_window(spec_NPFS = spec.seq[i],
                         sens_NPFS = sens.seq[j],
                         c_testNPFS = c_test.seq[k]) %>%
        Ec_pop(pop_age_window) %>%
        sapply(sum, na.rm = TRUE)
    }
  }
}

save(scenario2a, file = "../../data cleaned/scenario2a_costeffective.RData")


# scenario 2b (test @ NPFS only AND obtain Rx increase) ----------------------


scenario2b <- scenario_TEMPLATE

for (i in seq_along(spec.seq)) {
  for (j in seq_along(sens.seq)) {
    for (k in seq_along(c_test.seq)) {

      scenario2b[i,j,k, ] <-
        trans_mat_2b %>%
        Ec_by_age_window(spec_NPFS = spec.seq[i],
                         sens_NPFS = sens.seq[j],
                         c_testNPFS = c_test.seq[k]) %>%
        Ec_pop(pop_age_window) %>%
        sapply(sum, na.rm = TRUE)
    }
  }
}

save(scenario2b, file = "../../data cleaned/scenario2b_costeffective.RData")


# scenario 2c (test @ NPFS only AND switch from GP to NPFS) ---------------------
## assume 1/2 switch


scenario2c <- scenario_TEMPLATE

for (i in seq_along(spec.seq)) {
  for (j in seq_along(sens.seq)) {
    for (k in seq_along(c_test.seq)) {

      scenario2c[i,j,k, ] <-
        trans_mat_2c %>%
        Ec_by_age_window(spec_NPFS = spec.seq[i],
                         sens_NPFS = sens.seq[j],
                         c_testNPFS = c_test.seq[k]) %>%
        Ec_pop(pop_age_window) %>%
        sapply(sum, na.rm = TRUE)
    }
  }
}

save(scenario2c, file = "../../data cleaned/scenario2c_costeffective.RData")


# scenario 2d (test @ NPFS only AND NPFS use increase) -----------------------------------------
## assume same prop who already seek care


scenario2d <- scenario_TEMPLATE

for (i in seq_along(spec.seq)) {
  for (j in seq_along(sens.seq)) {
    for (k in seq_along(c_test.seq)) {

      scenario2d[i,j,k, ] <-
        trans_mat_2d %>%
        Ec_by_age_window(spec_NPFS = spec.seq[i],
                         sens_NPFS = sens.seq[j],
                         c_testNPFS = c_test.seq[k]) %>%
        Ec_pop(pop_age_window) %>%
        sapply(sum, na.rm = TRUE)
    }
  }
}

save(scenario2d, file = "../../data cleaned/scenario2d_costeffective.RData")



# scenario 2e (combine NPFS scenarios) -----------------------------------------


scenario2e <- scenario_TEMPLATE

for (i in seq_along(spec.seq)) {
  for (j in seq_along(sens.seq)) {
    for (k in seq_along(c_test.seq)) {

      scenario2e[i,j,k, ] <-
        trans_mat_2e %>%
        Ec_by_age_window(spec_NPFS = spec.seq[i],
                         sens_NPFS = sens.seq[j],
                         c_testNPFS = c_test.seq[k]) %>%
        Ec_pop(pop_age_window) %>%
        sapply(sum, na.rm = TRUE)
    }
  }
}

save(scenario2e, file = "../../data cleaned/scenario2e_costeffective.RData")


# scenario 3 (combine NPFS and GP scenarios) ------------------------------------


scenario3 <- scenario_TEMPLATE

for (i in seq_along(spec.seq)) {
  for (j in seq_along(sens.seq)) {
    for (k in seq_along(c_test.seq)) {

      scenario3[i,j,k, ] <-
        trans_mat_3 %>%
        Ec_by_age_window(spec_GP = spec.seq[i],
                         sens_GP = sens.seq[j],
                         c_testGP = c_test.seq[k],
                         p_GP.Rx = 1,
                         spec_NPFS = spec.seq[i],
                         sens_NPFS = sens.seq[j],
                         c_testNPFS = c_test.seq[k]) %>%
        Ec_pop(pop_age_window) %>%
        sapply(sum, na.rm = TRUE)
    }
  }
}

save(scenario3, file = "../../data cleaned/scenario3_costeffective.RData")

