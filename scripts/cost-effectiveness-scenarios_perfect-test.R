#
# project: i-sense
# N Green
# March 2017
#
# cost-effectiveness scenarios for a perfect performance test


library(magrittr)


c_test.seq <- seq(0, 30, 1)

scenario_TEMPLATE <- array(data = NA,
                           dim = c(length(c_test.seq), 2),
                           dimnames = list(c_test.seq, c("c","e")))

# scenario 0 (status-quo) -------------------------------------------------

scenario0 <-
  trans_mat %>%
  Ec_by_age_window() %>%
  Ec_pop(pop = pop_age_window) %>%
  sapply(sum, na.rm = TRUE)


# scenario 1a (test @ GP only) ----------------------------------------------
# GP prescribe on rapid test only

scenario1a <- scenario_TEMPLATE

for (k in seq_along(c_test.seq)) {

  scenario1a[k,] <-
    trans_mat %>%
    Ec_by_age_window(spec_GP = 1,
                     sens_GP = 1,
                     c_testGP = c_test.seq[k],
                     p_GP.Rx = 1) %>%
    Ec_pop(pop_age_window) %>%
    sapply(sum, na.rm = TRUE)
}


maxCost1a <-
  which(INMB(QALYgain = scenario0["e"] - scenario1a[, "e"],
             cost_incurred = scenario1a[, "c"] - scenario0["c"]) > 0) %>%
  names() %>%
  as.numeric() %>%
  max()


# scenario 1b (test @ GP & obtain Rx increase) --------------------------------
# GP prescribe on rapid test only

scenario1b <- scenario_TEMPLATE

for (k in seq_along(c_test.seq)) {

  scenario1b[k,] <-
    trans_mat_1b %>%
    Ec_by_age_window(spec_GP = 1,
                     sens_GP = 1,
                     c_testGP = c_test.seq[k],
                     p_GP.Rx = 1) %>%
    Ec_pop(pop_age_window) %>%
    sapply(sum, na.rm = TRUE)
}

maxCost1b <-
  which(INMB(QALYgain = scenario0["e"] - scenario1b[, "e"],
             cost_incurred = scenario1b[, "c"] - scenario0["c"]) > 0) %>%
  names() %>%
  as.numeric() %>%
  max()

# scenario 2a (test @ NPFS only) ---------------------------------------------

scenario2a <- scenario_TEMPLATE

for (k in seq_along(c_test.seq)) {

  scenario2a[k,] <-
    trans_mat %>%
    Ec_by_age_window(spec_NPFS = 1,
                     sens_NPFS = 1,
                     c_testNPFS = c_test.seq[k]) %>%
    Ec_pop(pop_age_window) %>%
    sapply(sum, na.rm = TRUE)
}

maxCost2a <-
  which(INMB(QALYgain = scenario0["e"] - scenario2a[, "e"],
             cost_incurred = scenario2a[, "c"] - scenario0["c"]) > 0) %>%
  names() %>%
  as.numeric() %>%
  max()

# scenario 2b (test @ NPFS only AND obtain Rx increase) ----------------------


scenario2b <- scenario_TEMPLATE

for (k in seq_along(c_test.seq)) {

  scenario2b[k,] <-
    trans_mat_2b %>%
    Ec_by_age_window(spec_NPFS = 1,
                     sens_NPFS = 1,
                     c_testNPFS = c_test.seq[k]) %>%
    Ec_pop(pop_age_window) %>%
    sapply(sum, na.rm = TRUE)
}

maxCost2b <-
  which(INMB(QALYgain = scenario0["e"] - scenario2b[, "e"],
             cost_incurred = scenario2b[, "c"] - scenario0["c"]) > 0) %>%
  names() %>%
  as.numeric() %>%
  max()

# scenario 2c (test @ NPFS only AND switch from GP to NPFS) ---------------------
## assume 1/2 switch


scenario2c <- scenario_TEMPLATE

for (k in seq_along(c_test.seq)) {

  scenario2c[k,] <-
    trans_mat_2c %>%
    Ec_by_age_window(spec_NPFS = 1,
                     sens_NPFS = 1,
                     c_testNPFS = c_test.seq[k]) %>%
    Ec_pop(pop_age_window) %>%
    sapply(sum, na.rm = TRUE)
}

maxCost2c <-
  which(INMB(QALYgain = scenario0["e"] - scenario2c[, "e"],
             cost_incurred = scenario2c[, "c"] - scenario0["c"]) > 0) %>%
  names() %>%
  as.numeric() %>%
  max()

# scenario 2d (test @ NPFS only AND NPFS use increase) -----------------------------------------
## assume same prop who already seek care


scenario2d <- scenario_TEMPLATE

for (k in seq_along(c_test.seq)) {

  scenario2d[k,] <-
    trans_mat_2d %>%
    Ec_by_age_window(spec_NPFS = 1,
                     sens_NPFS = 1,
                     c_testNPFS = c_test.seq[k]) %>%
    Ec_pop(pop_age_window) %>%
    sapply(sum, na.rm = TRUE)
}

maxCost2d <-
  which(INMB(QALYgain = scenario0["e"] - scenario2d[, "e"],
             cost_incurred = scenario2d[, "c"] - scenario0["c"]) > 0) %>%
  names() %>%
  as.numeric() %>%
  max()

# scenario 2e (combine NPFS scenarios) -----------------------------------------


scenario2e <- scenario_TEMPLATE

for (k in seq_along(c_test.seq)) {

  scenario2e[k,] <-
    trans_mat_2e %>%
    Ec_by_age_window(spec_NPFS = 1,
                     sens_NPFS = 1,
                     c_testNPFS = c_test.seq[k]) %>%
    Ec_pop(pop_age_window) %>%
    sapply(sum, na.rm = TRUE)
}

maxCost2e <-
  which(INMB(QALYgain = scenario0["e"] - scenario2e[, "e"],
             cost_incurred = scenario2e[, "c"] - scenario0["c"]) > 0) %>%
  names() %>%
  as.numeric() %>%
  max()

# scenario 3 (combine NPFS and GP scenarios) ------------------------------------


scenario3 <- scenario_TEMPLATE

for (k in seq_along(c_test.seq)) {

  scenario3[k,] <-
    trans_mat_3 %>%
    Ec_by_age_window(spec_GP = 1,
                     sens_GP = 1,
                     c_testGP = c_test.seq[k],
                     p_GP.Rx = 1,
                     spec_NPFS = 1,
                     sens_NPFS = 1,
                     c_testNPFS = c_test.seq[k]) %>%
    Ec_pop(pop_age_window) %>%
    sapply(sum, na.rm = TRUE)
}

maxCost3 <-
  which(INMB(QALYgain = scenario0["e"] - scenario3[, "e"],
             cost_incurred = scenario3[, "c"] - scenario0["c"]) > 0) %>%
  names() %>%
  as.numeric() %>%
  max()


maxCost1a
maxCost1b
maxCost2a
maxCost2b
maxCost2c
maxCost2d
maxCost2e
maxCost3


