#
# project: i-sense
# N Green
# March 2017
#
# cost-effectiveness scenarios


library(magrittr)


spec.seq  <- seq(0, 1, 0.1)#0.05)
sens.seq  <- seq(0, 1, 0.1)#0.05)
c_test.seq <- seq(0, 10, 1)#0.5)


# scenario 0 (status-quo) -------------------------------------------------

scenario0 <-
  trans_mat %>%
  Ec_by_age_window() %>%
  Ec_pop(pop_age_window) %>%
  sapply(sum, na.rm = TRUE)


# scenario 1 (test @ GP only) ----------------------------------------------

# GP prescribe on rapid test only
p_GP.Rx = 1

scenario1 <- array(data = NA,
                   dim = c(length(spec.seq), length(sens.seq), length(c_test.seq), 2),
                   dimnames = list(spec.seq, sens.seq, c_test.seq, c("c","e")))

for (i in seq_along(spec.seq)) {
  for (j in seq_along(sens.seq)) {
    for (k in seq_along(c_test.seq)) {

      scenario1[i,j,k, ] <-
        trans_mat %>%
        Ec_by_age_window(spec_GP = spec.seq[i],
                         sens_GP = sens.seq[j],
                         c_testGP = c_test.seq[k]) %>%
        Ec_pop(pop_age_window) %>%
        sapply(sum, na.rm = TRUE)
    }
  }
}

save(scenario1, file = "../../data cleaned/scenario1_costeffective.RData")


# scenario 2a (test @ NPFS only) ---------------------------------------------

scenario2a <- array(data = NA,
                    dim = c(length(spec.seq), length(sens.seq), length(c_test.seq), 2),
                    dimnames = list(spec.seq, sens.seq, c_test.seq, c("c","e")))

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

trans_mat2 <-
  trans_mat %>%
  mutate(prob = ifelse(from == "NPFS" & to == "coll",
                       prob + (1 - prob)/2,
                       prob))

scenario2b <- array(data = NA,
                    dim = c(length(spec.seq), length(sens.seq), length(c_test.seq), 2),
                    dimnames = list(spec.seq, sens.seq, c_test.seq, c("c","e")))

for (i in seq_along(spec.seq)) {
  for (j in seq_along(sens.seq)) {
    for (k in seq_along(c_test.seq)) {

      scenario2b[i,j,k, ] <-
        trans_mat2 %>%
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

# use sparse-square arrangement

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


scenario2c <- array(data = NA,
                    dim = c(length(spec.seq), length(sens.seq), length(c_test.seq), 2),
                    dimnames = list(spec.seq, sens.seq, c_test.seq, c("c","e")))

for (i in seq_along(spec.seq)) {
  for (j in seq_along(sens.seq)) {
    for (k in seq_along(c_test.seq)) {

      scenario2c[i,j,k, ] <-
        trans_mat2 %>%
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


trans_mat2 <-
  trans_mat %>%
  dcast(from + age + NPFS_weeks_window ~ to,
        value.var = "prob") %>%
  mutate(seekcare_H1N1 = NPFS_H1N1 + GP_H1N1,
         seekcare_notH1N1 = NPFS_notH1N1 + GP_notH1N1,
         NPFS_H1N1 = NPFS_H1N1 + seekcare_H1N1,
         NPFS_notH1N1 = NPFS_notH1N1 + seekcare_notH1N1,
         notseekcare_H1N1 = notseekcare_H1N1 - seekcare_H1N1,
         notseekcare_notH1N1 = notseekcare_notH1N1 - seekcare_notH1N1) %>%
  select(-seekcare_H1N1, -seekcare_notH1N1) %>%
  melt(id.vars = c("from", "age", "NPFS_weeks_window"),
       variable.name = "to",
       value.name = "prob") %>%
  select(from, to, everything()) %>%
  filter(complete.cases(.))


scenario2d <- array(data = NA,
                    dim = c(length(spec.seq), length(sens.seq), length(c_test.seq), 2),
                    dimnames = list(spec.seq, sens.seq, c_test.seq, c("c","e")))

for (i in seq_along(spec.seq)) {
  for (j in seq_along(sens.seq)) {
    for (k in seq_along(c_test.seq)) {

      scenario2d[i,j,k, ] <-
        trans_mat2 %>%
        Ec_by_age_window(spec_NPFS = spec.seq[i],
                         sens_NPFS = sens.seq[j],
                         c_testNPFS = c_test.seq[k]) %>%
        Ec_pop(pop_age_window) %>%
        sapply(sum, na.rm = TRUE)
    }
  }
}

save(scenario2d, file = "../../data cleaned/scenario2d_costeffective.RData")

