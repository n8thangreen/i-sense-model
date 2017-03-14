#
# project: i-sense
# N Green
# March 2017
#
# scenarios


library(magrittr)


spec.seq  <- seq(0, 1, 0.1)
sens.seq  <- seq(0, 1, 0.1)
c_test.seq <- seq(0, 5, 1)


# scenario 0 (status-quo) -------------------------------------------------

scenario0 <-
  trans_mat %>%
  Ec_by_age_window() %>%
  Ec_pop(total_service) %>%
  sapply(sum, na.rm = TRUE)


# scenario 1 (PCT test @ GP) ----------------------------------------------

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
        Ec_pop(total_service) %>%
        sapply(sum, na.rm = TRUE)
    }
  }
}


# scenario 2a (NPFS pos 2-step) -------------------------------------------

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
        Ec_pop(total_service) %>%
        sapply(sum, na.rm = TRUE)
    }
  }
}



# scenario 2b (obtain Rx increase) ----------------------------------------

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
        Ec_pop(total_service) %>%
        sapply(sum, na.rm = TRUE)
    }
  }
}



# scenario 2c (switch from GP to NPFS) ------------------------------------
## assume 1/2 switch

# sparse-square arrangement
trans_mat2 <-
  dcast(data = trans_mat,
        from + age + NPFS_weeks_window ~ to,
        value.var = "prob") %>%
  mutate(GP.H1N1 = GP.H1N1/2,
         GP.notH1N1 = GP.notH1N1/2,
         NPFS.H1N1 = NPFS.H1N1 + GP.H1N1,
         NPFS.notH1N1 = NPFS.notH1N1 + GP.notH1N1) %>%
  melt(id.vars = c("from", "age", "NPFS_weeks_window"),
       variable.name = "to",
       value.name = "prob") %>%
  select(from, to, everything())


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
        Ec_pop(total_service) %>%
        sapply(sum, na.rm = TRUE)
    }
  }
}



# scenario 2d (NPFS use increase) -----------------------------------------
## assume same prop who already seek care
## need to adjust the number of ILI as input to x2
##TODO## not equivalent...

total_service2 <-
  total_service %>%
  mutate(total_service = total_service*2)

# adjust the _relative_ probabilities
trans_mat2 <-
  trans_mat %>%
  mutate(prob = ifelse(to == "GP.H1N1",
                       prob/2,
                       prob),
         prob = ifelse(to == "GP.notH1N1",
                       prob/2,
                       prob))

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
        Ec_pop(total_service2) %>%
        sapply(sum, na.rm = TRUE)
    }
  }
}


