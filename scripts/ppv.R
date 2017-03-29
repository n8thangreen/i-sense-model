#
# project: i-sense
# N Green
# Feb 2017
#
# H1N1 incidence sensitivity analysis


i <- 0
out <- NULL
fracH1N1 <- seq(0, 1, by = 0.05)

# fraction H1N1
for (frac in fracH1N1) {

  i = i + 1

  trans_mat2 <-
    trans_mat %>%
    dcast(from + age + NPFS_weeks_window ~ to,
          value.var = "prob") %>%
    mutate(GP = GP_H1N1 + GP_notH1N1,
           NPFS = NPFS_H1N1 + NPFS_notH1N1,
           notseekcare = notseekcare_H1N1 + notseekcare_notH1N1,
           GP_H1N1 = GP*frac,
           GP_notH1N1 = GP*(1 - frac),
           NPFS_H1N1 = NPFS*frac,
           NPFS_notH1N1 = NPFS*(1 - frac),
           notseekcare_H1N1 = notseekcare*frac,
           notseekcare_notH1N1 = notseekcare*(1 - frac)) %>%
    melt(id.vars = c("from", "age", "NPFS_weeks_window"),
         variable.name = "to",
         value.name = "prob") %>%
    select(from, to, everything()) %>%
    filter(complete.cases(.))


  # treat everyone
  scenario_all <-
    trans_mat2 %>%
    Ec_by_age_window(spec_NPFS = 0,
                     sens_NPFS = 1,
                     spec_GP = 0,
                     sens_GP = 1,
                     c_testNPFS = 0,
                     c_testGP = 0) %>%
    Ec_pop(pop_age_window) %>%
    sapply(sum, na.rm = TRUE)

  # treat no-one
  scenario_nothing <-
    trans_mat2 %>%
    Ec_by_age_window(spec_NPFS = 1,
                     sens_NPFS = 0,
                     spec_GP = 1,
                     sens_GP = 0,
                     c_testNPFS = 0,
                     c_testGP = 0) %>%
    Ec_pop(pop_age_window) %>%
    sapply(sum, na.rm = TRUE)


  out[i] <- INMB(QALYgain = scenario_nothing['e'] - scenario_all['e'],
                 cost_incurred = scenario_all['c'] - scenario_nothing['c'],
                 wtp = 30000)

}

names(out) <- fracH1N1
plot(fracH1N1, out, type = "l", ylab = "INMB (GBP)", xlab = "Proportion seeking care (A)H1N1", main = "WTP = 30000")
abline(a = 0, b = 0, lty = 2)
