#
# project: i-sense
# N Green
# Feb 2017
#
# H1N1 incidence sensitivity analysis


# fraction those who seek are H1N1
frac <- 1

trans_mat2 <-
  trans_mat %>%
  dcast(from + age + NPFS_weeks_window ~ to,
        value.var = "prob") %>%
  mutate(GP = GP_H1N1 + GP_notH1N1,
         NPFS = NPFS_H1N1 + NPFS_notH1N1,
         GP_H1N1 = GP*frac,
         GP_notH1N1 = GP*(1 - frac),
         NPFS_H1N1 = NPFS*frac,
         NPFS_notH1N1 = NPFS*(1 - frac)) %>%
  melt(id.vars = c("from", "age", "NPFS_weeks_window"),
       variable.name = "to",
       value.name = "prob") %>%
  select(from, to, everything()) %>%
  filter(complete.cases(.))


# treat everyone
scenario_all <-
  trans_mat2 %>%
  Ec_by_age_window(spec_NPFS = 1,
                   sens_NPFS = 0,
                   spec_GP = 1,
                   sens_GP = 0,
                   c_testNPFS = 0) %>%
  Ec_pop(pop_age_window) %>%
  sapply(sum, na.rm = TRUE)

# treat no-one
scenario_nothing <-
  trans_mat2 %>%
  Ec_by_age_window(spec_NPFS = 0,
                   sens_NPFS = 1,
                   spec_GP = 0,
                   sens_GP = 1,
                   c_testNPFS = 0) %>%
  Ec_pop(pop_age_window) %>%
  sapply(sum, na.rm = TRUE)


INMB(QALYgain = scenario_nothing['e'] - scenario_all['e'],
     cost_incurred = scenario_all['c'] - scenario_nothing['c'])

# tests
#
#
#


