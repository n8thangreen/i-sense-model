


# tests ---------------

# simple test data
trans_mat2 <-
  trans_mat2 %>%
  filter(age == 514,
         NPFS_weeks_window == 2)

trans_mat_testdata <- edit(trans_mat2)

# treat everyone
scenario_all <-
  trans_mat_testdata %>%
  Ec_by_age_window(spec_NPFS = 0,
                   sens_NPFS = 1,
                   spec_GP = 0,
                   sens_GP = 1,
                   c_testNPFS = 0,
                   c_testGP = 0)
scenario_all
#
#
# ppv tests
#


# treat no-one
scenario_nothing <-
  trans_mat_testdata %>%
  Ec_by_age_window(spec_NPFS = 1,
                   sens_NPFS = 0,
                   spec_GP = 1,
                   sens_GP = 0,
                   c_testNPFS = 0,
                   c_testGP = 0)
scenario_nothing

# no H1N1 then
# dont avoid any hospital cost
# needlessly treat people
# no QALY loss
#
# scenario_all$c > scenario_nothing$c
# treament cost*p.seekcare

# mm <-
#   trans_mat_testdata %>%
#   filter(to %in% c("flu", "Sx", "NPFS_H1N1", "NPFS_notH1N1", "GP_H1N1", "GP_notH1N1")) %>%
#   group_by(from) %>%
#   dplyr::summarise(prob = sum(prob))
#
# # expected cost due to treatment
# 15.41*prod(mm$prob)



# all H1N1 then
# treating avoid some hospital cost
# treating avoids some QALY loss

# cost:

# all hosp, all death
c_GP + c_collect + c_death + c_hosp
37 + 15.41 + 1500 + 840
37 + 1500 + 840





