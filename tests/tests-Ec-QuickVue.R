

input <-
  trans_mat %>%
  filter(age == "2544",
         NPFS_weeks_window == 2)

with(input,
     Ec_ILI(
       # service use
       p_GP.H1N1 = prob[from == "Sx" & to == "GP_H1N1"],
       p_GP.notH1N1 = prob[from == "Sx" & to == "GP_notH1N1"],
       p_NPFS.H1N1 = prob[from == "Sx" & to == "NPFS_H1N1"],
       p_NPFS.notH1N1 = prob[from == "Sx" & to == "NPFS_notH1N1"],
       p_notseekcare_H1N1 = prob[from == "Sx" & to == "notseekcare_H1N1"],
       p_Sx = prob[from == "flu" & to == "Sx"],
       p_flu = prob[from == "pop" & to == "flu"],

       # treatment
       p_GP.collect = prob[from == "GP" & to == "coll"],
       p_NPFS.collect = prob[from == "NPFS" & to == "coll"],
       p_start = prob[from == "coll" & to == "start"],
       p_complete = prob[from == "start" & to == "complete"],
       p_hosp = prob[from == "ILI" & to == "hosp"],
       p_hosp_complete = prob[from == "complete" & to == "hosp"],
       p_death = prob[from == "hosp" & to == "death"],

       # test performance
       spec_NPFS = 0.84,
       sens_NPFS = 0.66,
       spec_GP = 0.84,
       sens_GP = 0.66,

       # costs incurred
       c_testNPFS = 5,
       c_testGP = 5,

       # QALY loss
       Q_excess_life = 20))
