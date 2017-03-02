k#
# project: i-sense
# N Green
# 21/2/2017
#
# cost-effectiveness model


# novel test

## unit cost
c_testGP <- seq(0, 100, by = 1)
c_testNPFS <- seq(0, 100, by = 1)

## sensitivity
sens_GP <- seq(0, 1, by = 0.1)
sens_NPFS <- seq(0, 1, by = 0.1)

## specificity
spec_GP <- seq(0, 1, by = 0.1)
spec_NPFS <- seq(0, 1, by = 0.1)


# expected costs ----------------------------------------------------------

Ec_ILI <-  function(p_GP.H1N1 = 0.1,
                    p_GP.notH1N1 = 0.1,
                    p_NPFS.H1N1 = 0.1,
                    p_NPFS.notH1N1 = 0.1,

                    # treatment
                    p_collect = 0.5,
                    p_start = 0.5,
                    p_complete = 0.5,
                    p_hosp = 0.5,
                    p_death = 0.5,

                    # test performance
                    spec_NPFS = 1,
                    sens_NPFS = 1,
                    spec_GP = 1,
                    sens_GP = 1,

                    # costs
                    c_NPFS = 1,
                    c_GP = 1,
                    c_collect = 1,
                    c_hosp = 1,
                    c_death = 1,
                    c_testNPFS = 0,
                    c_testGP = 0){

  # H1N1
  Ec_hospH1N1 <- c_death*p_death + (1 - p_death)*0
  Ec_negH1N1 <- p_hosp*(c_hosp + Ec_hospH1N1)
  Ec_completeH1N1 <- (Ec_hospH1N1 + c_hosp)*p_hosp + (1 - p_hosp)*0
  Ec_startH1N1 <- Ec_completeH1N1*p_complete + Ec_negH1N1*(1 - p_complete)
  Ec_collH1N1 <- Ec_startH1N1*p_start + Ec_negH1N1*(1 - p_start)
  Ec_posH1N1 <- (Ec_collH1N1 + c_collect)*p_collect + (1 - p_collect)*Ec_negH1N1

  # non-H1N1
  Ec_startnotH1N1 <- p_complete*0 + (1 - p_complete)*0
  Ec_collnotH1N1 <- p_start*Ec_startnotH1N1 + (1 - p_start)*0
  Ec_posnotH1N1 <- (Ec_collnotH1N1 + c_collect)*p_collect + (1 - p_collect)*0
  Ec_negnotH1N1 <- 0


  Ec_GP.H1N1 <- sens_GP*Ec_posH1N1 + (1 - sens_GP)*Ec_negH1N1
  Ec_GP.notH1N1 <- (1 - spec_GP)*Ec_posnotH1N1 + spec_GP*Ec_negnotH1N1

  Ec_NPFS.H1N1 <- sens_GP*Ec_posH1N1 + (1 - sens_GP)*Ec_negH1N1
  Ec_NPFS.notH1N1 <- (1 - spec_GP)*Ec_posnotH1N1 + spec_GP*Ec_negnotH1N1


  Ec_ILI <-
    p_GP.H1N1*(Ec_GP.H1N1 + c_GP + c_testGP) +
    p_GP.notH1N1*(Ec_GP.notH1N1 + c_GP + c_testGP) +
    p_NPFS.H1N1*(Ec_NPFS.H1N1 + c_NPFS + c_testNPFS) +
    p_NPFS.notH1N1*(Ec_NPFS.notH1N1 + c_NPFS + c_testNPFS)

  return(Ec_ILI)
}


Ec_ILI()


