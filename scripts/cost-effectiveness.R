k#
# project: i-sense
# N Green
# 21/2/2017
#
# cost-effectiveness model


p_H1N1 = 0.5
p_GPdiag = 0.5
p_NPFSauth = 0.5
p_obtain = 0.5
p_hosp_dropout = 0.5
p_start = 0.5
p_complete = 0.5
p_hosp_complete = 0.5
p_hosp_dropout.notH1N1 = 0.5
p_hosp_complete.notH1N1 = 0.5
spec_NPFS = 1
sens_NPFS = 1
spec_GP = 1
sens_GP = 1
c_NPFS = 1
c_GP = 1
C_obtain = 1
c_hosp = 1
c_PCT.NPFS = 0
c_PCT.GP = 0


# novel test

## unit cost
c_PCT.GP <- seq(0, 100, by = 1)
c_PCT.NPFS <- seq(0, 100, by = 1)

## sensitivity
sens_GP <- seq(0, 1, by = 0.1)
sens_NPFS <- seq(0, 1, by = 0.1)

## specificity
spec_GP <- seq(0, 1, by = 0.1)
spec_NPFS <- seq(0, 1, by = 0.1)


# expected costs ----------------------------------------------------------

Ec_ILI <-  function(p_H1N1 = 0.5,
                    p_GPdiag = 0.5,
                    p_NPFSauth = 0.5,
                    p_obtain = 0.5,
                    p_hosp_dropout = 0.5,
                    p_start = 0.5,
                    p_complete = 0.5,
                    p_hosp_complete = 0.5,
                    p_hosp_dropout.notH1N1 = 0.5,
                    p_hosp_complete.notH1N1 = 0.5,
                    spec_NPFS = 1,
                    sens_NPFS = 1,
                    spec_GP = 1,
                    sens_GP = 1,
                    c_NPFS = 1,
                    c_GP = 1,
                    c_obtain = ,
                    c_hosp = ,
                    c_PCT.NPFS = 0,
                    c_PCT.GP = 0){

  Ec_start.notH1N1 <- p_complete*p_hosp_complete.notH1N1*c_hosp + (1 - p_complete)*p_hosp_dropout.notH1N1*c_hosp
  Ec_start <- p_complete*p_hosp_complete*c_hosp + (1 - p_complete)*p_hosp_dropout*c_hosp

  Ec_obtain <- p_start*Ec_start + (1 - p_start)*p_hosp_dropout*c_hosp
  Ec_obtain.notH1N1 <- p_start*Ec_start.notH1N1 + (1 - p_start)*p_hosp_dropout.notH1N1*c_hosp

  Ec_Tx.notH1N1 <- p_obtain*(c_obtain + Ec_obtain.notH1N1) + (1 - p_obtain)*p_hosp_dropout.notH1N1*c_hosp
  Ec_Tx <- p_obtain*(c_obtain + Ec_obtain) + (1 - p_obtain)*p_hosp_dropout*c_hosp

  Ec_NPFS.notH1N1 <- c_NPFS + c_PCT.NPFS + (1 - spec_NPFS)*Ec_Tx.notH1N1
  Ec_NPFS <- c_NPFS + c_PCT.NPFS + sens_NPFS*Ec_Tx

  Ec_GP.notH1N1 <- c_GP + c_PCT.GP + (1 - spec_GP)*Ec_Tx
  Ec_GP <- c_GP + c_PCT.GP + sens_GP*Ec_Tx

  Ec_notH1N1 <- p_GPdiag*Ec_GP.notH1N1 + p_NPFSauth*Ec_NPFS.notH1N1
  Ec_H1N1 <- p_GPdiag*Ec_GP + p_NPFSauth*Ec_NPFS

  return(p_H1N1 * Ec_H1N1 + (1 - p_H1N1) * Ec_notH1N1)
}



# scenarios ---------------------------------------------------------------

# scenario 1 (PCT test @ GP):

Ec_ILI(p_GPpos = p_PCTsens,
       c_GP = c_GP + c_PCTtest)

# scenario 2a (NPFS pos 2-step):

Ec_ILI(p_NPFSpos = p_NPFSpos*p_PCTsens,
       c_NPFS = c_NPFS + c_PCTtest)

# scenario 2b (obtain Rx increase):

Ec_ILI(p_obtainNCSP = p_obtainNCSP*2,
       p_NPFSpos = p_PCTsens,
       c_NPFS = c_NPFS + c_PCTtest)

# scenario 2c (switch from GP to NPFS):
## assume 1/2 switch

Ec_ILI(p_GP1 = p_GP1/2,
       p_NPFS1 = p_NPFS1 + p_GP1/2,
       p_NPFSpos = p_PCTsens,
       c_NPFS = c_NPFS + c_PCTtest)

# scenario 2d (NPFS use increase):
## assume same prop who already seek care

Ec_ILI(p_NPFS1 = 2*p_NPFS1 + p_GP1,
       p_NPFSpos = p_PCTsens,
       c_NPFS = c_NPFS + c_PCTtest)


