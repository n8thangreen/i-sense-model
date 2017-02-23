#
# project: i-sense
# N Green
# 21/2/2017
#
# cost-effectiveness analysis


# unit costs

c_obtain <- 1
c_NPFS <- 1
c_GP <- 1
c_hosp <- 1


# probabilities

p_prescribe <- 0.7
p_obtainNPFS <- 0.9
p_obtainGP <- 0.9
p_take <- 0.9
p_complete <- 0.9
p_GPpos <- 0.7
p_NPFSpos <- 0.8
p_NPFS1 <- 0.1
p_NPFS2 <- 0.01
p_GP1 <- 0.05
p_GP2 <- 0.02
p_hosp <- 0.001


# novel test

c_POCtest <- 1

## sensitivity
p_POCsens <- seq(0, 1, by = 0.1)

## specificity
p_POCspec <- seq(0, 1, by = 0.1)


# expected costs ----------------------------------------------------------

Ec_flu <- function(c_obtain = 1,
                   c_NPFS = 1,
                   c_GP = 1,
                   c_hosp = 1,
                   p_prescribe = 0.7,
                   p_obtainNCSP = 0.9,
                   p_obtainGP = 0.9,
                   p_take = 0.9,
                   p_complete = 0.9,
                   p_GPpos = 0.7,
                   p_NPFSpos = 0.8,
                   p_NPFS1 = 0.1,
                   p_NPFS2 = 0.01,
                   p_GP1 = 0.05,
                   p_GP2 = 0.02,
                   p_hosp = 0.001){

  Ec_hosp <- c_hosp*p_hosp

  # treatment
  # assume the same pathway whether prescribed through GP or NPFS
  Ec_obtain <- c_obtain + Ec_hosp*(1-p_complete)*p_take + (1-p_take)*Ec_hosp

  Ec_posTestNPFS <- Ec_obtain*p_obtainNPFS + (1-p_obtainNPFS)*Ec_hosp
  Ec_posTestGP <- Ec_obtain*p_obtainGP + (1-p_obtainGP)*Ec_hosp

  # visit NPFS first
  Ec_GP2 <- c_GP + p_GPpos*p_prescribe*Ec_posTestGP + Ec_hosp*(p_GPpos*(1-p_prescribe) + (1-p_GPpos))

  Ec_NPFS1neg <- Ec_GP2*p_GP2 + (1-p_GP2)*Ec_hosp

  Ec_NPFS1 <- c_NPFS + p_NPFSpos*Ec_posTestNPFS + (1-p_NPFSpos)*Ec_NPFS1neg

  # assume all GPpos not prescibed go to NPFS
  Ec_GP1pos <- (1-p_prescibe)*(c_NPFS + Ec_hosp*(1-p_NPFSpos) + p_NPFSpos*Ec_posTestNPFS) + p_prescribe*Ec_posTestGP

  Ec_NPFS2 <- c_NPFS + Ec_hosp*(1-p_NPFSpos) + p_NPFSpos*Ec_posTestNPFS

  Ec_GP1neg <- Ec_NPFS2*p_NPFS2 + (1-p_NPFS2)*Ec_hosp

  # visit GP first
  Ec_GP1 <- c_GP + p_GPpos*Ec_GP1pos + (1-p_GPpos)*Ec_GP1neg

  p_NPFS1*Ec_NPFS1 + p_GP1*Ec_GP1
}


# scenarios ---------------------------------------------------------------

# scenario 1 (PoC test @ GP):

Ec_flu(p_GPpos = p_POCsens,
       c_GP = c_GP + c_POCtest) ##TODO: is this additional to current cost?

# scenario 2a (NPFS pos 2-step):

Ec_flu(p_NPFSpos = p_NPFSpos*p_POCsens,
       c_NPFS = c_NPFS + c_POCtest)

# scenario 2b (obtain Rx increase):

Ec_flu(p_obtainNCSP = p_obtainNCSP*2,
       p_NPFSpos = p_POCsens,          ##TODO: does the POC test replace the NPFS algorithm?
       c_NPFS = c_NPFS + c_POCtest)

# scenario 2c (switch from GP to NPFS):
## assume 1/2 switch

Ec_flu(p_GP1 = p_GP1/2,
       p_NPFS1 = p_NPFS1 + p_GP1/2,
       p_NPFSpos = p_POCsens,          ##TODO: does the POC test replace the NPFS algorithm?
       c_NPFS = c_NPFS + c_POCtest)

# scenario 2d (NPFS use increase):
## assume same prop who already seek care

Ec_flu(p_NPFS1 = 2*p_NPFS1 + p_GP1,
       p_NPFSpos = p_POCsens,          ##TODO: does the POC test replace the NPFS algorithm?
       c_NPFS = c_NPFS + c_POCtest)


