
#' Expected Cost Incurred and QALY Loss
#'
#' Cost-effectiveness model
#' The default test performance is not to filter out anyone.
#'
#' @param p_GP.H1N1 Probability
#' @param p_GP.notH1N1 Probability
#' @param p_NPFS.H1N1 Probability
#' @param p_NPFS.notH1N1 Probability
#' @param p_GP.collect Probability
#' @param p_NPFS.collect Probability
#' @param p_start Probability
#' @param p_complete Probability
#' @param p_hosp Probability
#' @param p_hosp_complete Probability
#' @param p_death Probability
#' @param spec_NPFS Test performance
#' @param sens_NPFS Test performance
#' @param spec_GP Test performance
#' @param sens_GP Test performance
#' @param c_NPFS Cost
#' @param c_GP Cost
#' @param c_collect Cost
#' @param c_avNPFS Cost
#' @param c_hosp Cost
#' @param c_death Cost
#' @param c_testNPFS Cost
#' @param c_testGP Cost
#' @param Q_excess_life QALY loss
#' @param Q_hosp QALY loss
#' @param Q_nonhosp QALY loss
#'
#' @return list(c, e)
#' @export
#'
#' @examples
#'
#' p_GP.H1N1 = 0.1
#' p_GP.notH1N1 = 0.1
#' p_NPFS.H1N1 = 0.1
#' p_NPFS.notH1N1 = 0.1
#'
#' # treatment
#' p_GP.collect = 0.5
#' p_NPFS.collect = 0.5
#' p_start = 0.5
#' p_complete = 0.5
#' p_hosp = 0.5
#' p_hosp_complete = 0.25
#' p_death = 0.5
#'
#' # test performance
#' spec_NPFS = 0
#' sens_NPFS = 1
#' spec_GP = 0
#' sens_GP = 1
#'
#' # costs
#' c_NPFS = 17 #phone call #Baguelin Vaccine (2010)
#' c_GP = 37 #consultation #Baguelin Vaccine (2010)
#' c_collect = 10 #antivirals #Baguelin Vaccine (2010)
#' c_avNPFS = 16 #antivirals + delivery #Baguelin Vaccine (2010)
#' c_hosp = 840 #Baguelin Vaccine (2010)
#' c_death = 1500 #intensive care #Baguelin Vaccine (2010)
#' c_testNPFS = 0
#' c_testGP = 0
#'
#' # QALY loss
#' Q_excess_life = 1
#' Q_hosp = 0.018 #Baguelin Vaccine (2010)
#' Q_nonhosp = 0.0082
#'
#' Ec_ILI()
#'
#' # perfect test at NPFS
#' Ec_ILI() - Ec_ILI(spec_NPFS = 1, c_testNPFS = 1)
#'
#' # cost to test
#' c_incur <- (p_NPFS.H1N1 + p_NPFS.notH1N1)*c_testNPFS
#' # cost saved
#' c_saved <- p_NPFS.notH1N1*p_NPFS.collect*c_collect
#' c_saved - c_incur
#'
Ec_ILI <-  function(p_GP.H1N1 = 0.1,
                    p_GP.notH1N1 = 0.1,
                    p_NPFS.H1N1 = 0.1,
                    p_NPFS.notH1N1 = 0.1,

                    # treatment
                    p_GP.collect = 0.5,
                    p_NPFS.collect = 0.5,
                    p_start = 0.5,
                    p_complete = 0.5,
                    p_hosp = 0.5,
                    p_hosp_complete = 0.25,
                    p_death = 0.5,

                    # test performance
                    spec_NPFS = 0,
                    sens_NPFS = 1,
                    spec_GP = 0,
                    sens_GP = 1,

                    # costs
                    c_NPFS = 17, #phone call #Baguelin, Vaccine (2010)
                    c_GP = 37, #consultation #Baguelin, Vaccine (2010)
                    c_collect = 10, #antivirals #Baguelin, Vaccine (2010)
                    c_avNPFS = 16, #antivirals + delivery #Baguelin, Vaccine (2010)
                    c_hosp = 840, #Baguelin, Vaccine (2010)
                    c_death = 1500, #intensive care #Baguelin, Vaccine (2010)
                    c_testNPFS = 0,
                    c_testGP = 0,

                    # QALY loss
                    Q_excess_life = 0,
                    Q_hosp = 0.018, #Baguelin, Vaccine (2010)
                    Q_nonhosp = 0.0082) #Baguelin, Vaccine (2010)
{
  ## cost incurred ##

  # H1N1
  Ec_hospH1N1 <- c_death*p_death + (1 - p_death)*0 + c_hosp
  Ec_negH1N1 <- p_hosp*Ec_hospH1N1
  Ec_completeH1N1 <- Ec_hospH1N1*p_hosp_complete + (1 - p_hosp_complete)*0
  Ec_startH1N1 <- Ec_completeH1N1*p_complete + Ec_negH1N1*(1 - p_complete)
  Ec_collH1N1 <- Ec_startH1N1*p_start + Ec_negH1N1*(1 - p_start) + c_collect
  Ec_GP.posH1N1 <- Ec_collH1N1*p_GP.collect + (1 - p_GP.collect)*Ec_negH1N1
  Ec_NPFS.posH1N1 <- Ec_collH1N1*p_NPFS.collect + (1 - p_NPFS.collect)*Ec_negH1N1

  Ec_GP.H1N1 <- sens_GP*Ec_GP.posH1N1 + (1 - sens_GP)*Ec_negH1N1 + c_testGP + c_GP
  Ec_NPFS.H1N1 <- sens_NPFS*Ec_NPFS.posH1N1 + (1 - sens_NPFS)*Ec_negH1N1 + c_testNPFS + c_NPFS

  # non-H1N1
  Ec_GP.notH1N1 <- (1 - spec_GP)*c_collect*p_GP.collect + c_testGP + c_GP
  Ec_NPFS.notH1N1 <- (1 - spec_NPFS)*c_collect*p_NPFS.collect + c_testNPFS + c_NPFS

  cost <-
    p_GP.H1N1*Ec_GP.H1N1 +
    p_GP.notH1N1*Ec_GP.notH1N1 +
    p_NPFS.H1N1*Ec_NPFS.H1N1 +
    p_NPFS.notH1N1*Ec_NPFS.notH1N1


  ## QALY loss ##

  # H1N1
  Eq_hospH1N1 <- Q_excess_life*p_death + Q_hosp
  Eq_negH1N1 <- Eq_hospH1N1*p_hosp + (1 - p_hosp)*Q_nonhosp
  Eq_completeH1N1 <- Eq_hospH1N1*p_hosp_complete + (1 - p_hosp_complete)*Q_nonhosp
  Eq_startH1N1 <- Eq_completeH1N1*p_complete + Eq_negH1N1*(1 - p_complete)
  Eq_collH1N1 <- Eq_startH1N1*p_start + Eq_negH1N1*(1 - p_start)
  Eq_GP.posH1N1 <- Eq_collH1N1*p_GP.collect + (1 - p_GP.collect)*Eq_negH1N1
  Eq_NPFS.posH1N1 <- Eq_collH1N1*p_NPFS.collect + (1 - p_NPFS.collect)*Eq_negH1N1

  # non-H1N1
  Eq_GP.H1N1 <- sens_GP*Eq_GP.posH1N1 + (1 - sens_GP)*Eq_negH1N1
  Eq_GP.notH1N1 <- 0

  Eq_NPFS.H1N1 <- sens_NPFS*Eq_NPFS.posH1N1 + (1 - sens_NPFS)*Eq_negH1N1
  Eq_NPFS.notH1N1 <- 0

  eff <-
    p_GP.H1N1*Eq_GP.H1N1 +
    p_GP.notH1N1*Eq_GP.notH1N1 +
    p_NPFS.H1N1*Eq_NPFS.H1N1 +
    p_NPFS.notH1N1*Eq_NPFS.notH1N1


  return(c(c = cost,
           e = eff))
}






