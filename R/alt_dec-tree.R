
#' Expected Cost Incurred and QALYs Loss
#'
#' cost-effectiveness model
#'
#' @param p_GP.H1N1
#' @param p_GP.notH1N1
#' @param p_NPFS.H1N1
#' @param p_NPFS.notH1N1
#' @param p_GP.collect
#' @param p_NPFS.collect
#' @param p_start
#' @param p_complete
#' @param p_hosp
#' @param p_hosp_complete
#' @param p_death
#' @param spec_NPFS
#' @param sens_NPFS
#' @param spec_GP
#' @param sens_GP
#' @param c_NPFS
#' @param c_GP
#' @param c_collect
#' @param c_avNPFS
#' @param c_hosp
#' @param jc_death
#' @param c_testNPFS
#' @param c_testGP
#' @param Q_excess_life
#' @param Q_hosp
#' @param Q_nonhosp
#'
#' @return list(e, c)
#' @export
#'
#' @examples
#'
#' Ec_ILI2()
#'
#' # perfect test at NPFS
#' Ec_ILI2() - Ec_ILI2(spec_NPFS = 1, c_testNPFS = 1)
#'
#' # cost to test
#' c_incur <- (p_NPFS.H1N1 + p_NPFS.notH1N1)*c_testNPFS
#' # cost saved
#' c_saved <- p_NPFS.notH1N1*p_NPFS.collect*c_collect
#' c_saved - c_incur
#'
Ec_ILI2 <-  function(p_GP.H1N1 = 0.1,
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
  Ec_hosp <- c_death*p_death + c_hosp

  cost <-
    p_NPFS.notH1N1*(c_NPFS + c_testNPFS +
                      (1 - spec_NPFS)*p_NPFS.collect*c_collect) +

    p_NPFS.H1N1*(c_NPFS + c_testNPFS +
                   sens_NPFS*(p_NPFS.collect*(c_collect + p_start*p_complete*p_hosp_complete*Ec_hosp +
                                                                    (1 - p_start*p_complete)*p_hosp*Ec_hosp) +
                                                    (1 - p_NPFS.collect)*p_hosp*Ec_hosp) +
                   (1 - sens_NPFS)*p_hosp*Ec_hosp) +

    p_GP.notH1N1*(c_GP + c_testGP +
                    (1 - spec_GP)*p_GP.collect*c_collect) +

    p_GP.H1N1*(c_GP + c_testGP +
                 sens_GP*(p_GP.collect*(c_collect + p_start*p_complete*p_hosp_complete*Ec_hosp +
                                                          (1 - p_start*p_complete)*p_hosp*Ec_hosp) +
                                            (1 - p_GP.collect)*p_hosp*Ec_hosp) +
                 (1 - sens_GP)*p_hosp*Ec_hosp)


  Eq_hosp <- p_death*Q_excess_life + Q_hosp

  eff <-
    p_NPFS.H1N1*(sens_NPFS*(
      p_NPFS.collect*p_start*p_complete*(p_hosp_complete*Eq_hosp + (1 - p_hosp_complete)*Q_nonhosp) +
        (1 - p_NPFS.collect*p_start*p_complete)*(p_hosp*Eq_hosp + (1 - p_hosp)*Q_nonhosp)) +
        (1 - sens_NPFS)*(p_hosp*Eq_hosp + (1 - p_hosp)*Q_nonhosp)) +
    p_GP.H1N1*(sens_GP*(
      p_GP.collect*p_start*p_complete*(p_hosp_complete*Eq_hosp + (1 - p_hosp_complete)*Q_nonhosp) +
        (1 - p_GP.collect*p_start*p_complete)*(p_hosp*Eq_hosp + (1 - p_hosp)*Q_nonhosp)) +
        (1 - sens_GP)*(p_hosp*Eq_hosp + (1 - p_hosp)*Q_nonhosp))


  return(c(c = cost,
           e = eff))
}






