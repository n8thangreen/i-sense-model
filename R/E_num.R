
#' Event Joint Probabilities
#'
#' These are the joint probabilities, not the step by step conditional probabilities
#'
#' @param p_GP.H1N1 Probability
#' @param p_GP.notH1N1 Probability
#' @param p_NPFS.H1N1 Probability
#' @param p_NPFS.notH1N1 Probability
#' @param p_notseekcare_H1N1 Probability
#' @param p_Sx Probability
#' @param p_flu Probability
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
#'
E_num <-  function(p_GP.H1N1 = 0.1,
                    p_GP.notH1N1 = 0.1,
                    p_NPFS.H1N1 = 0.1,
                    p_NPFS.notH1N1 = 0.1,
                    p_notseekcare_H1N1 = 0.1,
                    p_Sx = 0.5,
                    p_flu = 0.1,

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
                    sens_GP = 1)
{

  # ILI cases going to NPFS
  ILI_NPFS <- p_flu*p_Sx*(p_NPFS.notH1N1 + p_NPFS.H1N1)

  # ILI cases going to GP
  ILI_GP <- p_flu*p_Sx*(p_GP.notH1N1 + p_GP.H1N1)

  # NPFS authorisations after test
  new_NPFS <- (p_NPFS.notH1N1*(1 - spec_NPFS) + p_NPFS.H1N1*sens_NPFS)*p_flu*p_Sx

  # GP prescriptions after test
  new_GP <- (p_GP.notH1N1*(1 - spec_GP) + p_GP.H1N1*sens_GP)*p_flu*p_Sx

  # number treatment collections GP
  collection_GP <- new_GP*p_GP.collect

  # number treatment collections NPFS
  collection_NPFS <- new_NPFS*p_NPFS.collect

  # number treatment completed
  complete_Tx <- (collection_GP + collection_NPFS)*p_start*p_complete

  # number treatment completed H1N1
  complete_Tx_H1N1 <- (p_NPFS.H1N1*sens_NPFS + p_GP.H1N1*sens_GP)*p_flu*p_Sx*p_start*p_complete

  p_H1N1 <- p_NPFS.H1N1 + p_GP.H1N1 + p_notseekcare_H1N1

  # number hospitalisation
  num_hosp <- complete_Tx_H1N1*p_hosp_complete + (p_H1N1*p_flu*p_Sx - complete_Tx_H1N1)*p_hosp

  # death in hospital
  num_death <- num_hosp*p_death

  return(c(p_flu = p_flu,
           ILI_NPFS = ILI_NPFS,
           ILI_GP = ILI_GP,
           new_NPFS = new_NPFS,
           new_GP = new_GP,
           collection_GP = collection_GP,
           collection_NPFS = collection_NPFS,
           complete_Tx = complete_Tx,
           complete_Tx_H1N1 = complete_Tx_H1N1,
           p_H1N1 = p_H1N1,
           num_hosp = num_hosp,
           num_death = num_death))
}






