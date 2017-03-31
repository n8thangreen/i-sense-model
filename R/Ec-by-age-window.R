
#' Expected Cost by Age and Time Window
#'
#' Default to no intervention (repid test) scenario.
#'
#' @param transmat Transition matrix with covariates age and time window in long (tidy) format
#' @param spec_NPFS
#' @param sens_NPFS
#' @param spec_GP
#' @param sens_GP
#' @param c_testNPFS
#' @param c_testGP
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' Ec_by_age_window(trans_mat)
#'
#' spec_NPFS <- 0
#' sens_NPFS <- 1
#' spec_GP <- 0
#' sens_GP <- 1
#' c_testNPFS <- 0
#' c_testGP <- 0
#'
Ec_by_age_window <- function(trans_mat,
                             spec_NPFS = 0,
                             sens_NPFS = 1,
                             spec_GP = 0,
                             sens_GP = 1,
                             c_testNPFS = 0,
                             c_testGP = 0,
                             ...){

  AGE <- levels(trans_mat$age)
  AGE <- AGE[!AGE %in% c("overall", "total", NA)]

  WINDOW <- unique(trans_mat$NPFS_weeks_window) %>% sort()

  out <- array(data = NA,
               dim = c(length(WINDOW),  length(AGE), 2),
               dimnames =  list(WINDOW, AGE, c("c","e")))

  for (j in seq_along(AGE)) {
    for (i in seq_along(WINDOW)) {

      input <-
        trans_mat %>%
        filter(age == AGE[j],
               NPFS_weeks_window == WINDOW[i])

      try(out[i, j, ] <- with(input,
                           Ec_ILI(
                             # service use
                             p_GP.H1N1 = prob[from == "Sx" & to == "GP_H1N1"],
                             p_GP.notH1N1 = prob[from == "Sx" & to == "GP_notH1N1"],
                             p_NPFS.H1N1 = prob[from == "Sx" & to == "NPFS_H1N1"],
                             p_NPFS.notH1N1 = prob[from == "Sx" & to == "NPFS_notH1N1"],
                             p_notseekcare_H1N1 = prob[from == "Sx" & to == "notseekcare_H1N1"],
                             p_notseekcare_notH1N1 = prob[from == "Sx" & to == "notseekcare_notH1N1"],
                             p_Sx = prob[from == "flu" & to == "Sx"],
                             p_flu = prob[from == "pop" & to == "flu"],

                             # treatment
                             p_GP.collect = prob[from == "Rx_GP" & to == "coll"],
                             p_NPFS.collect = prob[from == "auth_NPFS" & to == "coll"],
                             p_start = prob[from == "coll" & to == "start"],
                             p_complete = prob[from == "start" & to == "complete"],
                             p_hosp = prob[from == "ILI" & to == "hosp"],
                             p_hosp_complete = prob[from == "complete" & to == "hosp"],
                             p_death = prob[from == "hosp" & to == "death"],

                             # test performance
                             spec_NPFS = spec_NPFS,
                             sens_NPFS = sens_NPFS,
                             spec_GP = spec_GP,
                             sens_GP = sens_GP,

                             # costs incurred
                             c_testNPFS = c_testNPFS,
                             c_testGP = c_testGP,

                             # QALY loss
                             Q_excess_life = excess_life_QALYs(AGE[j]),
                             ...)
      ), silent = TRUE)
    }
  }

  return(out)
}

