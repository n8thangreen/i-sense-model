
#' INMB
#'
#' @param QALYgain
#' @param cost_incurred
#' @param wtp Default to £30,000/QALY
#'
#' @return
#' @export
#'
#' @examples
#'
INMB <- function(QALYgain,
                 cost_incurred,
                 wtp = 30000){

  (QALYgain*wtp) - cost_incurred
}


#' maxCost
#'
#' @param interv
#' @param status_quo
#'
#' @return
#' @export
#'
#' @examples
#'
maxCost <- function(interv,
                    status_quo) {

  spec_GP.seq <- dimnames(interv)[[1]]
  sens_GP.seq <- dimnames(interv)[[2]]

  maxCost <- array(data = NA,
                   dim = c(length(spec_GP.seq), length(sens_GP.seq)),
                   dimnames = list(spec_GP.seq, sens_GP.seq))

  for (i in seq_along(spec_GP.seq)) {
    for (j in seq_along(sens_GP.seq)) {

      maxCost[i,j] <-
        which(INMB(QALYgain = status_quo["e"] - interv[i,j, ,"e"],
                   cost_incurred = interv[i,j, ,"c"] - status_quo["c"]) > 0) %>%
        names() %>%
        as.numeric() %>% max()
    }
  }
  return(maxCost)
}
