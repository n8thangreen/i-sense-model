
#' Expected Total Cost Incurred and QALY Loss For Given Population
#'
#' @param cost_QALY
#' @param pop Sub-population sizes in long format
#'
#' @return
#' @export
#'
#' @example
#' cost_QALY <- trans_mat %>%
#'              Ec_by_age_window(spec_GP = 0.6,
#'                               sens_GP = 0.8,
#'                               c_testGP = 10)
#' Ec_pop(cost_QALY, pop = pop_age_window)
#'
Ec_pop <- function(cost_QALY,
                   pop){

  out <- list()

  pop2 <- dcast(data = pop,
                NPFS_weeks_window ~ age,
                value.var = "pop")

  rownames(pop2) <- pop2$NPFS_weeks_window

  row_match <- intersect(rownames(pop2),
                         rownames(cost_QALY[,,"c"]))

  col_match <- intersect(colnames(pop2),
                         colnames(cost_QALY[,,"c"]))

  out$c <- pop2[row_match, col_match] * cost_QALY[row_match, col_match, "c"]
  out$e <- pop2[row_match, col_match] * cost_QALY[row_match, col_match, "e"]

  return(out)
}
