
#' Expected Total Cost Incurred and QALY Loss For Given Population
#'
#' @param cost_QALY
#' @param total_service Sub-population sizes in long format
#'
#' @return
#' @export
#'
Ec_pop <- function(cost_QALY,
                   total_service){

  out <- list()

  total_service2 <- dcast(data = total_service,
                          NPFS_weeks_window ~ age,
                          value.var = "total_service")

  rownames(total_service2) <- total_service2$NPFS_weeks_window

  row_match <- intersect(rownames(total_service2),
                         rownames(cost_QALY[,,"c"]))

  col_match <- intersect(colnames(total_service2),
                         colnames(cost_QALY[,,"c"]))

  out$c <- total_service2[row_match, col_match] * cost_QALY[row_match, col_match, "c"]
  out$e <- total_service2[row_match, col_match] * cost_QALY[row_match, col_match, "e"]

  return(out)
}
