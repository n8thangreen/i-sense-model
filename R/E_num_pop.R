
#' Expected Total Number For Given Population
#'
#' @param num
#' @param pop Sub-population sizes in long format
#'
#' @return
#' @export
#'
#' @example
#'
E_num_pop <- function(num,
                      pop){

  out <- list()

  pop2 <- dcast(data = pop,
                NPFS_weeks_window ~ age,
                value.var = "pop")

  rownames(pop2) <- pop2$NPFS_weeks_window

  row_match <- intersect(rownames(pop2),
                         rownames(num[,,"flu"]))

  col_match <- intersect(colnames(pop2),
                         colnames(num[,,"flu"]))

  names_pop <- c("flu","ILI_NPFS","ILI_GP","new_NPFS","new_GP","collection_GP",
                 "collection_NPFS","complete_Tx","complete_Tx_H1N1","SxH1N1","hosp","death")

  for (i in seq_along(names_pop)) {

    out[[names_pop[i]]] <- pop2[row_match, col_match] * num[row_match, col_match,
                                                            names_pop[i]]
  }

  return(out)
}
