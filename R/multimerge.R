#' multimerge
#'
#' @param LIST
#' @param BY
#'
#' @return
#' @export
#'
#' @examples
multimerge <- function(LIST, BY){

  out <- LIST[[1]]

  for (i in seq_along(LIST)[-1]) {

    out <- merge(out, LIST[[i]], by = BY)
  }

  names(out)[!names(out) %in% c("window","variable")] <- names(LIST)

  return(out)
}
