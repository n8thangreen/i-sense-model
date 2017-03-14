# plots


INMB <- function(e, c, wtp = 20000){
  e*wtp - c
}


maxCost <- function(interv, status_quo) {

  spec_GP.seq <- dimnames(interv)[[1]]
  sens_GP.seq <- dimnames(interv)[[2]]

  maxCost <- array(data = NA,
                   dim = c(length(spec_GP.seq), length(sens_GP.seq)),
                   dimnames = list(spec_GP.seq, sens_GP.seq))

  for (i in seq_along(spec_GP.seq)) {
    for (j in seq_along(sens_GP.seq)) {

      maxCost[i,j] <-
        which(INMB(e = status_quo["e"] - interv[i,j, ,"e"],
                   c = interv[i,j, ,"c"] - status_quo["c"]) > 0) %>%
        names() %>%
        as.numeric() %>% max()
    }
  }
  return(maxCost)
}
