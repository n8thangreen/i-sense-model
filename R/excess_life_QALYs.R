
#' Calculate Excess Lifetime QALY loss (Due to Death)
#'
#' TODO:
#' * generalise so that can provide argument lookup tables
#' * use original QoL table age groups (different from AGE)
#' * other than 3.5\% discount
#'
#' @param AGE Age group from "04", "514", "1524", "2544", "4564", "65."
#'
#' @return
#' @export
#'
#' @examples
#' excess_life_QALYs("1524")
#' excess_life_QALYs("2544")
#' excess_life_QALYs("4564")
#'
excess_life_QALYs <- function(AGE) {

  life_expectancy <- read.csv("C:/Users/Nathan/Dropbox/i-sense/data raw/agegroup_excess-life-expectancy.csv",
                              colClasses = c("character", "numeric", "numeric", "numeric"))

  QoL_age <- read.csv("C:/Users/Nathan/Dropbox/i-sense/data raw/QoL_age.csv",
                      colClasses = c("character", "numeric"))

  rownames(QoL_age) <- QoL_age$age

  years <-
    life_expectancy$years_remaining[life_expectancy$age == AGE] %>%
    ceiling()

  # from age group to continuous then back again
  mid_age <- life_expectancy$mid_age[life_expectancy$age == AGE]
  ages.seq <- cut(mid_age + 1:years, breaks = c(-1, life_expectancy$max_age))
  ages.seq <- plyr::mapvalues(ages.seq,
                              from = c("(-1,4]", "(4,14]", "(14,24]", "(24,44]", "(44,64]", "(64,100]"),
                              to = c("04", "514", "1524", "2544", "4564", "65."))

  QoL <- QoL_age[ages.seq, "QoL"]

  return(sum(QoL/(1 + 0.035)^seq(0, years - 1)))
}
