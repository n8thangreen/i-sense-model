#
#
#
# N Green
# Feb 2017
#
# lagged surveillance


library(ggplot2)
library(magrittr)
library(dplyr)
library(reshape2)
library(gridExtra)


load(file = "../../data cleaned/data_positiveILI_GP_perrine_feb2017.RData")
load(file = "../../data cleaned/data_positiveILI_NPFS_perrine_feb2017.RData")

# with swabbing C.I.s
# load("../../data raw/data_positiveILI_NPFS_perrine_march2017.RData")
# load("../../data raw/data_positiveILI_GP_perrine_march2017.RData")

load("../../R/Ilarias-model/H1N1model/data/dates_lookup.RData")


SENS <- 0.5


posILI.GP <-
  dat.posILI.GP %>%
  group_by(week) %>%
  dplyr::summarise(posILI_GP = sum(posILI),
                   auth_GP = sum(estim.consult))

posILI.NPFS <-
  dat.posILI.NPFS %>%
  group_by(week) %>%
  dplyr::summarise(posILI_NPFS = sum(posILI),
                   auth_NPFS = sum(authorisations))

dat <-
  posILI.GP %>%
  merge(posILI.NPFS, by = "week", all = TRUE) %>%
  rowwise() %>%
  mutate(posILI = sum(c(posILI_GP, posILI_NPFS), na.rm = TRUE),
         auth = sum(c(auth_GP, auth_NPFS), na.rm = TRUE))


# uncertainty

dat <-
  dat %>%
  mutate(negILI = auth - posILI,
         hi_PCTposILI = qbinom(p = 0.975, size = round(posILI/SENS), prob = 0.73), #upper-bound positive after test
         lo_PCTposILI = qbinom(p = 0.025, size = round(posILI/SENS), prob = 0.73), #lower-bound positive after test
         hi_PCTnegILI = qbinom(p = 0.975, size = round(negILI), prob = 0.04), #upper-bound positive after test
         lo_PCTnegILI = qbinom(p = 0.025, size = round(negILI), prob = 0.04), #lower-bound positive after test
         hi_PCT = hi_PCTposILI + hi_PCTnegILI,
         lo_PCT = lo_PCTposILI + lo_PCTnegILI)

# join with week lookup
dat <-
  dat %>%
  merge(dates_lookup, all = TRUE) %>%
  mutate(week_end = as.Date(week_end))


dat <- dat[1:50, ]


# subset for different time windows ---------------------------------------

subdat <- list()
week_cuts <- c(6, 12, 18, 37)

dat$posILIcurrent <- dat$posILI

for (i in seq_along(week_cuts)) {

  subdat[[i]] <- dat
  subdat[[i]][subdat[[i]]$NPFS_weeks > week_cuts[i] + 2, c("posILIcurrent", "auth", "hi_PCTposILI", "lo_PCTposILI",
                                             "hi_PCTnegILI", "lo_PCTnegILI", "hi_PCT", "lo_PCT")] <- NA

  subdat[[i]][subdat[[i]]$NPFS_weeks > week_cuts[i], c("posILI", "negILI")] <- NA # "posILI.lo", "posILI.hi",

  # subdat[[i]][subdat[[i]]$NPFS_weeks < 11, c("hi_PCTposILI", "lo_PCTposILI",
  #                              "hi_PCTnegILI", "lo_PCTnegILI", "hi_PCT", "lo_PCT")] <- NA
}




# plots -------------------------------------------------------------------

surveill_plot <- function(DATA,
                          x_axis_labels = FALSE,
                          TITLE = "") {

  gg <-
    ggplot(data = DATA) +
    theme_minimal() +
    labs(title = TITLE) +
    ylab("Population") + xlab("Date") +
    geom_line(aes(x = week_end, y = auth), color = "black") +
    geom_line(aes(x = week_end, y = posILI/SENS), color = "blue", show.legend = FALSE) +   #upwards adjust for swab sensitivity
    geom_line(aes(x = week_end, y = posILIcurrent/SENS), color = "black", linetype = "dotted") #+
    # geom_ribbon(aes(x = week_end, ymin = lo_PCTposILI, ymax = lo_PCTnegILI + lo_PCTposILI),
    #             fill = "blue", alpha = 0.25) +
    # geom_ribbon(aes(x = week_end, ymin = 0, ymax = lo_PCTposILI),
    #             fill = "red", alpha = 0.25)

  if (!x_axis_labels) {
    gg <- gg + theme(axis.title.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.ticks.x = element_blank())}

  gg
}


# pdf("plots/surveillance_grid_plot.pdf")
# tiff('plots/surveillance_grid_plot.tiff', units = "in", width = 20, height = 20, res = 300)

gridExtra::grid.arrange(
  surveill_plot(subdat[[1]], TITLE = "(a)"),
  surveill_plot(subdat[[2]], TITLE = "(b)"),
  surveill_plot(subdat[[3]], TITLE = "(c)"),
  surveill_plot(subdat[[4]], x_axis_labels = TRUE, TITLE = "(d)"),
  ncol = 1)

# dev.off()
