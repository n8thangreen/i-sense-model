#
# project: i-sense
# N Green
# March 2017
#
# time-series plots adjusting for imperfect test


library(ggplot2)


load("C:/Users/nathan.green.PHE/Dropbox/i-sense/data raw/data_positiveILI_NPFS_perrine_march2017.RData")
load("C:/Users/nathan.green.PHE/Dropbox/i-sense/data raw/data_positiveILI_GP_perrine_march2017.RData")


sensitivity <- 0.6


dat.posILI.NPFS <-
  dat.posILI.NPFS %>%
  mutate(positivity = posILI/(authorisations*sensitivity))

ggplot2::ggplot(dat.posILI.NPFS, aes(x=w, y=positivity, colour=ageGP.fac, group=ageGP.fac)) +
  geom_line() + theme_bw() + geom_abline(slope = 0, intercept = 0.5) + geom_abline(slope = 0, intercept = 0.7) +
  ggtitle("NPFS")


dat.posILI.GP <-
  dat.posILI.GP %>%
  mutate(positivity = posILI/(estim.consult*sensitivity))

ggplot2::ggplot(dat.posILI.GP, aes(x=week, y=positivity, colour=ageGP.fac, group=ageGP.fac)) +
  geom_line() + theme_bw() + geom_abline(slope = 0, intercept = 0.5) + geom_abline(slope = 0, intercept = 0.7) +
  ggtitle("GP")

