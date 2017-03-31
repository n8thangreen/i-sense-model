#
# project: i-sense
# N Green
# Feb 2017
#
# sens-spec-cost grid plots


library(gridExtra)



# threshold C-E unit test cost --------------------------------------------

maxCost_scenario1  <- maxCost(interv = scenario1, status_quo = scenario0)
maxCost_scenario2a <- maxCost(interv = scenario2a, status_quo = scenario0)
maxCost_scenario2b <- maxCost(interv = scenario2b, status_quo = scenario0)
maxCost_scenario2c <- maxCost(interv = scenario2c, status_quo = scenario0)
maxCost_scenario2d <- maxCost(interv = scenario2d, status_quo = scenario0)

AT_SEQ <- seq(0, 16, 1)

# maxCost_scenario1[is.infinite(-maxCost_scenario1)] <- 0

# filled.contour(z = apply(maxCost, 1, rev),
#                xlab = "Sensitivity",
#                ylab = "Specificity",
#                color.palette = terrain.colors)

s1 <-
  lattice::levelplot(maxCost_scenario1,
                     xlab = "Specificity (%)", ylab = "Sensitivity (%)",
                     at = seq(0, 30, 1),
                     main = "Scenario 1",
                     col.regions = terrain.colors(30),
                     scales = list(
                       x = list(at = seq(1, 21, 2),
                                labels = c("0", "20", "40", "60", "80", "100")),
                       y = list(at = seq(1, 21, 2),
                                labels = c("0", "20", "40", "60", "80", "100"))))#,
#                         col.regions = grey(1 - 0:100/200)#,
#                          # panel = function(...){
#                          #   panel.contourplot(..., contour = TRUE)}
# )

s2 <-
  lattice::levelplot(maxCost_scenario2a,
                     xlab = "Specificity (%)", ylab = "Sensitivity (%)",
                     at = AT_SEQ,
                     main = "Scenario 2a",
                     col.regions = terrain.colors(16),
                     scales = list(
                       x = list(at = seq(1, 21, 2),
                                labels = c("0", "20", "40", "60", "80", "100")),
                       y = list(at = seq(1, 21, 2),
                                labels = c("0", "20", "40", "60", "80", "100"))))#,
#                          col.regions = gray(1 - 0:100/200)#,
#                          # panel = function(...){
#                          #   panel.contourplot(..., contour = TRUE)}
# )

s3 <-
  lattice::levelplot(maxCost_scenario2b,
                     xlab = "Specificity (%)", ylab = "Sensitivity (%)",
                     at = AT_SEQ,
                     main = "Scenario 2b",
                     col.regions = terrain.colors(16),
                     scales = list(
                       x = list(at = seq(1, 21, 2),
                                labels = c("0", "20", "40", "60", "80", "100")),
                       y = list(at = seq(1, 21, 2),
                                labels = c("0", "20", "40", "60", "80", "100"))))#,
#                          col.regions = gray(1 - 0:100/200)#,
#                          # panel = function(...){
#                          #   panel.contourplot(..., contour = TRUE)}
# )

s4 <-
  lattice::levelplot(maxCost_scenario2c,
                     xlab = "Specificity (%)", ylab = "Sensitivity (%)",
                     at = AT_SEQ,
                     main = "Scenario 2c",
                     col.regions = terrain.colors(16),
                     scales = list(
                       x = list(at = seq(1, 21, 2),
                                labels = c("0", "20", "40", "60", "80", "100")),
                       y = list(at = seq(1, 21, 2),
                                labels = c("0", "20", "40", "60", "80", "100"))))#,
#                          col.regions = gray(1 - 0:100/200)#,
#                          # panel = function(...){
#                          #   panel.contourplot(..., contour = TRUE)}
# )

s5 <-
  lattice::levelplot(maxCost_scenario2d,
                     xlab = "Specificity (%)", ylab = "Sensitivity (%)",
                     at = AT_SEQ,
                     main = "Scenario 2d",
                     col.regions = terrain.colors(16),
                     scales = list(
                       x = list(at = seq(1, 21, 2),
                                labels = c("0", "20", "40", "60", "80", "100")),
                       y = list(at = seq(1, 21, 2),
                                labels = c("0", "20", "40", "60", "80", "100"))))#,
#                          col.regions = gray(1 - 0:100/200)#,
#                          # panel = function(...){
#                          #   panel.contourplot(..., contour = TRUE)}
# )

s1
s2
s3
s4
s5

par(mar=c(5.1,4.1,4.1,2.1))
x11()
grid.arrange(s1, s2, s3, s4, s5, ncol = 2)

grid.arrange(s1, arrangeGrob(s2, s3), arrangeGrob(s4, s5), ncol = 3)




# break-even test performance frontiers --------------------------------------

contour(z = maxCost_scenario1,
        xlab = "Specificity",
        ylab = "Sensitivity", levels = 0.5, labels = "scenario 1")

contour(z = maxCost_scenario2a,
        xlab = "Specificity",
        ylab = "Sensitivity", levels = 0.5, add = TRUE, labels = "scenario 2a", lty = 2)

contour(z = maxCost_scenario2b,
        xlab = "Specificity",
        ylab = "Sensitivity", levels = 0.5, add = TRUE, labels = "scenario 2b", lwd = 2)

contour(z = maxCost_scenario2c,
        xlab = "Specificity",
        ylab = "Sensitivity", levels = 0.5, add = TRUE, labels = "scenario 2c", lty = 2, lwd = 2)

contour(z = maxCost_scenario2d,
        xlab = "Specificity",
        ylab = "Sensitivity", levels = 0.5, add = TRUE, labels = "scenario 2d")



# INMB for fixed costs -----------------------------------------------------

##TODO##

c_index <- c(1,2,3,4)

INMBout1 <-
  INMBout2a <-
  INMBout2b <-
  INMBout2c <- array(data = NA,
                 dim = c(length(spec.seq), length(sens.seq), length(c_index)),
                 dimnames = list(spec.seq, sens.seq, c_index))

for (k in seq_along(c_index)) {
  for (i in seq_along(spec.seq)) {
    for (j in seq_along(sens.seq)) {

      INMBout1[i,j,k] <-
        INMB(QALYgain = scenario0["e"] - scenario1[i,j,c_index[k] ,"e"],
             cost_incurred = scenario1[i,j,c_index[k] ,"c"] - scenario0["c"])

      INMBout2a[i,j,k] <-
        INMB(QALYgain = scenario0["e"] - scenario2a[i,j,c_index[k] ,"e"],
             cost_incurred = scenario2a[i,j,c_index[k] ,"c"] - scenario0["c"])

      INMBout2b[i,j,k] <-
        INMB(QALYgain = scenario0["e"] - scenario2b[i,j,c_index[k] ,"e"],
             cost_incurred = scenario2b[i,j,c_index[k] ,"c"] - scenario0["c"])

      INMBout2c[i,j,k] <-
        INMB(QALYgain = scenario0["e"] - scenario2c[i,j,c_index ,"e"],
             cost_incurred = scenario2c[i,j,c_index[k] ,"c"] - scenario0["c"])
    }
  }
}

par(mfrow = c(2,2))

filled.contour(z = apply(INMBout1[,,1,], 1, rev),
               xlab = "Sensitivity",
               ylab = "Specificity",
               color.palette = terrain.colors)

filled.contour(z = apply(INMBout2a[,,1,], 1, rev),
               xlab = "Sensitivity",
               ylab = "Specificity",
               color.palette = terrain.colors)

filled.contour(z = apply(INMBout2b[,,1,], 1, rev),
               xlab = "Sensitivity",
               ylab = "Specificity",
               color.palette = terrain.colors)

filled.contour(z = apply(INMBout2c[,,1,], 1, rev),
               xlab = "Sensitivity",
               ylab = "Specificity",
               color.palette = terrain.colors)



#  alls scenarios frontier -------------------------------------------------------------------

maxCost_scenario1[maxCost_scenario1< -10] <- 0
maxCost_scenario1[maxCost_scenario1!=0] <- 1
maxCost_scenario2a[maxCost_scenario2a< -10] <- 0
maxCost_scenario2a[maxCost_scenario2a!=0] <- 1
maxCost_scenario2b[maxCost_scenario2b< -10] <- 0
maxCost_scenario2b[maxCost_scenario2b!=0] <- 1
maxCost_scenario2c[maxCost_scenario2c< -10] <- 0
maxCost_scenario2c[maxCost_scenario2c!=0] <- 1

combined <- maxCost_scenario1 + maxCost_scenario2a + maxCost_scenario2b + maxCost_scenario2c
lattice::levelplot(combined,
                   xlab = "Specificity", ylab = "Sensitivity",
                   main = "combined",
                   col.regions = gray(1 - 0:100/200))


