#
# project: i-sense
# N Green
# Feb 2017
#
# plots


library(gridExtra)


# threshold C-E unit test cost --------------------------------------------

maxCost_scenario1 <- maxCost(scenario1, scenario0)
maxCost_scenario2a <- maxCost(scenario2a, scenario0)
maxCost_scenario2b <- maxCost(scenario2b, scenario0)
maxCost_scenario2c <- maxCost(scenario2c, scenario0)
maxCost_scenario2d <- maxCost(scenario2d, scenario0)



# maxCost[is.infinite(-maxCost)] <- 0

# filled.contour(z = apply(maxCost, 1, rev),
#                xlab = "Sensitivity",
#                ylab = "Specificity",
#                color.palette = terrain.colors)

s1 <- lattice::levelplot(maxCost_scenario1,
                   xlab = "Specificity", ylab = "Sensitivity",
                   at = seq(0,5,0.5),
                   main = "scenario 1",
                   col.regions = gray(1 - 0:100/200)#,
                   # panel = function(...){
                   #   panel.contourplot(..., contour = TRUE)}
                   )

s2 <- lattice::levelplot(maxCost_scenario2a,
                   xlab = "Specificity", ylab = "Sensitivity",
                   at = seq(0,5,0.5),
                   main = "scenario 2a",
                   col.regions = gray(1 - 0:100/200)#,
                   # panel = function(...){
                   #   panel.contourplot(..., contour = TRUE)}
                    )

s3 <- lattice::levelplot(maxCost_scenario2b,
                   xlab = "Specificity", ylab = "Sensitivity",
                   at = seq(0,5,0.5),
                   main = "scenario 2b",
                   col.regions = gray(1 - 0:100/200)#,
                   # panel = function(...){
                   #   panel.contourplot(..., contour = TRUE)}
                    )

s4 <- lattice::levelplot(maxCost_scenario2c,
                   xlab = "Specificity", ylab = "Sensitivity",
                   at = seq(0,5,0.5),
                   main = "scenario 2c",
                   col.regions = gray(1 - 0:100/200)#,
                   # panel = function(...){
                   #   panel.contourplot(..., contour = TRUE)}
                    )

s5 <- lattice::levelplot(maxCost_scenario2d,
                   xlab = "Specificity", ylab = "Sensitivity",
                   at = seq(0,5,0.5),
                   main = "scenario 2d",
                   col.regions = gray(1 - 0:100/200)#,
                   # panel = function(...){
                   #   panel.contourplot(..., contour = TRUE)}
                    )

grid.arrange(s1, s2, s3, s4, ncol = 2)


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

INMBout <- array(data = NA,
                 dim = c(length(spec.seq), length(sens.seq)),
                 dimnames = list(spec.seq, sens.seq))
c_index <- c(1,2,3,4)

for (i in seq_along(spec.seq)) {
  for (j in seq_along(sens.seq)) {

    INMBout1[i,j] <-
      INMB(QALYgain = scenario0["e"] - scenario2d[i,j,c_index ,"e"],
           cost_incurred = scenario2d[i,j,c_index ,"c"] - scenario0["c"])

    INMBout2[i,j] <-
      INMB(QALYgain = scenario0["e"] - scenario2d[i,j,c_index ,"e"],
           cost_incurred = scenario2d[i,j,c_index ,"c"] - scenario0["c"])

    INMBout3[i,j] <-
      INMB(QALYgain = scenario0["e"] - scenario2d[i,j,c_index ,"e"],
           cost_incurred = scenario2d[i,j,c_index ,"c"] - scenario0["c"])

    INMBout4[i,j] <-
      INMB(QALYgain = scenario0["e"] - scenario2d[i,j,c_index ,"e"],
           cost_incurred = scenario2d[i,j,c_index ,"c"] - scenario0["c"])
  }
}

par(mfrow = c(2,2))

filled.contour(z = apply(INMBout1, 1, rev),
               xlab = "Sensitivity",
               ylab = "Specificity",
               color.palette = terrain.colors)

filled.contour(z = apply(INMBout2, 1, rev),
               xlab = "Sensitivity",
               ylab = "Specificity",
               color.palette = terrain.colors)

filled.contour(z = apply(INMBout3, 1, rev),
               xlab = "Sensitivity",
               ylab = "Specificity",
               color.palette = terrain.colors)

filled.contour(z = apply(INMBout4, 1, rev),
               xlab = "Sensitivity",
               ylab = "Specificity",
               color.palette = terrain.colors)
