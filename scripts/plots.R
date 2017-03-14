#
# project: i-sense
# N Green
# Feb 2017
#
# plots




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

lattice::levelplot(maxCost_scenario1,
                   xlab = "Specificity", ylab = "Sensitivity",
                   col.regions = gray(1 - 0:100/200)#,
                   # panel = function(...){
                   #   panel.contourplot(..., contour = TRUE)}
                   )

lattice::levelplot(maxCost_scenario2a,
                   xlab = "Specificity", ylab = "Sensitivity",
                   col.regions = gray(1 - 0:100/200)#,
                   # panel = function(...){
                   #   panel.contourplot(..., contour = TRUE)}
)

lattice::levelplot(maxCost_scenario2b,
                   xlab = "Specificity", ylab = "Sensitivity",
                   col.regions = gray(1 - 0:100/200)#,
                   # panel = function(...){
                   #   panel.contourplot(..., contour = TRUE)}
)

lattice::levelplot(maxCost_scenario2c,
                   xlab = "Specificity", ylab = "Sensitivity",
                   col.regions = gray(1 - 0:100/200)#,
                   # panel = function(...){
                   #   panel.contourplot(..., contour = TRUE)}
)

lattice::levelplot(maxCost_scenario2d,
                   xlab = "Specificity", ylab = "Sensitivity",
                   col.regions = gray(1 - 0:100/200)#,
                   # panel = function(...){
                   #   panel.contourplot(..., contour = TRUE)}
)


# break-even test performance ---------------------------------------------

contour(z = maxCost_scenario1,
        xlab = "Specificity",
        ylab = "Sensitivity", levels = 0.5, labels = "scenario 1")

contour(z = maxCost_scenario2a,
        xlab = "Specificity",
        ylab = "Sensitivity", levels = 0.5, add = TRUE, labels = "scenario 2a")

contour(z = maxCost_scenario2b,
        xlab = "Specificity",
        ylab = "Sensitivity", levels = 0.5, add = TRUE, labels = "scenario 2b")

contour(z = maxCost_scenario2c,
        xlab = "Specificity",
        ylab = "Sensitivity", levels = 0.5, add = TRUE, labels = "scenario 2c")

contour(z = maxCost_scenario2d,
        xlab = "Specificity",
        ylab = "Sensitivity", levels = 0.5, add = TRUE, labels = "scenario 2d")



# INMB --------------------------------------------------------------------

INMBout <- array(data = NA,
                 dim = c(length(spec_GP.seq), length(sens_GP.seq)),
                 dimnames = list(spec_GP.seq, sens_GP.seq))
c_index <- 2

for (i in seq_along(spec_GP.seq)) {
  for (j in seq_along(sens_GP.seq)) {

    INMBout[i,j] <-
      INMB(e = scenario0["e"] - scenario2d[i,j,c_index ,"e"],
           c = scenario2d[i,j,c_index ,"c"] - scenario0["c"])
  }
}

filled.contour(z = apply(INMBout, 1, rev),
               xlab = "Sensitivity",
               ylab = "Specificity",
               color.palette = terrain.colors)
