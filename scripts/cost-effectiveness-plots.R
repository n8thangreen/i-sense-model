#
# project: i-sense
# N Green
# March 2017
#
# cost-effectiveness plots


plot(scenario1_sensitivity$e - scenario0_sensitivity$e,
     scenario0_sensitivity$c - scenario1_sensitivity$c)


bcea_res <- bcea(cbind(scenario0_sensitivity$e, scenario1_sensitivity$e),
                 cbind(scenario0_sensitivity$c, scenario1_sensitivity$c))

ceplane.plot(bcea_res,
             wtp = 20000)
contour(bcea_res,
        wtp = 20000)

contour2(bcea_res,
         wtp = 20000, graph = "ggplot2")

