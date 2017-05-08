

library(ggplot2)
library(reshape2)


##TODO: transform/remove probabilities to what is best to plot
##TODO: reorder fields to same as in decision tree


plot_data <-
  num_dat_probs %>%
  # filter(age == "04") %>%
  melt(id.vars = c("age", "NPFS_weeks_window"))

ggplot(data = plot_data) +
  aes(x = NPFS_weeks_window, y = value) +
  geom_area(aes(colour = variable, fill = variable), position = 'stack') +
  ylab("Population") + xlab("week") +
  theme_bw() + scale_colour_grey() + facet_grid(. ~ age) # + scale_fill_grey(start = 0.4, end = .9)













