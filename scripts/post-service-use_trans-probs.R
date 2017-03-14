#
# project: i-sense
# N Green
# Feb 2017
#
# create transition probability arrays


library(reshape2)


load("C:/Users/Nathan/Dropbox/i-sense/R/Ilarias-model/H1N1model/data/dates_lookup.RData")

ageGroups <- c("04", "514", "1524", "2544", "4564", "65.")


# duplicate first NPFS week for missing pre-NPFS weeks --------------------

dat.npfs <-
  dat.npfs %>%
  filter(week == min(week)) %>%
  data.frame(weektemp = rep(FIRST_WEEK:29, each = length(ageGroups))) %>%
  select(-weeks_window, -NPFS_weeks_window, -NPFS_weeks, -week_start, -week, -week_end, -epiweek) %>%
  rename(week = weektemp) %>%
  merge(dates_lookup) %>%
  rbind.data.frame(dat.npfs)


# antiviral collection ----------------------------------------------------

trans_mat_coll <-
  dat.npfs %>%
  group_by(NPFS_weeks_window, age) %>%
  dplyr::summarise(num_coll = sum(coll),
                   num_auth = sum(auth)) %>%
  mutate(coll = num_coll/num_auth) %>%
  melt(id.vars = c("age", "NPFS_weeks_window"),
       measure.vars = "coll",
       variable.name = "to",
       value.name = "prob") %>%
  data.frame(from = "GP") %>%
  select(from, to, everything())

# duplicate from-GP for from-NPFS
trans_mat_coll <-
  trans_mat_coll %>%
  select(-from) %>%
  data.frame(from = "NPFS") %>%
  select(from, to, everything()) %>%
  rbind(trans_mat_coll)


# complete treatment ----------------------------------------------------
#
#TODO: missing ages
# mising all ages window 1
# missing 04 age  window 2 and 3
#
#


# duplicate first NPFS week for missing pre-NPFS weeks --------------------

## too sparse data to do by week windows directly
# filter(complete.cases(.)) %>%
# trans_mat_Tx <-
#   usersurvey %>%
#   group_by(age) %>%
#   dplyr::summarise(num_obtain = sum(obtainantivirals),
#                    num_start = sum(startedantivirals01),
#                    num_complete = sum(completedantivirals01)) %>%
#   mutate(coll.start = num_start/num_obtain,
#          start.complete = num_complete/num_start) %>%
#   melt(id.vars = c("age", "NPFS_weeks_window"),
#        measure.vars = c("coll.start", "start.complete"),
#        variable.name = "fromto",
#        value.name = "prob") %>%
#   separate(fromto, c("from", "to"), "\\.") %>%
#   select(from, to, everything())


trans_mat_Tx <-
  usersurvey %>%
  group_by(age) %>%
  dplyr::summarise(num_obtain = sum(obtainantivirals),
                   num_start = sum(startedantivirals01),
                   num_complete = sum(completedantivirals01)) %>%
  mutate(coll.start = num_start/num_obtain,
         start.complete = num_complete/num_start) %>%
  arrange(age) %>%
  select(age, everything()) %>%
  bind_rows(data.frame(age = "04",
                       filter(., age == "514")[-1])) %>%
  filter(complete.cases(.)) %>%
  slice(rep(1:n(), each = 3)) %>%
  mutate(NPFS_weeks_window = rep(1:3, times = length(ageGroups))) %>%
  melt(id.vars = c("age", "NPFS_weeks_window"),
       measure.vars = c("coll.start", "start.complete"),
       variable.name = "fromto",
       value.name = "prob") %>%
  separate(fromto, c("from", "to"), "\\.") %>%
  select(from, to, everything())




# hospitalisation ---------------------------------------------------------

# impact of outpatient neuraminidase inhibitor treatment on hospitalisation in patients
# infected with influenza A (H1N1)pdm09: An IPD analysis
# S. Venkatesan∗, P.R. Myles et al
completeTx.adj <- 0.52


# Presanis et al (2011) BMJ "Changes in severity of 2009 pandemic A/..."
trans_mat_hosp <-
  data.frame(age = ageGroups,
             NPFS_weeks_window = 1,
             ILI.hosp = 0.0054,
             coll.hosp = 0.0054,
             start.hosp = 0.0054,
             hosp.death = 0.028) %>%
  rbind(
    data.frame(age = ageGroups,
               NPFS_weeks_window = 2,
               ILI.hosp = 0.0001,
               coll.hosp = 0.0001,
               start.hosp = 0.0001,
               hosp.death = 0.028)) %>%
  rbind(
    data.frame(age = ageGroups,
               NPFS_weeks_window = 3,
               ILI.hosp = 0.0055,
               coll.hosp = 0.0055,
               start.hosp = 0.0055,
               hosp.death = 0.032)) %>%
  mutate(complete.hosp = start.hosp*completeTx.adj) %>%
  melt(id.vars = c("age", "NPFS_weeks_window"),
       variable.name = "fromto",
       value.name = "prob") %>%
  separate(fromto, c("from", "to"), "\\.") %>%
  select(from, to, everything())



trans_mat <- rbind(trans_mat_ILI,
                   trans_mat_coll,
                   trans_mat_Tx,
                   trans_mat_hosp)


