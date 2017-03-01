#
# project: i-sense
# N Green
# Feb 2017
#
# create transition probability arrays


library(reshape2)


ageGroups <- c("04", "514", "1524", "2544", "4564", "65.", "total")


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
  data.frame(from = "pos") %>%
  select(from, to, everything())


# complete treatment ----------------------------------------------------

trans_mat_Tx <-
  usersurvey %>%
  group_by(NPFS_weeks_window, age) %>%
  dplyr::summarise(num_obtain = sum(obtainantivirals),
                   num_start = sum(startedantivirals01),
                   num_complete = sum(completedantivirals01)) %>%
  mutate(coll_start = num_start/num_obtain,
         start_complete = num_complete/num_start) %>%
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


# novel test --------------------------------------------------------------
# calculate this inside model...

sens <- 0.9
spec <- 0.9

trans_mat_test <-
  coll_NPFS %>%
  dplyr::select(age, NPFS_weeks_window) %>%
  data.frame(H1N1_GP.neg = 1 - sens,
             H1N1_NPFS.neg = 1 - sens,
             notH1N1_NPFS.neg = spec,
             notH1N1_GP.neg = spec,
             H1N1_GP.pos = sens,
             H1N1_NPFS.pos = sens,
             notH1N1_NPFS.pos = 1 - spec,
             notH1N1_GP.pos = 1 - spec) %>%
  melt(id.vars = c("age", "NPFS_weeks_window"),
       variable.name = "fromto",
       value.name = "prob") %>%
  separate(fromto, c("from", "to"), "\\.") %>%
  select(from, to, everything())

