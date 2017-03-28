#
# project: i-sense
# N Green
# Feb 2017
#
# create model inputs for decision tree: after service use


library(reshape2)


load("../../R/Ilarias-model/H1N1model/data/dates_lookup.RData")



# duplicate first NPFS week for missing pre-NPFS weeks --------------------

dat.npfs <-
  dat.npfs %>%
  filter(week == min(week)) %>%
  data.frame(weektemp = rep(FIRST_WEEK:29, each = length(ageGroups) + 1)) %>%
  select(-weeks_window, -NPFS_weeks_window, -NPFS_weeks, -week_start, -week, -week_end, -epiweek) %>%
  rename(week = weektemp) %>%
  merge(dates_lookup) %>%
  rbind.data.frame(dat.npfs)


# antiviral collection ----------------------------------------------------

num_dat_coll <-
  dat.npfs %>%
  group_by(NPFS_weeks_window, age) %>%
  dplyr::summarise(coll_NPFS = sum(coll),
                   auth_NPFS = sum(auth)) %>%
  mutate(auth_GP.coll = coll_NPFS/auth_NPFS,
         auth_NPFS.coll = auth_GP.coll)



num_dat <-
  num_dat_coll %>%
  select(-auth_NPFS) %>%
  merge(num_dat_ILI, by = c("NPFS_weeks_window", "age")) %>%
  mutate(coll_GP = auth_GP.coll*auth_GP,
         coll_NPFS = ifelse(NPFS_weeks_window == 1,
                            0, coll_NPFS))



# complete treatment ----------------------------------------------------

num_dat_Tx <-
  usersurvey %>%
  group_by(age) %>%
  dplyr::summarise(obtain = sum(obtainantivirals),
                   start = sum(startedantivirals01),
                   complete = sum(completedantivirals01)) %>%
  mutate(coll.start = start/obtain,
         start.complete = complete/start) %>%
  arrange(age) %>%
  select(age, everything()) %>%
  bind_rows(data.frame(age = "04",
                       filter(., age == "514")[-1])) %>%
  filter(complete.cases(.)) %>%
  slice(rep(1:n(), each = 3)) %>%
  mutate(NPFS_weeks_window = rep(1:3, times = length(ageGroups))) %>%
  select(-obtain, -start, -complete)


num_dat <-
  num_dat %>%
  merge(num_dat_Tx, by = c("NPFS_weeks_window", "age")) %>%
  mutate(start_GP = coll.start*coll_GP,
         start_NPFS = coll.start*coll_NPFS,
         complete_GP = start.complete*start_GP,
         complete_NPFS = start.complete*start_NPFS,
         complete_NPFS_H1N1 = complete_NPFS*p.NPFS_swab_pos,
         complete_GP_H1N1 = complete_GP*p.GP_swab_pos,
         complete = complete_NPFS + complete_GP,
         complete_H1N1 = complete_NPFS_H1N1 + complete_GP_H1N1,
         # Sx_H1N1 = H1N1_NPFS + H1N1_GP + notseekcare_H1N1,
         notcomplete_NPFS_H1N1 = H1N1_NPFS - complete_NPFS_H1N1,
         notcomplete_GP_H1N1 = H1N1_GP - complete_GP_H1N1,
         SxH1N1_notcomplete = Sx_H1N1 - complete_H1N1)


# hospitalisation ---------------------------------------------------------

# impact of outpatient neuraminidase inhibitor treatment on hospitalisation in patients
# infected with influenza A (H1N1)pdm09: An IPD analysis
# S. Venkatesan, P.R. Myles et al
completeTx.adj <- 0.52


# Presanis et al (2011) BMJ "Changes in severity of 2009 pandemic A/..."
num_dat_hosp <-
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
  mutate(complete.hosp = start.hosp*completeTx.adj)



num_dat <-
  num_dat %>%
  merge(num_dat_hosp, by = c("NPFS_weeks_window", "age")) %>%
  mutate(complete_hosp = complete_H1N1*complete.hosp,
         notcomplete_hosp = SxH1N1_notcomplete*ILI.hosp,
         hosp = complete_hosp + notcomplete_hosp,
         death = hosp*hosp.death)


num_dat_counts <-
  num_dat[, !grepl("\\.", colnames(num_dat))] %>%
  select(NPFS_weeks_window, age,
         pop,
         flu,
         Sx,
         Sx_H1N1,
         ILI_Dorigatti,
         notseekcare_H1N1,
         notseekcare_notH1N1,
         seekcare,
         auth_NPFS,
         auth_GP,
         H1N1_GP,
         notH1N1_GP,
         H1N1_NPFS,
         notH1N1_NPFS,
         coll_NPFS,
         coll_GP,
         start_GP,
         start_NPFS,
         complete_GP,
         complete_NPFS,
         complete,
         complete_hosp,
         notcomplete_hosp,
         complete_NPFS_H1N1,
         complete_GP_H1N1,
         complete_H1N1,
         notcomplete_NPFS_H1N1,
         notcomplete_GP_H1N1,
         SxH1N1_notcomplete,
         hosp,
         death)


num_dat_probs <-
  num_dat[, grepl("(\\.)|NPFS_weeks_window|age", colnames(num_dat))] %>%
  select(NPFS_weeks_window, age,
         pop.flu,
         flu.Sx,
         p.seekcare,
         Sx.notseekcare_H1N1,
         Sx.notseekcare_notH1N1,
         Sx.GP_H1N1,
         Sx.NPFS_H1N1,
         Sx.NPFS_notH1N1,
         Sx.GP_notH1N1,
         auth_GP.coll,
         auth_NPFS.coll,
         p.GP_swab_pos,
         p.NPFS_swab_pos,
         coll.start,
         start.complete,
         ILI.hosp,
         coll.hosp,
         start.hosp,
         complete.hosp,
         hosp.death)


#  ------------------------------------------------------------------------
#  transition probability matrix
#  ------------------------------------------------------------------------

trans_mat <-
  num_dat %>%
  melt(id.vars = c("age", "NPFS_weeks_window"),
       variable.name = "fromto",
       value.name = "prob",
       measure.vars = c("pop.flu",
                        "flu.Sx",
                        "Sx.GP_H1N1",
                        "Sx.NPFS_H1N1",
                        "Sx.NPFS_notH1N1",
                        "Sx.GP_notH1N1",
                        "Sx.notseekcare_H1N1",
                        "Sx.notseekcare_notH1N1",
                        "auth_NPFS.coll",
                        "auth_GP.coll",
                        "coll.start",
                        "start.complete",
                        "complete.hosp",
                        "ILI.hosp",
                        "coll.hosp",
                        "start.hosp",
                        "hosp.death")) %>%
  separate(fromto, c("from", "to"), "\\.") %>%
  select(from, to, everything()) %>%
  arrange(to)


write.csv(num_dat_counts, file = "../../data cleaned/num_dat_counts.csv")
write.csv(num_dat_probs, file = "../../data cleaned/num_dat_probs.csv")

