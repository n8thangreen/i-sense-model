
with(num_dat_probs, Sx.notseekcare_H1N1 +   Sx.notseekcare_notH1N1+ Sx.GP_H1N1 + Sx.NPFS_H1N1 + Sx.NPFS_notH1N1 + Sx.GP_notH1N1)

with(num_dat_probs, complete.hosp/ILI.hosp)

with(num_dat_probs, auth_NPFS.coll*coll.start*start.complete*complete.hosp*hosp.death)
