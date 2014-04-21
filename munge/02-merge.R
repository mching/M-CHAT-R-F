# Merge pre and post datasets on matchID

dat <- merge(pre, post, by = "matchID", all = T)
names(dat)

# Combine care_direct (if either one is 1, then make a 1, if either is 0, then make a 0, else NA)
# Make a combo variable for care_direct
dat$combo_care_direct <- rep(NA, length(dat[, 1]))
dat$combo_care_direct <- ifelse(dat$pre_care_direct == 1 | dat$post_care_direct == 1, 1, 0)

# for those that are NA, check if one of the variables is 0
dat$combo_care_direct[is.na(dat$combo_care_direct)] <-
  ifelse(dat$pre_care_direct[is.na(dat$combo_care_direct)] == 0 |
           dat$post_care_direct[is.na(dat$combo_care_direct)] == 0, 0, NA
           )

table(dat$combo_care_direct)

# it turns out that all the NAs were really 0s so we could have just assigned 0 to NAs
data.frame(pre = dat$pre_care_direct, post = dat$post_care_direct, combined = dat$combo_care_direct)

## Combine care_supervise into role
dat$combo_care_supervise <- rep(NA, length(dat[, 1]))
dat$combo_care_supervise <- ifelse(dat$pre_care_supervise == 1 | dat$post_care_supervise == 1, 10, 0)
dat$combo_care_supervise[is.na(dat$combo_care_supervise)] <- 0
dat$combo_role <- dat$combo_care_supervise + dat$combo_care_direct 
# 0 = no role, 1 = direct care only, 10 = supervise only, 11 = direct care and supervise
table(dat$combo_role)
dat$combo_role <- factor(dat$combo_role, labels = c("no care", "direct care only", "supervise only", "direct care & supervise"))

# retain only those whoe have some role (drop no care)
dat_all <- dat
dat <- dat

# combine number of ASD patients post_q0015 and pre_q0012
cbind(dat$pre_q0012, dat$post_q0015)

dat$combo_n_ASD_pts <- rep(NA, length(dat[,1]))
for(i in 1:length(dat[,1])) {
  if(is.na(dat$pre_q0012[i]) & is.na(dat$post_q0015[i])) dat$combo_n_ASD_pts[i] <- NA
  else if(!is.na(dat$post_q0015[i])) dat$combo_n_ASD_pts[i] <- dat$post_q0015[i]
  else if(!is.na(dat$pre_q0012[i])) dat$combo_n_ASD_pts[i] <- dat$pre_q0012[i]
}
dat$combo_n_ASD_pts <- factor(dat$combo_n_ASD_pts, labels = levels(dat$pre_q0012))
dat$combo_n_ASD_pts
summary(dat$combo_n_ASD_pts)

# combine type of doctor (attending general ped, subspecialist, resident level)
summary(dat$pre_q0013)
summary(dat$post_q0016)

dat$combo_attending_resident <- dat$pre_q0013
for(i in 1:length(dat[,1])) {
  if(is.na(dat$pre_q0013[i])) dat$combo_attending_resident[i] <- dat$post_q0016[i]
}


