# Merge pre and post datasets on matchID

dat <- merge(pre, post, by = "matchID", all = T)
names(dat)

dat$matchID
head(dat)

summary(dat)
