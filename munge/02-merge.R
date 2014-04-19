# Merge pre and post datasets on match_ID

dat <- merge(pre, post, by = "match_ID", all = T)
names(dat)

