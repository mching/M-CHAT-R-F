# Exploratory analyses

# number in pre and post
length(pre[,1])
length(post[,1])
length(dat[,1])

# q0001: Type of general pediatrics care provided by respondent
summary(dat$pre_care_direct)
table(dat$pre_care_direct, dat$post_care_direct, useNA="if")
summary(dat$pre_care_supervise)
summary(dat$pre_care_no_direct_or_supervise)
table(direct = pre$care_direct, supervise = pre$care_supervise, no.direct.or.supervise = pre$care_no_direct_or_supervise, useNA="if") 

table(dat$care_role)


# q0002: I am comfortable with administering and scoring the Modified Checklist
# for Autism in Toddlers (M-CHAT).

par(mfrow = c(1,2))
plot(dat$pre_q0002_0001, main = "pre comfort")
plot(dat$post_q0002_0001, main = "post comfort")
par(mfrow = c(1,1))

summary(dat$pre_q0002_0001)
table0002pre <- table(dat$pre_q0002_0001 == "Agree" | dat$pre_q0002_0001 == "Strongly Agree") # number of agree or strongly agree
binom.confint(table0002pre[2], n = sum(table0002pre), method = "exact")

summary(dat$post_q0002_0001)
table0002post <- table(dat$post_q0002_0001 == "Agree" | dat$post_q0002_0001 == "Strongly Agree") # number of agree or strongly agree
binom.confint(table0002post[2], n = sum(table0002post), method = "exact")

mean(as.numeric(dat$pre_q0002_0001), na.rm = T)
sd(as.numeric(dat$pre_q0002_0001), na.rm = T)

mean(as.numeric(dat$pre_q0002_0001), na.rm = T)
sd(as.numeric(dat$pre_q0002_0001), na.rm = T)

t.test(as.numeric(dat$pre_q0002_0001), as.numeric(dat$post_q0002_0001), na.rm = T) # t-test on likert scale values
fisher.test(matrix(c(table0002pre, table0002post), nrow = 2)) # Fisher test for ratios

# pre_q0003, post_q0004: How often do you feel the M-CHAT adds to your overall clinical
# impression of the risk for autism?
par(mfrow = c(1,2))
plot(dat$pre_q0003_0001, main = "pre added value")
plot(dat$post_q0004_0001, main = "post added value")
par(mfrow = c(1,1))

summary(dat$pre_q0003_0001)
table0003pre <- table(dat$pre_q0003_0001 == "Most of the Time" | dat$pre_q0002_0001 == "Always") # Most/Always
binom.confint(table0003pre[2], n = sum(table0003pre), method = "exact")

summary(dat$post_q0004_0001)
table0004post <- table(dat$post_q0004_0001 == "Most of the Time" | dat$post_q0004_0001 == "Always") # Most/Always
binom.confint(table0004post[2], n = sum(table0004post), method = "exact")

summary(dat$pre_q0003_0001)
mean(as.numeric(dat$pre_q0003_0001), na.rm = T)
sd(as.numeric(dat$pre_q0003_0001), na.rm = T)

mean(as.numeric(dat$post_q0004_0001), na.rm = T)
sd(as.numeric(dat$post_q0004_0001), na.rm = T)

t.test(as.numeric(dat$pre_q0003_0001), as.numeric(dat$post_q0004_0001), na.rm = T) # t-test on likert scale values
fisher.test(matrix(c(table0003pre, table0004post), nrow = 2)) # Fisher test for ratios

# q0004: During a well child visit, when do you most often score the M-CHAT?
summary(pre$q0004_0001)

# q0005: What do you do most often if a parent has not completed the M-CHAT at an 18 or 24 month well child visit?
summary(pre$q0005)
# several say to do partial screening

# pre_q0006, post_q0007: corrected answers version is called pre$q0006_correct and post$q0007_correct
table0006pre <- table(dat$pre_q0006_correct)
mean(as.numeric(dat$pre_q0006_correct), na.rm = T) - 1
binom.confint(table0006pre[2], n = sum(table0006pre), method = "exact")

table0007post <- table(dat$post_q0007_correct)
mean(as.numeric(dat$post_q0007_correct), na.rm = T) - 1
binom.confint(table0007post[2], n = sum(table0007post), method = "exact")

fisher.test(matrix(c(table0006pre, table0007post), nrow = 2)) # Fisher test for ratios

# pre_q0007, post_q0009: Before today, were you aware of the M-CHAT Follow-Up Interview?
par(mfrow = c(1,2))
plot(dat$pre_q0007_0001, main = "pre f/u aware")
plot(dat$post_q0009_0001, main = "post f/u aware")
par(mfrow = c(1,1))

summary(dat$pre_q0007_0001)
table0007pre <- table(dat$pre_q0007_0001 == "Agree" | dat$pre_q0007_0001 == "Strongly Agree") # number of agree or strongly agree
binom.confint(table0007pre[2], n = sum(table0007pre), method = "exact")

summary(dat$post_q0009_0001)
table0009post <- table(dat$post_q0009_0001 == "Agree" | dat$post_q0009_0001 == "Strongly Agree") # number of agree or strongly agree
binom.confint(table0009post[2], n = sum(table0009post), method = "exact")

mean(as.numeric(dat$pre_q0007_0001), na.rm = T)
sd(as.numeric(dat$pre_q0007_0001), na.rm = T)

mean(as.numeric(dat$post_q0009_0001), na.rm = T)
sd(as.numeric(dat$post_q0009_0001), na.rm = T)

t.test(as.numeric(dat$pre_q0007_0001), as.numeric(dat$post_q0009_0001), na.rm = T) # t-test on likert scale values
fisher.test(matrix(c(table0007pre, table0009post), nrow = 2)) # Fisher test for ratios


# q0008: When a patient has failed their M-CHAT, how often do you administer the
# M-CHAT Follow-Up Interview?
summary(pre$q0008_0001)

# q0009: What barriers do you see that make it difficult to administer the M-CHAT?
# _0001: Lack of knowledge on how to score the M-CHAT
summary(dat$pre_q0009_0001)
plot(dat$pre_q0009_0001)

# _0002: Unable to find scoring sheet
summary(dat$pre_q0009_0002)

# _0003: It takes too much time
summary(dat$pre_q0009_0003)

# q0010: What barriers do you see that make it difficult to administer the
# M-CHAT Follow-Up Interview?
# _0001: Not aware there was a follow-up interview  
summary(pre$q0010_0001)

# _0002: Not aware of how to do the follow-up interview	
summary(pre$q0010_0002)

# _0003: Not aware of where to get the follow-up interview questions	
summary(pre$q0010_0003)

# _0004: It takes too long
summary(pre$q0010_0004)



# Demographics breakdown
# Type of doctor (attending, specialist attending, resident)
table(dat$combo_attending_resident)
prop.table(table(dat$combo_attending_resident))

# how many residents
sum(table(dat$combo_attending_resident)[3:5])
sum(table(dat$combo_attending_resident)[3:5])/sum(table(dat$combo_attending_resident))

plot(dat$combo_attending_resident, main = "doctor type")

# Panels of ASD patients
plot(dat$combo_n_ASD_pts, main = "number of ASD patients per doctor")
table(dat$combo_n_ASD_pts)
prop.table(table(dat$combo_n_ASD_pts))