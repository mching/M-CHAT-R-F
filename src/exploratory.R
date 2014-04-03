# Exploratory analyses

# q0001: Type of general pediatrics care provided by respondent
summary(pre$care_direct)
summary(pre$care_supervise)
summary(pre$care_no_direct_or_supervise)
table(direct = pre$care_direct, supervise = pre$care_supervise, no.direct.or.supervise = pre$care_no_direct_or_supervise, useNA="if") 

# q0002: I am comfortable with administering and scoring the Modified Checklist
# for Autism in Toddlers (M-CHAT).
summary(pre$q0002_0001)
mean(as.numeric(pre$q0002_0001), na.rm = T)
median(as.numeric(pre$q0002_0001), na.rm = T)

# q0003: How often do you feel the M-CHAT adds to your overall clinical
# impression of the risk for autism?
summary(pre$q0003_0001)
mean(as.numeric(pre$q0003_0001), na.rm = T)
median(as.numeric(pre$q0003_0001), na.rm = T)

# q0004: During a well child visit, when do you most often score the M-CHAT?
summary(pre$q0004_0001)

# q0005: What do you do most often if a parent has not completed the M-CHAT at an 18 or 24 month well child visit?
summary(pre$q0005)
# several say to do partial screening

# q0006: corrected answers version is called pre$q0006_correct
summary(pre$q0006_correct)
mean(as.numeric(pre$q0006_correct), na.rm = T) - 1
# only a third got the right answer

# q0007: Before today, were you aware of the M-CHAT Follow-Up Interview?
summary(pre$q0007_0001)

# q0008: When a patient has failed their M-CHAT, how often do you administer the
# M-CHAT Follow-Up Interview?
summary(pre$q0008_0001)

# q0009: What barriers do you see that make it difficult to administer the M-CHAT?
# _0001: Lack of knowledge on how to score the M-CHAT
summary(pre$q0009_0001)
# _0002: Unable to find scoring sheet
summary(pre$q0009_0002)
# _0003: It takes too much time
summary(pre$q0009_0003)

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
