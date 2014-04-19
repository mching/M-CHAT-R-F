# Preprocessing script for the pre questionnaire

#############################
# Keep only the variables that start with a lower case q
#############################
pre <- as.data.frame(pre)

# Create a vector of variable names
variable.names <- names(pre) 

# Create a vector of the positions where the first letter is lowercase q
lower.q.questions <- grep("^q", variable.names) 
rm(variable.names)

# Filter dataset by these variables
pre.filtered <- pre[c(2, lower.q.questions)]
pre.unfiltered <- pre
pre <- pre.filtered
rm(pre.filtered)
rm(lower.q.questions)

##########################
# q0001_0001, 0002, 0003 : convert to variables on how an individual is involved
# in the general pediatrics clinic
#
# Output variables = care_direct, care_supervise, care_no_direct_or_supervise
#
##########################
pre$care_direct <- as.numeric(pre$q0001_0001)
pre$care_supervise <- as.numeric(pre$q0001_0002)
pre$care_no_direct_or_supervise <- as.numeric(pre$q0001_0003)

table(direct = pre$care_direct, supervise = pre$care_supervise, no.direct.or.supervise = pre$care_no_direct_or_supervise, useNA="if") 
# no one failed to respond to this question

pre$care_direct[is.na(pre$care_direct)] <- 0
pre$care_supervise[is.na(pre$care_supervise)] <- 0
pre$care_no_direct_or_supervise[is.na(pre$care_no_direct_or_supervise)] <- 0

# replace as factor variables
pre$care_direct <- as.factor(pre$care_direct)
pre$care_supervise <- as.factor(pre$care_supervise)
pre$care_no_direct_or_supervise <- as.factor(pre$care_no_direct_or_supervise)

# If no direct care or supervision, make their responses on q2-10 into NA
a <- pre$care_no_direct_or_supervise == 1
names(pre)
pre[a, 5:26] <- NA
rm(a)

# Correct q0006. Correct answers were selected manually because of the judgment involved
# in determining if the respondent was correct
pre$q0006_correct <- rep(0, length(pre$q0006))
correct_respondents <- c(3, 4, 7, 9, 16, 18, 30, 32, 38)  # need to check >= vs >
pre$q0006_correct[correct_respondents] <- 1
pre$q0006_correct[pre$q0006 == pre$q0006[12]] <- NA # Make NAs for not answered in q0006
pre$q0006_correct <- factor(pre$q0006_correct)
rm(correct_respondents)

# Make matching codes all uppercase to facilitate matching
pre$matchID <- toupper(pre$q0011)

# Trim leading and trailing white space
pre$matchID <- gsub("^\\s+|\\s+$", "", pre$matchID)

# Make "" into randoms
pre$matchID[pre$matchID == ""] <- runif(runif(table(pre$matchID == "")[2]))

# Put prefix of pre_ on all variables except matchID
names(pre)[-38] <- paste0("pre_", names(pre)[-38])
# head(pre)