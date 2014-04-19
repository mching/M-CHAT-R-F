# Preprocessing script for post questionnaire
# Note: Corrected data is for 30 responses only. If more are received, you need
# to manually correct additional entries

# Make list into data frame
post <- as.data.frame(post)

#############################
# Keep only the variables that start with a lower case q
#############################

# Create a vector of variable names
variable.names <- names(post) 

# Create a vector of the positions where the first letter is lowercase q
lower.q.questions <- grep("^q", variable.names) 
rm(variable.names)

# Filter dataset by these variables
post.filtered <- post[c(2, lower.q.questions)]
post.unfiltered <- post
post <- post.filtered
rm(post.filtered)
rm(lower.q.questions)


##########################
# q0001_0001, 0002, 0003 : convert to variables on how an individual is involved
# in the general pediatrics clinic
#
# Output variables = care_direct, care_supervise, care_no_direct_or_supervise
#
##########################
post$care_direct <- as.numeric(post$q0001_0001)
post$care_supervise <- as.numeric(post$q0001_0002)
post$care_no_direct_or_supervise <- as.numeric(post$q0001_0003)

post$care_direct[is.na(post$care_direct)] <- 0
post$care_supervise[is.na(post$care_supervise)] <- 0
post$care_no_direct_or_supervise[is.na(post$care_no_direct_or_supervise)] <- 0

table(direct = post$care_direct, supervise = post$care_supervise, no.direct.or.supervise = post$care_no_direct_or_supervise, useNA="if") 

# replace as factor variables
post$care_direct <- as.factor(post$care_direct)
post$care_supervise <- as.factor(post$care_supervise)
post$care_no_direct_or_supervise <- as.factor(post$care_no_direct_or_supervise)

# If no direct care or supervision, make their responses on q2-13 into NA
a <- post$care_no_direct_or_supervise == 1
post[a, 6:30] <- NA
rm(a)

# One person failed to respond to this question, respondent 16
# post[40] == 0 & post[41] == 0 & post[42] == 0
# If you add more data, you need to check respondent 16 is still same #
# make respondent 16 into NA on all roles (ok through 30 responses)
post[16, 40:42] <- NA

# Correct q0007. Correct answers were selected manually because of the judgment involved
# in determining if the respondent was correct
post$q0007_correct <- rep(0, length(post$q0007))
post$q0007
correct_respondents <- c(6, 9, 10, 13, 16, 21, 23)
post$q0007_correct[correct_respondents] <- 1
post$q0007_correct[post$q0007 == post$q0007[12]] <- NA # Make NAs for not answered in q0007

# CONTINUE EDITING HERE
post$q0007_correct <- factor(post$q0007_correct)
rm(correct_respondents)

# Make matching codes all uppercase to facilitate matching
post$q0014 <- toupper(post$q0014)

# Trim leading and trailing white space
post$q0014 <- gsub("^\\s+|\\s+$", "", post$q0014)

# Make "" into NAs
post$q0014[post$q0014 == ""] <- NA

# Rename q0014 to match_ID
post$match_ID <- post$q0014
