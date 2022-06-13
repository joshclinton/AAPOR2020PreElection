#################################################################################
#   Analyzing State Primary Polls to Create Primary Poll Datasets
#   Josh Clinton, Vanderbilt University
#   Sept 28, 2020
# Edited May 22,2020 by Mellissa Meisels
# Edited Sept 28, 2020 by JDC to save dataframes as state postal codes
#################################################################################

setwd("/Users/Mellissa/Dropbox/AAPOR 2020 Election Polling/")
#setwd("/Users/clintojd/Dropbox (Personal)/AAPOR 2020 Election Polling")

library(readxl)
library(tidyverse)

######################################################################################
##  ALABAMA
######################################################################################

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "AL")     # Change for each state
# Calculate Margin of Error based on sample size if missing
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96
# Replace with samplesize based calculation if missing
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

## Create days to election
##  Format depends on how read in (%Y is 4 digit year)

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/3/2020", "%m/%d/%Y")       # Change for each state

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

# Only keep those done in last 14 days for official AAPOR report
dat <- dat[dat$DaysToElection > -14,]

# Some polls in 2018 not include Buttigieg/Klobuchar so because there were "NA" that were read in as characters need to make numeric
dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)

######################################################################################
# Measures of Poll Performance
######################################################################################

# Traditional AAPOR Measures
dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

# Works - ties are "correct"  -- Change for each state
dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]))){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]))){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

###########################################
# Now Look at Candidate specific error
###########################################

# Vote - Poll: > 0 means underpredicted by poll
dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

AL <- dat
saveRDS(AL, file="Rdata/ALpolls14.RDS")
rm(dat)

######################################################################################
##  ARKANSAS
######################################################################################

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "AR")     # Change for each state
# Calculate Margin of Error based on sample size if missing
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

# Replace with samplesize based calculation if missing
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

## Create days to election
##  Format depends on how read in (%Y is 4 digit year)

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/3/2020", "%m/%d/%Y")      # Change for each state

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

# Only keep those done in last 14 days for official AAPOR report
dat <- dat[dat$DaysToElection > -14,]

# Some polls in 2018 not include Buttigieg/Klobuchar so because there were "NA" that were read in as characters need to make numeric
dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)

######################################################################################
# Measures of Poll Performance
######################################################################################

# Traditional AAPOR Measures
dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

# Works - ties are "correct"  -- Change for each state
dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]))){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]))){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

###########################################
# Now Look at Candidate specific error
###########################################

# Vote - Poll: > 0 means underpredicted by poll
dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

AR <- dat
saveRDS(AR, file="Rdata/ARpolls14.RDS")
rm(dat)

######################################################################################
##  CALIFORNIA
######################################################################################

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "CA")     # Change for each state
# Calculate Margin of Error based on sample size if missing
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

# Replace with samplesize based calculation if missing
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

## Create days to election
##  Format depends on how read in (%Y is 4 digit year)

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/3/2020", "%m/%d/%Y")     # Change for each state

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

# Only keep those done in last 14 days for official AAPOR report
dat <- dat[dat$DaysToElection > -14,]

# Some polls in 2018 not include Buttigieg/Klobuchar so because there were "NA" that were read in as characters need to make numeric
# Mellissa: Bloomberg also needs to be made numeric
dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

######################################################################################
# Measures of Poll Performance
######################################################################################

# Traditional AAPOR Measures
dat$poll.vote.margin <- (dat$Sanders-dat$Biden) - (dat$sanders.vote - dat$biden.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

# Works - ties are "correct"  -- Change for each state
dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Sanders[i] >= max(c(dat$Buttigieg[i],dat$Biden[i],dat$Warren[i]))){dat$correct.pred[i]<-1}
  if(dat$Sanders[i] < max(c(dat$Buttigieg[i],dat$Biden[i],dat$Warren[i]))){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

###########################################
# Now Look at Candidate specific error
###########################################

# Vote - Poll: > 0 means underpredicted by poll
dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

CA <- dat

saveRDS(CA, file="Rdata/CApolls14.RDS")
rm(dat)

######################################################################################
##  COLORADO
######################################################################################

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "CO")     # Change for each state
# Calculate Margin of Error based on sample size if missing
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

# Replace with samplesize based calculation if missing
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

## Create days to election
##  Format depends on how read in (%Y is 4 digit year)

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/3/2020", "%m/%d/%Y")     # Change for each state

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

# Only keep those done in last 14 days for official AAPOR report
dat <- dat[dat$DaysToElection > -14,]

# Some polls in 2018 not include Buttigieg/Klobuchar so because there were "NA" that were read in as characters need to make numeric
# Mellissa: Bloomberg also needs to be made numeric
dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

######################################################################################
# Measures of Poll Performance
######################################################################################

# Traditional AAPOR Measures
dat$poll.vote.margin <- (dat$Sanders-dat$Biden) - (dat$sanders.vote - dat$biden.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

# Works - ties are "correct"  -- Change for each state
dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Sanders[i] >= max(c(dat$Bloomberg[i],dat$Biden[i],dat$Warren[i]))){dat$correct.pred[i]<-1}
  if(dat$Sanders[i] < max(c(dat$Bloomberg[i],dat$Biden[i],dat$Warren[i]))){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

###########################################
# Now Look at Candidate specific error
###########################################

# Vote - Poll: > 0 means underpredicted by poll
dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

CO <- dat
saveRDS(CO, file="Rdata/COpolls14.RDS")
rm(dat)

######################################################################################
##  IOWA
######################################################################################

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "IA")     # Change for each state
# Calculate Margin of Error based on sample size if missing
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

# Replace with samplesize based calculation if missing
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

## Create days to election
##  Format depends on how read in (%Y is 4 digit year)

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("02/03/2020", "%m/%d/%Y")       # Change for each state

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

# Only keep those done in last 14 days for official AAPOR report
dat <- dat[dat$DaysToElection > -14,]

# Some polls in 2018 not include Buttigieg/Klobuchar so because there were "NA" that were read in as characters need to make numeric
dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)

######################################################################################
# Measures of Poll Performance
######################################################################################

# Traditional AAPOR Measures
dat$poll.vote.margin <- (dat$Sanders - dat$Buttigieg) - (dat$sanders.vote - dat$buttigieg.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

# Works - ties are "correct"  -- Change for each state
dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Sanders[i] >= max(c(dat$Buttigieg[i],dat$Biden[i],dat$Warren[i]))){dat$correct.pred[i]<-1}
  if(dat$Sanders[i] < max(c(dat$Buttigieg[i],dat$Biden[i],dat$Warren[i]))){dat$correct.pred[i]<-0}
}
mean(dat$correct.pred)

###########################################
# Now Look at Candidate specific error
###########################################

# Vote - Poll: > 0 means underpredicted by poll
dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Buttigieg.error <- dat$buttigieg.vote - dat$Buttigieg
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren 
dat$Klobuchar.error <- dat$klobuchar.vote - dat$Klobuchar

IA <- dat

saveRDS(IA, file="Rdata/IApolls14.RDS")
rm(dat)

######################################################################################
##  IDAHO
######################################################################################

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "ID")     # Change for each state
# Calculate Margin of Error based on sample size if missing
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

# Replace with samplesize based calculation if missing
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

## Create days to election
##  Format depends on how read in (%Y is 4 digit year)

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/10/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

# Only keep those done in last 14 days for official AAPOR report
dat <- dat[dat$DaysToElection > -14,]

# Some polls in 2018 not include Buttigieg/Klobuchar so because there were "NA" that were read in as characters need to make numeric
#  Bloomberg also needs to be made numeric
dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

######################################################################################
# Measures of Poll Performance
######################################################################################

# Traditional AAPOR Measures
dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

# Works - ties are "correct"  -- Change for each state
dat$correct.pred <- NA

# excluding Warren & Bloomberg because respondents were not asked about them
for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= dat$Sanders[i]){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < dat$Sanders[i]){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

###########################################
# Now Look at Candidate specific error
###########################################

# Vote - Poll: > 0 means underpredicted by poll
dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden

ID <- dat

saveRDS(ID, file="Rdata/IDpolls14.RDS")
rm(dat)

######################################################################################
##  MASSACHUSSETTS
######################################################################################

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "MA")     # Change for each state
# Calculate Margin of Error based on sample size if missing
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

# Replace with samplesize based calculation if missing
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

## Create days to election
##  Format depends on how read in (%Y is 4 digit year)

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/3/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

# Only keep those done in last 14 days for official AAPOR report
dat <- dat[dat$DaysToElection > -14,]

# Some polls in 2018 not include Buttigieg/Klobuchar so because there were "NA" that were read in as characters need to make numeric
#  Bloomberg also needs to be made numeric
dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

######################################################################################
# Measures of Poll Performance
######################################################################################

# Traditional AAPOR Measures
dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

# Works - ties are "correct"  -- Change for each state
dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]))){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]))){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

###########################################
# Now Look at Candidate specific error
###########################################

# Vote - Poll: > 0 means underpredicted by poll
dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

MA <- dat

saveRDS(MA, file="Rdata/MApolls14.RDS")
rm(dat)

######################################################################################
##  MAINE
######################################################################################

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "ME")     # Change for each state
# Calculate Margin of Error based on sample size if missing
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

# Replace with samplesize based calculation if missing
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

## Create days to election
##  Format depends on how read in (%Y is 4 digit year)

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/3/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

# Only keep those done in last 14 days for official AAPOR report
dat <- dat[dat$DaysToElection > -14,]

# Some polls in 2018 not include Buttigieg/Klobuchar so because there were "NA" that were read in as characters need to make numeric
#  Bloomberg also needs to be made numeric
dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

######################################################################################
# Measures of Poll Performance
######################################################################################

# Traditional AAPOR Measures
dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

# Works - ties are "correct"  -- Change for each state
dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]))){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]))){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

###########################################
# Now Look at Candidate specific error
###########################################

# Vote - Poll: > 0 means underpredicted by poll
dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

ME <- dat

saveRDS(ME, file="Rdata/MEpolls14.RDS")
rm(dat)

######################################################################################
##  MICHIGAN
######################################################################################

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "MI")     # Change for each state
# Calculate Margin of Error based on sample size if missing
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

# Replace with samplesize based calculation if missing
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

## Create days to election
##  Format depends on how read in (%Y is 4 digit year)

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/10/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

# Only keep those done in last 14 days for official AAPOR report
dat <- dat[dat$DaysToElection > -14,]

# Some polls in 2018 not include Buttigieg/Klobuchar so because there were "NA" that were read in as characters need to make numeric
#  Bloomberg also needs to be made numeric
#  Warren was "not asked" in some polls
dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

######################################################################################
# Measures of Poll Performance
######################################################################################

# Traditional AAPOR Measures
dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

# Works - ties are "correct"  -- Change for each state
dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=TRUE)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=TRUE)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

###########################################
# Now Look at Candidate specific error
###########################################

# Vote - Poll: > 0 means underpredicted by poll
dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

MI <- dat

saveRDS(MI, file="Rdata/MIpolls14.RDS")
rm(dat)

######################################################################################
##  MINNESOTA
######################################################################################

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "MN")     # Change for each state
# Calculate Margin of Error based on sample size if missing
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

# Replace with samplesize based calculation if missing
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

## Create days to election
##  Format depends on how read in (%Y is 4 digit year)

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/3/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

# Only keep those done in last 14 days for official AAPOR report
dat <- dat[dat$DaysToElection > -14,]

# Some polls in 2018 not include Buttigieg/Klobuchar so because there were "NA" that were read in as characters need to make numeric
#  Bloomberg also needs to be made numeric
#  Warren was "not asked" in some polls
dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

######################################################################################
# Measures of Poll Performance
######################################################################################

# Traditional AAPOR Measures
dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

# Works - ties are "correct"  -- Change for each state
dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=TRUE)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=TRUE)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

###########################################
# Now Look at Candidate specific error
###########################################

# Vote - Poll: > 0 means underpredicted by poll
dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

MN <- dat

saveRDS(MN, file="Rdata/MNpolls14.RDS")
rm(dat)

######################################################################################
##  MISSOURI
######################################################################################

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "MO")     # Change for each state
# Calculate Margin of Error based on sample size if missing
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

# Replace with samplesize based calculation if missing
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

## Create days to election
##  Format depends on how read in (%Y is 4 digit year)

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/10/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

# Only keep those done in last 14 days for official AAPOR report
dat <- dat[dat$DaysToElection > -14,]

# Some polls in 2018 not include Buttigieg/Klobuchar so because there were "NA" that were read in as characters need to make numeric
#  Bloomberg also needs to be made numeric
#  Warren was "not asked" in some polls
dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

######################################################################################
# Measures of Poll Performance
######################################################################################

# Traditional AAPOR Measures
dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

# Works - ties are "correct"  -- Change for each state
dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=TRUE)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=TRUE)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

###########################################
# Now Look at Candidate specific error
###########################################

# Vote - Poll: > 0 means underpredicted by poll
dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

MO <- dat

saveRDS(MO, file="Rdata/MOpolls14.RDS")
rm(dat)

######################################################################################
##  MISSISSIPPI
######################################################################################

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "MS")     # Change for each state
# Calculate Margin of Error based on sample size if missing
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

# Replace with samplesize based calculation if missing
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

## Create days to election
##  Format depends on how read in (%Y is 4 digit year)

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/10/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

# Only keep those done in last 14 days for official AAPOR report
dat <- dat[dat$DaysToElection > -14,]

# Some polls in 2018 not include Buttigieg/Klobuchar so because there were "NA" that were read in as characters need to make numeric
#  Bloomberg also needs to be made numeric
#  Warren was "not asked" in some polls
dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

######################################################################################
# Measures of Poll Performance
######################################################################################

# Traditional AAPOR Measures
dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

# Works - ties are "correct"  -- Change for each state
dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=TRUE)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=TRUE)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

###########################################
# Now Look at Candidate specific error
###########################################

# Vote - Poll: > 0 means underpredicted by poll
dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

MS <- dat

saveRDS(MS, file="Rdata/MSpolls14.RDS")
rm(dat)

######################################################################################
##  NORTH CAROLINA
######################################################################################

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "NC")     # Change for each state
# Calculate Margin of Error based on sample size if missing
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

# Replace with samplesize based calculation if missing
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

## Create days to election
##  Format depends on how read in (%Y is 4 digit year)

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/3/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

# Only keep those done in last 14 days for official AAPOR report
dat <- dat[dat$DaysToElection > -14,]

# Some polls in 2018 not include Buttigieg/Klobuchar so because there were "NA" that were read in as characters need to make numeric
#  Bloomberg also needs to be made numeric
#  Warren was "not asked" in some polls
dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

######################################################################################
# Measures of Poll Performance
######################################################################################

# Traditional AAPOR Measures
dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

# Works - ties are "correct"  -- Change for each state
dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=TRUE)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=TRUE)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

###########################################
# Now Look at Candidate specific error
###########################################

# Vote - Poll: > 0 means underpredicted by poll
dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

NC <- dat

saveRDS(NC, file="Rdata/NCpolls14.RDS")
rm(dat)

######################################################################################
##  NORTH DAKOTA
######################################################################################

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "ND")     # Change for each state
# Calculate Margin of Error based on sample size if missing
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

# Replace with samplesize based calculation if missing
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

## Create days to election
##  Format depends on how read in (%Y is 4 digit year)

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/10/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

# Only keep those done in last 14 days for official AAPOR report
dat <- dat[dat$DaysToElection > -14,]

# Some polls in 2018 not include Buttigieg/Klobuchar so because there were "NA" that were read in as characters need to make numeric
#  Bloomberg also needs to be made numeric
#  Warren was "not asked" in some polls
dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

######################################################################################
# Measures of Poll Performance
######################################################################################

# Traditional AAPOR Measures
dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

# Works - ties are "correct"  -- Change for each state
dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=TRUE)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=TRUE)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

###########################################
# Now Look at Candidate specific error
###########################################

# Vote - Poll: > 0 means underpredicted by poll
dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

ND <- dat

saveRDS(ND, file="Rdata/NDpolls14.RDS")
rm(dat)

######################################################################################
##  New Hampshire
######################################################################################

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "NH")   # Change for each state

# Calculate Margin of Error based on sample size if missing
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

# Replace with samplesize based calculation if missing
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

## Create days to election
##  Format depends on how read in (%Y is 4 digit year)

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("02/11/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

# Only keep those done in last 14 days for official AAPOR report
dat <- dat[dat$DaysToElection > -14,]

# Some polls in 2018 not include Buttigieg/Klobuchar so because there were "NA" that were read in as characters need to make numeric
dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)

######################################################################################
# Measures of Poll Performance
######################################################################################

# Traditional AAPOR Measures
dat$poll.vote.margin <- (dat$Sanders - dat$Buttigieg) - (dat$sanders.vote - dat$buttigieg.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

# Works - ties are "correct"
dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Sanders[i] >= max(c(dat$Buttigieg[i],dat$Biden[i],dat$Warren[i]))){dat$correct.pred[i]<-1}
  if(dat$Sanders[i] < max(c(dat$Buttigieg[i],dat$Biden[i],dat$Warren[i]))){dat$correct.pred[i]<-0}
}
mean(dat$correct.pred)

###########################################
# Now Look at Candidate specific error
###########################################

# Vote - Poll: > 0 means underpredicted by poll
dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Buttigieg.error <- dat$buttigieg.vote - dat$Buttigieg
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren 
dat$Klobuchar.error <- dat$klobuchar.vote - dat$Klobuchar

NH <- dat

saveRDS(NH, file="Rdata/NHpolls14.RDS")
rm(dat)

######################################################################################
##  Nevada
######################################################################################

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "NV")   # Change for each state

# Calculate Margin of Error based on sample size if missing
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

# Replace with samplesize based calculation if missing
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

## Create days to election
##  Format depends on how read in (%Y is 4 digit year)

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("02/22/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

# Only keep those done in last 14 days for official AAPOR report
dat <- dat[dat$DaysToElection > -14,]

# Some polls in 2018 not include Buttigieg/Klobuchar so because there were "NA" that were read in as characters need to make numeric
dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)

######################################################################################
# Measures of Poll Performance
######################################################################################

# Traditional AAPOR Measures
dat$poll.vote.margin <- (dat$Sanders - dat$Buttigieg) - (dat$sanders.vote - dat$buttigieg.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

# Works - ties are "correct"
dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Sanders[i] >= max(c(dat$Buttigieg[i],dat$Biden[i],dat$Warren[i]))){dat$correct.pred[i]<-1}
  if(dat$Sanders[i] < max(c(dat$Buttigieg[i],dat$Biden[i],dat$Warren[i]))){dat$correct.pred[i]<-0}
}
mean(dat$correct.pred)

###########################################
# Now Look at Candidate specific error
###########################################

# Vote - Poll: > 0 means underpredicted by poll
dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Buttigieg.error <- dat$buttigieg.vote - dat$Buttigieg
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren 
dat$Klobuchar.error <- dat$klobuchar.vote - dat$Klobuchar

NV <- dat

saveRDS(NV, file="Rdata/NVpolls14.RDS")
rm(dat)

######################################################################################
##  OKLAHOMA
######################################################################################

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "OK")     # Change for each state
# Calculate Margin of Error based on sample size if missing
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

# Replace with samplesize based calculation if missing
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

## Create days to election
##  Format depends on how read in (%Y is 4 digit year)

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/3/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

# Only keep those done in last 14 days for official AAPOR report
dat <- dat[dat$DaysToElection > -14,]

# Some polls in 2018 not include Buttigieg/Klobuchar so because there were "NA" that were read in as characters need to make numeric
#  Bloomberg also needs to be made numeric
#  Warren was "not asked" in some polls
dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

######################################################################################
# Measures of Poll Performance
######################################################################################

# Traditional AAPOR Measures
dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

# Works - ties are "correct"  -- Change for each state
dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=TRUE)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=TRUE)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

###########################################
# Now Look at Candidate specific error
###########################################

# Vote - Poll: > 0 means underpredicted by poll
dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

OK <- dat

saveRDS(OK, file="Rdata/OKpolls14.RDS")
rm(dat)

######################################################################################
##  SOUTH CAROLINA
######################################################################################

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "SC")     # Change for each state
# Calculate Margin of Error based on sample size if missing
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

# Replace with samplesize based calculation if missing
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

## Create days to election
##  Format depends on how read in (%Y is 4 digit year)

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("02/29/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

# Only keep those done in last 14 days for official AAPOR report
dat <- dat[dat$DaysToElection > -14,]

# Some polls in 2018 not include Buttigieg/Klobuchar so because there were "NA" that were read in as characters need to make numeric
#  Bloomberg also needs to be made numeric
#  Warren was "not asked" in some polls
dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

######################################################################################
# Measures of Poll Performance
######################################################################################

# Traditional AAPOR Measures
dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

# Works - ties are "correct"  -- Change for each state
dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Buttigieg[i],dat$Sanders[i],dat$Warren[i]))){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Buttigieg[i],dat$Sanders[i],dat$Warren[i]))){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

###########################################
# Now Look at Candidate specific error
###########################################

# Vote - Poll: > 0 means underpredicted by poll
dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Buttigieg.error <- Buttigieg.vote - dat$Buttigieg
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Klobuchar.error <- Klobuchar.vote - dat$Klobuchar

SC <- dat

saveRDS(SC, file="Rdata/SCpolls14.RDS")
rm(dat)

######################################################################################
##  TENNESSEE
######################################################################################

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "TN")     # Change for each state
# Calculate Margin of Error based on sample size if missing
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

# Replace with samplesize based calculation if missing
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

## Create days to election
##  Format depends on how read in (%Y is 4 digit year)

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/3/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

# Only keep those done in last 14 days for official AAPOR report
dat <- dat[dat$DaysToElection > -14,]

# Some polls in 2018 not include Buttigieg/Klobuchar so because there were "NA" that were read in as characters need to make numeric
#  Bloomberg also needs to be made numeric
#  Warren was "not asked" in some polls
dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

######################################################################################
# Measures of Poll Performance
######################################################################################

# Traditional AAPOR Measures
dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

# Works - ties are "correct"  -- Change for each state
dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

###########################################
# Now Look at Candidate specific error
###########################################

# Vote - Poll: > 0 means underpredicted by poll
dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

TN <- dat
saveRDS(TN, file="Rdata/TNpolls14.RDS")
rm(dat)


######################################################################################
##  TEXAS
######################################################################################

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "TX")     # Change for each state
# Calculate Margin of Error based on sample size if missing
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

# Replace with samplesize based calculation if missing
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

## Create days to election
##  Format depends on how read in (%Y is 4 digit year)

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/3/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

# Only keep those done in last 14 days for official AAPOR report
dat <- dat[dat$DaysToElection > -14,]

# Some polls in 2018 not include Buttigieg/Klobuchar so because there were "NA" that were read in as characters need to make numeric
#  Bloomberg also needs to be made numeric
#  Warren was "not asked" in some polls
dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

######################################################################################
# Measures of Poll Performance
######################################################################################

# Traditional AAPOR Measures
dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

# Works - ties are "correct"  -- Change for each state
dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

###########################################
# Now Look at Candidate specific error
###########################################

# Vote - Poll: > 0 means underpredicted by poll
dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

TX <- dat

saveRDS(TX, file="Rdata/TXpolls14.RDS")
rm(dat)


######################################################################################
##  UTAH
######################################################################################

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "UT")     # Change for each state
# Calculate Margin of Error based on sample size if missing
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

# Replace with samplesize based calculation if missing
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

## Create days to election
##  Format depends on how read in (%Y is 4 digit year)

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/3/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

# Only keep those done in last 14 days for official AAPOR report
dat <- dat[dat$DaysToElection > -14,]

# Some polls in 2018 not include Buttigieg/Klobuchar so because there were "NA" that were read in as characters need to make numeric
#  Bloomberg also needs to be made numeric
#  Warren was "not asked" in some polls
dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

######################################################################################
# Measures of Poll Performance
######################################################################################

# Traditional AAPOR Measures
dat$poll.vote.margin <- (dat$Sanders-dat$Biden) - (dat$sanders.vote - dat$biden.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

# Works - ties are "correct"  -- Change for each state
dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Sanders[i] >= max(c(dat$Buttigieg[i],dat$Biden[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-1}
  if(dat$Sanders[i] < max(c(dat$Buttigieg[i],dat$Biden[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

###########################################
# Now Look at Candidate specific error
###########################################

# Vote - Poll: > 0 means underpredicted by poll
dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

UT <- dat

saveRDS(UT, file="Rdata/UTpolls14.RDS")
rm(dat)

######################################################################################
##  VIRGINA
######################################################################################

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "VA")     # Change for each state
# Calculate Margin of Error based on sample size if missing
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

# Replace with samplesize based calculation if missing
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

## Create days to election
##  Format depends on how read in (%Y is 4 digit year)

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/3/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

# Only keep those done in last 14 days for official AAPOR report
dat <- dat[dat$DaysToElection > -14,]

# Some polls in 2018 not include Buttigieg/Klobuchar so because there were "NA" that were read in as characters need to make numeric
#  Bloomberg also needs to be made numeric
#  Warren was "not asked" in some polls
dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

######################################################################################
# Measures of Poll Performance
######################################################################################

# Traditional AAPOR Measures
dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

# Works - ties are "correct"  -- Change for each state
dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Buttigieg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Buttigieg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

###########################################
# Now Look at Candidate specific error
###########################################

# Vote - Poll: > 0 means underpredicted by poll
dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

VA <- dat

saveRDS(VA, file="Rdata/VApolls14.RDS")
rm(dat)

######################################################################################
##  VERMONT
######################################################################################

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "VT")     # Change for each state
# Calculate Margin of Error based on sample size if missing
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

# Replace with samplesize based calculation if missing
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

## Create days to election
##  Format depends on how read in (%Y is 4 digit year)

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/3/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

# Only keep those done in last 14 days for official AAPOR report
dat <- dat[dat$DaysToElection > -14,]

# Some polls in 2018 not include Buttigieg/Klobuchar so because there were "NA" that were read in as characters need to make numeric
#  Bloomberg also needs to be made numeric
#  Warren was "not asked" in some polls
dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

######################################################################################
# Measures of Poll Performance
######################################################################################

# Traditional AAPOR Measures
dat$poll.vote.margin <- (dat$Sanders-dat$Biden) - (dat$sanders.vote - dat$biden.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

# Works - ties are "correct"  -- Change for each state
dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Sanders[i] >= max(c(dat$Bloomberg[i],dat$Biden[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-1}
  if(dat$Sanders[i] < max(c(dat$Bloomberg[i],dat$Biden[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-0}}


mean(dat$correct.pred)

###########################################
# Now Look at Candidate specific error
###########################################

# Vote - Poll: > 0 means underpredicted by poll
dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

VT <- dat

saveRDS(VT, file="Rdata/VTpolls14.RDS")
rm(dat)

######################################################################################
##  WASHINGTON
######################################################################################

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "WA")     # Change for each state
# Calculate Margin of Error based on sample size if missing
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

# Replace with samplesize based calculation if missing
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

## Create days to election
##  Format depends on how read in (%Y is 4 digit year)

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/10/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

# Only keep those done in last 14 days for official AAPOR report
dat <- dat[dat$DaysToElection > -14,]

# Some polls in 2018 not include Buttigieg/Klobuchar so because there were "NA" that were read in as characters need to make numeric
#  Bloomberg also needs to be made numeric
#  Warren was "not asked" in some polls
dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

######################################################################################
# Measures of Poll Performance
######################################################################################

# Traditional AAPOR Measures
dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

# Works - ties are "correct"  -- Change for each state
dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

###########################################
# Now Look at Candidate specific error
###########################################

# Vote - Poll: > 0 means underpredicted by poll
dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

WA <- dat

saveRDS(WA, file="Rdata/WApolls14.RDS")
rm(dat)

######################################################################################
##  ARIZONA
######################################################################################

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "AZ")     # Change for each state
# Calculate Margin of Error based on sample size if missing
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

# Replace with samplesize based calculation if missing
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

## Create days to election
##  Format depends on how read in (%Y is 4 digit year)

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/17/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

# Only keep those done in last 14 days for official AAPOR report
dat <- dat[dat$DaysToElection > -14,]

# Some polls in 2018 not include Buttigieg/Klobuchar so because there were "NA" that were read in as characters need to make numeric
#  Bloomberg also needs to be made numeric
#  Warren was "not asked" in some polls
dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

######################################################################################
# Measures of Poll Performance
######################################################################################

# Traditional AAPOR Measures
dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

# Works - ties are "correct"  -- Change for each state
dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

###########################################
# Now Look at Candidate specific error
###########################################

# Vote - Poll: > 0 means underpredicted by poll
dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

AZ <- dat

saveRDS(AZ, file="Rdata/AZpolls14.RDS")
rm(dat)


######################################################################################
##  FLORIDA
######################################################################################

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "FL")     # Change for each state
# Calculate Margin of Error based on sample size if missing
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

# Replace with samplesize based calculation if missing
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

## Create days to election
##  Format depends on how read in (%Y is 4 digit year)

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/17/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

# Only keep those done in last 14 days for official AAPOR report
dat <- dat[dat$DaysToElection > -14,]

# Some polls in 2018 not include Buttigieg/Klobuchar so because there were "NA" that were read in as characters need to make numeric
#  Bloomberg also needs to be made numeric
#  Warren was "not asked" in some polls
dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

######################################################################################
# Measures of Poll Performance
######################################################################################

# Traditional AAPOR Measures
dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

# Works - ties are "correct"  -- Change for each state
dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

###########################################
# Now Look at Candidate specific error
###########################################

# Vote - Poll: > 0 means underpredicted by poll
dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

FL <- dat

saveRDS(FL, file="Rdata/FLpolls14.RDS")
rm(dat)


######################################################################################
##  ILLINOIS
######################################################################################

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "IL")     # Change for each state
# Calculate Margin of Error based on sample size if missing
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

# Replace with samplesize based calculation if missing
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

## Create days to election
##  Format depends on how read in (%Y is 4 digit year)

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/17/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

# Only keep those done in last 14 days for official AAPOR report
dat <- dat[dat$DaysToElection > -14,]

# Some polls in 2018 not include Buttigieg/Klobuchar so because there were "NA" that were read in as characters need to make numeric
#  Bloomberg also needs to be made numeric
#  Warren was "not asked" in some polls
dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

######################################################################################
# Measures of Poll Performance
######################################################################################

# Traditional AAPOR Measures
dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

# Works - ties are "correct"  -- Change for each state
dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

###########################################
# Now Look at Candidate specific error
###########################################

# Vote - Poll: > 0 means underpredicted by poll
dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

IL <- dat

saveRDS(IL, file="Rdata/ILpolls14.RDS")
rm(dat)


######################################################################################
##  WISCONSIN
######################################################################################

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "WI")     # Change for each state
# Calculate Margin of Error based on sample size if missing
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

# Replace with samplesize based calculation if missing
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

## Create days to election
##  Format depends on how read in (%Y is 4 digit year)

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("4/7/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

# Only keep those done in last 14 days for official AAPOR report
dat <- dat[dat$DaysToElection > -14,]

# Some polls in 2018 not include Buttigieg/Klobuchar so because there were "NA" that were read in as characters need to make numeric
#  Bloomberg also needs to be made numeric
#  Warren was "not asked" in some polls
dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

######################################################################################
# Measures of Poll Performance
######################################################################################

# Traditional AAPOR Measures
dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

# Works - ties are "correct"  -- Change for each state
dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

###########################################
# Now Look at Candidate specific error
###########################################

# Vote - Poll: > 0 means underpredicted by poll
dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

WI <- dat

saveRDS(WI, file="Rdata/WIpolls14.RDS")
rm(dat)



