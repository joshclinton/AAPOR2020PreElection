#########################################################################
#########################################################################
# AAPOR Task Force on 2020 Pre-Election Polls: 
# Performance of Popular Vote Polls
#########################################################################
#########################################################################

library(miceadds)
library(stargazer)
library(stringr)
library(plyr)
library(tidyverse)
library(ggplot2)
library(gplots)
library(ggpubr)
library(readxl)

######################################################################################
##  Overall assessment: all polls
######################################################################################


dat <- read_excel("~/Google Drive/AAPOR.PreElectionPolling/2020 General Election Polling Data/GenPresPolling.xlsx", sheet = "NationalPolls")

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("11/3/2020", "%m/%d/%Y")   
dat$DaysToElection <-  dat$EndDate - election.day

# Recode missing
dat[dat==-99]<-NA


# Calculate Margin of Error based on sample size if missing

dat$MoE <- as.numeric(dat$MoE)
dat$SampleSize <- as.numeric(dat$SampleSize)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]



# Create measures of polling error
###############################################################################

# Signed Error: Certified Vote
dat$SignedError <- (dat$Biden - dat$Trump) - (dat$DemCertVote-dat$RepCertVote)

# Absolute Error: Certified Vote
dat$AbsError <-  abs(dat$SignedError)

# Partisan Error
dat$RepError <- dat$Trump - dat$RepCertVote
dat$DemError <- dat$Biden - dat$DemCertVote 

# Percent Correctly Predicted
dat$BidenWinPoll <- as.logical(dat$Biden > dat$Trump)
dat$PollWin <- NA
dat$PollWin[dat$Biden > dat$Trump] <- "Dem"
dat$PollWin[dat$Biden < dat$Trump] <- "Rep"
dat$PollWin[dat$Biden == dat$Trump] <- "Tie"
dat$poll.predicted <- dat$CorrectWinner <- as.logical(dat$PollWin == dat$Winner)

nrow(dat)
length(table(dat$Conducted))

dat$DaysToElection <- as.numeric(dat$DaysToElection)

png(file="~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/Fig0_PresidentialPollsTiming.png",width=900, height=600)
hist(dat$DaysToElection,breaks=40,xlab="Days between Last Polling Day and Election Day",
     main="Timing of Presidential Polls Relative to Election Day",
     col = "GREY")
dev.off()



############################################################################################
##    Last 2 weeks only
############################################################################################

dat <- subset(dat,dat$DaysToElection > -14)

nrow(dat)
length(table(dat$Conducted))

mean(dat$SignedError,na.rm=TRUE)
mean(dat$AbsError,na.rm=TRUE)
mean(dat$RepError, na.rm=TRUE)
mean(dat$DemError, na.rm=TRUE)
mean(dat$CorrectWinner)

dat$PollMargin20 <- abs(dat$Biden - dat$Trump)
dat$Margin20 <- abs(dat$DemCertVote - dat$RepCertVote)
mean(dat$poll.predicted,na.rm=TRUE)
length(dat$poll.predicted)
mean(dat$poll.predicted[dat$PollMargin20 > dat$MoE],na.rm=TRUE)
length(dat$poll.predicted[dat$PollMargin20 > dat$MoE])
mean(dat$poll.predicted[dat$Margin20 > dat$MoE],na.rm=TRUE)
length(dat$poll.predicted[dat$Margin20 > dat$MoE])

nrow(dat)

##    Last weeks only
dat2 <- subset(dat,dat$DaysToElection >= -7)
mean(dat2$SignedError,na.rm=TRUE)
mean(dat2$AbsError,na.rm=TRUE)
length(table(dat2$Conducted))

# Last 3 Days
dat3 <- subset(dat2,dat2$DaysToElection >= -3)
nrow(dat3)
length(table(dat3$Conducted))

mean(dat3$SignedError,na.rm=TRUE)
mean(dat3$AbsError,na.rm=TRUE)

######################################################################################
##        Error by Collapsed Mode
######################################################################################

dat.unknown <- dat[dat$Mode=="Phone - unknown",]
table(dat$Mode)
dat$Mode[dat$Mode=="IVR/Text"] <- "NA"
100*prop.table(table(dat$Mode))

######################################################################################
##        Error by Collapsed Mode: Alternative Recode with Mixed category
######################################################################################

dat$ModeRecode2 <- NA
dat$ModeRecode2[dat$Mode=="Live phone - RDD"] <- "Phone"
dat$ModeRecode2[dat$Mode=="Live phone - RBS"] <- "Phone"
dat$ModeRecode2[dat$Mode=="Phone - uknown"] <- NA
dat$ModeRecode2[dat$Mode=="Online"] <- "Online"
dat$ModeRecode2[dat$Mode=="Online/Text"] <- "Mixed"
dat$ModeRecode2[dat$Mode=="Online/Phone"] <- "Mixed"
dat$ModeRecode2[dat$Mode=="Phone/Online"] <- "Mixed"
dat$ModeRecode2[dat$Mode=="IVR/Live Phone"] <- "Mixed"
dat$ModeRecode2[dat$Mode=="IVR/Text"] <- "Mixed"
dat$ModeRecode2[dat$Mode=="IVR/Online"] <- "Mixed"


######################################################################################
##        Error by Collapsed Mode
######################################################################################

tapply(dat$RepError,dat$Mode,mean)
tapply(dat$SignedError,dat$Mode,mean)
tapply(dat$AbsError,dat$Mode,mean)

tapply(dat$DemError,dat$ModeRecode2,mean)
tapply(dat$RepError,dat$ModeRecode2,mean)
tapply(dat$SignedError,dat$ModeRecode2,mean)
tapply(dat$AbsError,dat$ModeRecode2,mean)

tapply(dat$Biden + dat$Trump,dat$ModeRecode2,mean)

png(file="~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/SignedErrorByModeRecodeNationalPresVote.png",width=480, height=480)
boxplot(dat$SignedError~dat$ModeRecode2,ylim=c(-10,20),ylab="(Biden Poll - Trump Poll) - (Biden Vote - Trump Vote)",xlab="",
        main="Signed Error by Mode in National Presidential Polls: \n Last Two Weeks")
text(seq(1,3),rep(-10,times=3),
     round(tapply(dat$SignedError,dat$ModeRecode2,mean),digits=1),cex=1.5)
abline(h=0,lty=2)
dev.off()

table(dat$ModeRecode2)

table(dat$PctWhite)
table(dat$PctRepublicans)
table(dat$PctStrongRepublican)

# Only 27% reported partisan marginals
sum(as.logical(!is.na(as.numeric(dat$PctDemocrats))))/nrow(dat)


plot(dat$PctDemocrats,dat$PctRepublicans,main="Partisan Marginals in National Polls: Last Two Weeks",
     xlab="Percentage Democrats",ylab="Percentage Republicans")

hist(as.numeric(dat$PctDemocrats)-as.numeric(dat$PctRepublicans),main="Difference in National Partisan Marginals: Last Two Weeks")

dat$ptydiff <- as.numeric(dat$PctDemocrats)-as.numeric(dat$PctRepublicans)


plot(dat$ptydiff,dat$RepError)
plot(dat$ptydiff,dat$SignedError,pch=18,xlab="Reported Pct Dem - Pct Rep in National Sample",ylab="Signed Error")
abline(lm(dat$SignedError~dat$ptydiff))

sum(as.logical(!is.na(as.numeric(dat$Clinton2016))))/nrow(dat)
sum(as.logical(!is.na(as.numeric(dat$Trump2016))))/nrow(dat)

plot(dat$Clinton2016,dat$Trump2016,main="2016 Candidate Marginals in National Polls: Last Two Weeks",
     xlab="Percentage Clinton 2016",ylab="Percentage Trump 2016")

hist(as.numeric(dat$Clinton2016)-as.numeric(dat$Trump2016),main="Difference in National 2016 Candidate Marginals: Last Two Weeks",
     xlab="Clinton 2016 - Trump 2016")


plot(dat$Trump2016,dat$RepError)
plot(dat$Clinton2016,dat$RepError)
