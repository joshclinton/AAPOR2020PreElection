#########################################################################
#########################################################################
# AAPOR Task Force on 2020 Pre-Election Polls: 
# Performance of State Presidential Polls
# Josh Clinton
# Vanderbilt University
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

# Get from the AnalyzeStateSenGubPolls.R
# state.with.sengub
# This identifies states with a senate or gubernatorial election for comparing similar states across race types

sengub <- as.data.frame(cbind(state.with.sengub,rep(1,times = length(state.with.sengub))))
colnames(sengub) <- c("State","SenGubElection20")
sengub$SenGubElection20 <- as.numeric(sengub$SenGubElection20)

# Merge in 2016 vote data from AAPOR 2016 (note that Arkansas had wrong postal code in 2016 and was corrected in 2020)

dat16 <- election2016 <- read.csv("~/Google Drive/AAPOR.PreElectionPolling/Historical Polls Original/Original Data/AAPOR2016-PresidentialGeneralPollData/election2016.csv")
dat16 <- merge(dat16,sengub, by = "State", all = TRUE)
dat16$Clinton16 <- dat16$Clinton*100
dat16$Trump16 <- dat16$Trump*100
dat16$Geography <- dat16$State
dat16$TrumpWin16 <- dat16$TrumpWin
dat16$Margin16 <- dat16$Trump16 - dat16$Clinton16

dat16 <- dat16[,-c(1,2,3,4,5,6)]  # Drop extraneous columns

# Read in Polling Data for 2020
dat <- read_excel("~/Google Drive/AAPOR.PreElectionPolling/2020 General Election Polling Data/GenPresPolling.xlsx", sheet = "StatePolls")
dat <- merge(dat,dat16, by = "Geography", all = TRUE)

# Create DaysToElection variable - last day of field period to Election Day
dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("11/3/2020", "%m/%d/%Y")   
dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysToElection <- as.numeric(dat$DaysToElection)

# Recode missing data in any cells
dat[dat == -99] <- NA

# Classify State Type based on 2016 margin

dat$StateType[dat$Margin16 > 5] <- "Solid Rep"
dat$StateType[dat$Margin16  < -5] <- "Solid Dem"
dat$StateType[abs(dat$Margin16) <= 5] <- "Competitive"
dat$StateType <- as.factor(dat$StateType)
dat$StateType <- relevel(dat$StateType, "Solid Dem")

dat$SampleSize <- as.numeric(dat$SampleSize)
dat$MoE <- as.numeric(dat$MoE)

# Calculate Margin of Error based on sample size if missing
# Many polls failed to report a margin of error -- if so use a sample-size based calculation
dat$MoE2 <- dat$MoE
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96
dat$MoE2[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

# % Response other than Biden/Trump in the presidential horse race
dat$DK <- 100 - dat$Biden - dat$Trump
mean(dat$DK, na.rm = TRUE)

# Create measures of polling error
###############################################################################

# Signed Error: Certified Vote
dat$SignedError <- (dat$Biden - dat$Trump) - (dat$DemCertVote - dat$RepCertVote)

# Absolute Error: Certified Vote
dat$AbsError <-  abs(dat$SignedError)

# Partisan/Candidate-specific Error
dat$RepError <- dat$Trump - dat$RepCertVote
dat$DemError <- dat$Biden - dat$DemCertVote 

# Two-Candidate Errors (removing those who choose response other than Biden or Trump from denominator)
dat$BidenTrump <- dat$Biden + dat$Trump
dat$BidenNorm <- 100*(dat$Biden / dat$BidenTrump)
dat$TrumpNorm <- 100*(dat$Trump / dat$BidenTrump)
dat$SignedErrorNorm <- (dat$BidenNorm - dat$TrumpNorm) - (dat$DemCertVote - dat$RepCertVote)
dat$AbsErrorNorm <- abs(dat$SignedErrorNorm)
dat$RepNormError <- dat$TrumpNorm - dat$RepCertVote
dat$DemNormError <- dat$BidenNorm - dat$DemCertVote 


# Percent Correctly Predicted
# Also have poll.predicted in Excel that does same calculation
dat$BidenWinPoll <- as.logical(dat$Biden > dat$Trump)
dat$PollWin <- NA
dat$PollWin[dat$Biden > dat$Trump] <- "Dem"
dat$PollWin[dat$Biden < dat$Trump] <- "Rep"
dat$PollWin[dat$Biden == dat$Trump] <- "Tie"
dat$CorrectWinner <- as.logical(dat$PollWin == dat$Winner)
dat$AbsPollMargin <- abs(dat$Biden - dat$Trump)

# Just getting counts of number of various types of polls
nrow(dat)
length(table(dat$Conducted))
nrow(dat[dat$StateType == "Competitive",])
length(table(dat$Conducted[dat$StateType == "Competitive"]))

png(file = "~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/Fig1_StateLevelPollsTiming.png",
    width = 800, 
    height = 600)
hist(dat$DaysToElection,
     breaks = 40,
     xlab = "Days between Last Polling Day and Election Day",
     main = "Timing of State-Level Presidential Polls Relative to Election Day",
     ylab = "",
     cex.main = 2,
     cex.lab = 2,
     col = "GREY")
dev.off()

dat.all <- dat

############################################################################################
##    Last 2 weeks only: Election Day within 14 days of final field period day
############################################################################################

dat <- subset(dat,dat$DaysToElection > -14)
nrow(dat)
length(table(dat$Conducted))
nrow(dat[dat$StateType == "Competitive",])
length(table(dat$Conducted[dat$StateType == "Competitive"]))
mean(dat$DK,na.rm = TRUE)


# Table 1
mean(dat$SignedError, na.rm = TRUE)
mean(dat$AbsError, na.rm = TRUE)

# Table 3
mean(dat$RepError, na.rm = TRUE)
mean(dat$DemError, na.rm = TRUE)
mean(dat$RepNormError, na.rm = TRUE)
mean(dat$DemNormError, na.rm = TRUE)

mean(dat$RepError[dat$SenGubElection20 == TRUE], na.rm = TRUE)
mean(dat$DemError[dat$SenGubElection20 == TRUE], na.rm = TRUE)
sum(table(dat$RepError[dat$SenGubElection20 == TRUE]))

# Table 4
mean(dat$CorrectWinner)
mean(dat$poll.predicted)
mean(dat$poll.predicted[dat$SenGubElection20 == TRUE],na.rm = TRUE)

# Correct Prediction
dat$PollMargin20 <- abs(dat$Biden - dat$Trump)
dat$Margin20 <- abs(dat$DemCertVote - dat$RepCertVote)
mean(dat$poll.predicted, na.rm = TRUE)
length(dat$poll.predicted)

sum(!is.na(dat$MoE))
sum(!is.na(dat$MoE))/nrow(dat)

# Among online polls -- are they included in the count of polls with a MoE? Yes.
sum(!is.na(dat$MoE[dat$Mode == "Online"]))/sum(as.logical(dat$Mode == "Online"))

mean(dat$MoE, na.rm = TRUE)

mean(abs(dat$RepError) > dat$MoE, na.rm = TRUE)

mean(abs(dat$DemError) > dat$MoE, na.rm = TRUE)
1 - mean(abs(dat$RepNormError) > dat$MoE, na.rm = TRUE)
1 - mean(abs(dat$DemNormError) > dat$MoE, na.rm = TRUE)

mean(dat$poll.predicted[dat$PollMargin20 > dat$MoE], na.rm = TRUE)
length(dat$poll.predicted[dat$PollMargin20 > dat$MoE])
mean(dat$poll.predicted[dat$Margin20 > dat$MoE], na.rm = TRUE)
length(dat$poll.predicted[dat$Margin20 > dat$MoE])

mean(dat$poll.predicted[dat$PollMargin20 > 2*dat$MoE], na.rm = TRUE)
length(dat$poll.predicted[dat$PollMargin20 > 2*dat$MoE])
mean(dat$poll.predicted[dat$Margin20 > 2*dat$MoE], na.rm = TRUE)
length(dat$poll.predicted[dat$Margin20 > 2*dat$MoE])

mean(dat$SignedError[dat$PollMargin20 > 2*dat$MoE], na.rm = TRUE)
mean(dat$AbsError[dat$PollMargin20 > 2*dat$MoE], na.rm = TRUE)

# Correct Prediction
mean(dat$poll.predicted[dat$SenGubElection20 == 1], na.rm = TRUE)
table(dat$SenGubElection20 == 1)
mean(dat$poll.predicted[dat$SenGubElection20 == TRUE & dat$PollMargin20 > dat$MoE], na.rm = TRUE)
table(dat$SenGubElection20 == TRUE & dat$PollMargin20 > dat$MoE)
mean(dat$poll.predicted[dat$SenGubElection20 == TRUE & dat$Margin20 > dat$MoE], na.rm = TRUE)
table(dat$SenGubElection20 == TRUE & dat$Margin20 > dat$MoE)

mean(dat$poll.predicted[dat$SenGubElection20 == TRUE & dat$PollMargin20 > 2*dat$MoE], na.rm = TRUE)
table(dat$SenGubElection20 == TRUE & dat$PollMargin20 > 2*dat$MoE)
mean(dat$poll.predicted[dat$SenGubElection20 == TRUE & dat$Margin20 > 2*dat$MoE], na.rm = TRUE)
table(dat$SenGubElection20 == TRUE & dat$Margin20 > 2*dat$MoE)

nrow(dat)
mean(dat$DK, na.rm = TRUE)

png(file = "~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/Fig1B_PresidentialStatePollsDKEffect.png",
    width = 700,
    height = 700)
par(mfrow = c(2,1))
hist(dat$SignedError[dat$DK > 5],
     main = "Distribution of Signed Errors if 5%+ Choose Other",
     xlab = "",
     breaks = seq(-15,30))
abline(v = mean(dat$SignedError[dat$DK > 5]),lwd = 2)

hist(dat$SignedError[dat$DK <= 5],
     main = "Distribution of Signed Errors if <= 5% Choose Other",
     xlab = "",
     breaks = seq(-15,30))
abline(v = mean(dat$SignedError[dat$DK <= 5]),lwd = 2)
dev.off()

############################################################################################
##    Errors Among Polls Done in Last week only
############################################################################################

dat2 <- subset(dat,dat$DaysToElection >= -7)
mean(dat2$DK, na.rm = TRUE)
nrow(dat2[dat2$StateType == "Competitive",])
length(table(dat2$Conducted[dat2$StateType == "Competitive"]))

# Table 1
mean(dat2$SignedError, na.rm = TRUE)
mean(dat2$AbsError, na.rm = TRUE)

# Table 3
mean(dat2$RepError, na.rm = TRUE)
mean(dat2$DemError, na.rm = TRUE)

mean(dat2$RepError[dat2$SenGubElection20 == TRUE], na.rm = TRUE)
mean(dat2$DemError[dat2$SenGubElection20 == TRUE], na.rm = TRUE)
sum(table(dat2$RepError[dat2$SenGubElection20 == TRUE]))

# Table 4
mean(dat2$CorrectWinner)
mean(dat2$poll.predicted)
mean(dat2$poll.predicted[dat2$SenGubElection20 == TRUE], na.rm = TRUE)

mean(dat2$poll.predicted[dat2$AbsPollMargin >= 1])
mean(dat2$poll.predicted[dat2$AbsPollMargin >= 5])

nrow(dat2)
length(table(dat2$Conducted))

dat3 <- subset(dat2,dat2$DaysToElection >= -3)
nrow(dat3)
length(table(dat3$Conducted))
mean(dat3$DK, na.rm = TRUE)

############################################################################################
##    Errors Among Polls Done in Last 3 Days Only
############################################################################################

dat3 <- subset(dat,dat$DaysToElection >= -3)

mean(dat3$SignedError, na.rm = TRUE)
mean(dat3$AbsError, na.rm = TRUE)
nrow(dat3[dat3$StateType == "Competitive",])
length(table(dat3$Conducted[dat3$StateType == "Competitive"]))

# Summarize all state-level Presidential Polls

png(file = "~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/Fig4_PresidentialStatePollsSummaryPooled.png",
    width = 800,
    height = 900)
par(mfrow = c(3,2))
hist(dat$SignedError,
     main = "Last Two Weeks",
     xlab = "Signed Error on Margin",
     xlim = c(-20,20),
     breaks = seq(-21,21,by = 2),
     cex.lab = 1.5,
     cex.main = 2)
abline(v = mean(dat$SignedError))
text(-10,40, "Estimated \n Margin Too \n Republican", cex = 2)
text(10,40, "Estimated \n Margin Too \n Democratic", cex = 2)
hist(dat$AbsError,
     xlab = "Absolute Error on Margin",
     main = "Last Two Weeks",
     xlim = c(0,20),
     breaks = seq(-.5,21.5,by = 1),
     cex.lab = 1.5,
     cex.main = 2)
abline(v = mean(dat$AbsError))
hist(dat2$SignedError,
     main = "Last Week",
     xlab = "Signed Error on Margin",
     xlim = c(-20,20),
     breaks = seq(-21,21,by = 2),
     cex.lab = 1.5,
     cex.main = 2)
abline(v = mean(dat2$SignedError))
text(-10,40,"Estimated \n Margin Too \n Republican", cex = 2)
text(10,40,"Estimated \n Margin Too \n Democratic", cex = 2)
hist(dat2$AbsError,
     xlab = "Absolute Error on Margin",
     main = "Last Week",
     xlim = c(0,20),
     breaks = seq(-.5,21.5, by = 1),
     cex.lab = 1.5,
     cex.main = 2)
abline(v = mean(dat2$AbsError))
hist(dat3$SignedError,
     main = "Last 3 Days",
     xlab = "Signed Error on Margin",
     xlim = c(-20,20),
     breaks = seq(-21,21, by = 2),
     cex.lab = 1.5,
     cex.main = 2)
abline(v = mean(dat3$SignedError))
text(-10,30,"Estimated \n Margin Too \n Republican", cex = 2)
text(10,30,"Estimated \n Margin Too \n Democratic", cex = 2)
hist(dat3$AbsError,
     xlab = "Absolute Error on Margin",
     main = "Last 3 Days",
     xlim = c(0,20),
     breaks = seq(-.5,21.5, by = 1),
     cex.lab = 1.5,
     cex.main = 2)
abline(v = mean(dat3$AbsError))
dev.off()

dat.all <- dat.all[dat.all$SignedError > -21 & dat.all$SignedError < 21,]

png(file = "~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/FigPresentation_PresidentialStatePollsSignedPooled.png",
    width = 900,
    height = 900)
par(mfrow = c(2,2))
hist(dat.all$SignedError,
     main = "All Polls",
     xlab = "Signed Error on Margin",
     xlim = c(-20,20),
     breaks = seq(-21,21, by = 2),
     cex.lab = 1.5,
     cex.main = 2)
abline(v = mean(dat$SignedError))
abline(v = 0, lwd = 2)
text(-10,200,"Estimated \n Margin Too \n Republican", cex = 2)
text(10,200,"Estimated \n Margin Too \n Democratic", cex = 2)
hist(dat$SignedError,
     main = "Last Two Weeks",
     xlab = "Signed Error on Margin",
     xlim = c(-20,20),
     breaks = seq(-21,21, by = 2),
     cex.lab = 1.5,
     cex.main = 2)
abline(v = mean(dat$SignedError))
abline(v = 0, lwd = 2)
text(-10,40,"Estimated \n Margin Too \n Republican", cex = 2)
text(10,40,"Estimated \n Margin Too \n Democratic", cex = 2)
hist(dat2$SignedError,
     main = "Last Week",
     xlab = "Signed Error on Margin",
     xlim = c(-20,20),
     breaks = seq(-21,21, by = 2),
     cex.lab = 1.5,
     cex.main = 2)
abline(v = mean(dat2$SignedError))
abline(v = 0,lwd = 2)
text(-10,40,"Estimated \n Margin Too \n Republican", cex = 2)
text(10,40,"Estimated \n Margin Too \n Democratic", cex = 2)
hist(dat3$SignedError,
     main = "Last 3 Days",
     xlab = "Signed Error on Margin",
     xlim = c(-20,20),
     breaks = seq(-21,21, by = 2),
     cex.lab = 1.5,
     cex.main = 2)
abline(v = mean(dat3$SignedError))
abline(v = 0, lwd = 2)
text(-10,30,"Estimated \n Margin Too \n Republican", cex = 2)
text(10,30,"Estimated \n Margin Too \n Democratic", cex = 2)
dev.off()

############################################################################################################

png(file = "~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/MarginOfErrorInadequate.png",
    width = 600, 
    height = 480)
hist(dat$AbsError,
     main = "Absolute Error: \n State Level Presidential Polls in Last Two Weeks",
     cex.main = 2,
     xlab = "Absolute Error on the Margin",
     breaks = seq(0,20))
abline(v = mean(dat$MoE, na.rm = TRUE), lwd = 4, lty = 2)
abline(v = mean(2*dat$MoE, na.rm = TRUE), lwd = 4, lty = 2)
text(mean(dat$MoE, na.rm = TRUE) - 2,50,"Avg. Margin \n of Error")
text(mean(2*dat$MoE, na.rm = TRUE) + 3,50,"2 x Avg. Margin \n of Error")
dev.off()

table(dat$AbsError > dat$MoE)
table(dat$AbsError > 2*dat$MoE)

mean(dat$MoE, na.rm = TRUE)
mean(2*dat$MoE, na.rm = TRUE)

# What if we renormalize to exclude undecided respondents?
# TABLE 5A

mean(dat$SignedErrorNorm)
mean(dat$RepNormError)
mean(dat$DemNormError)

mean(dat$SignedError)
mean(dat$RepError)
mean(dat$DemError)

######################################################################################
##        Error by Collapsed Mode
######################################################################################

# Recode infrequent categories to "Mixed"

table(dat$Mode)
dat$Mode[dat$Mode == "IVR"] <- "NA"
dat$Mode[dat$Mode == "IVR/Text"] <- "Mixed"
dat$Mode[dat$Mode == "Online/Text"] <- "Mixed"
dat$Mode[dat$Mode == "Phone - unknown"] <- "NA"
table(dat$Mode)

# Look at unknown phone polls
dat.unknown <- dat[dat$Mode == "Phone - unknown",]
dat.misc <- dat[dat$Mode == "Misc.",]

# Figure 1
100*prop.table(table(dat$Mode))
100*prop.table(table(dat$Mode[dat$StateType == "Competitive"]))

table(dat$Mode)

# TABLE 5
tapply(dat$SignedError,dat$Mode,mean)
tapply(dat$AbsError,dat$Mode,mean)
tapply(dat$SignedError,dat$Mode,median)
tapply(dat$AbsError,dat$Mode,median)

summary(lm(dat$SignedError~as.factor(dat$Mode) + dat$Margin16 - 1))
summary(lm(dat$AbsError~as.factor(dat$Mode) + dat$Margin16 - 1))

# Renormalized

tapply(dat$SignedErrorNorm,dat$Mode,mean)
tapply(dat$AbsErrorNorm,dat$Mode,mean)
tapply(dat$poll.predicted,dat$Mode,mean)
table(dat$Mode)

# Poll Error in Competitive States Only

tapply(dat$SignedErrorNorm[dat$StateType == "Competitive"],dat$Mode[dat$StateType == "Competitive"],mean)
tapply(dat$AbsErrorNorm[dat$StateType == "Competitive"],dat$Mode[dat$StateType == "Competitive"],mean)
table(dat$Mode[dat$StateType == "Competitive"])
tapply(dat$CorrectWinner[dat$StateType == "Competitive"],dat$Mode[dat$StateType == "Competitive"],mean)

dat$Mode[dat$Mode == "NA"] <- "Misc."
dat$Mode[dat$Mode == "Mixed"] <- "Misc."
######################################################################################
##        Error by Collapsed Mode: Alternative Recoding with Mixed category
######################################################################################

dat$ModeRecode2 <- NA
dat$ModeRecode2[dat$Mode == "Live phone - RDD"] <- "Phone"
dat$ModeRecode2[dat$Mode == "Live phone - RBS"] <- "Phone"
dat$ModeRecode2[dat$Mode == "Phone - uknown"] <- NA
dat$ModeRecode2[dat$Mode == "Online"] <- "Online"
dat$ModeRecode2[dat$Mode == "Online/Text"] <- "Mixed"
dat$ModeRecode2[dat$Mode == "Online/Phone"] <- "Mixed"
dat$ModeRecode2[dat$Mode == "Phone/Online"] <- "Mixed"
dat$ModeRecode2[dat$Mode == "IVR/Live Phone"] <- "Mixed"
dat$ModeRecode2[dat$Mode == "IVR/Text"] <- "Mixed"
dat$ModeRecode2[dat$Mode == "IVR/Online"] <- "Mixed"

dat.nomisc <- dat[-seq(1,nrow(dat))*as.logical(dat$Mode == "Misc."), ]

table(dat$ModeRecode2)

tapply(dat$SignedError,dat$ModeRecode2,mean)
tapply(dat$AbsError,dat$ModeRecode2,mean)
tapply(dat$CorrectWinner,dat$ModeRecode2,mean)
table(dat$ModeRecode2)
tapply(dat.nomisc$SignedError,dat.nomisc$ModeRecode2,mean)
tapply(dat.nomisc$AbsError,dat.nomisc$ModeRecode2,mean)

summary(lm(dat$AbsError~as.factor(dat$ModeRecode2) + as.factor(dat$StateType) + dat$Margin16))

dat$PredTrumpWin <- dat$Trump > dat$Biden
dat$TrumpWin <- dat$RepCertVote > dat$DemCertVote
tapply(dat$PredTrumpWin,dat$ModeRecode2,mean)
tapply(dat$TrumpWin,dat$ModeRecode2,mean)

table(Pred = dat$PredTrumpWin[dat$ModeRecode2 == "Mixed"],Act = dat$TrumpWin[dat$ModeRecode2 == "Mixed"])
table(Pred = dat$PredTrumpWin[dat$ModeRecode2 == "Online"],Act = dat$TrumpWin[dat$ModeRecode2 == "Online"])
table(Pred = dat$PredTrumpWin[dat$ModeRecode2 == "Phone"],Act = dat$TrumpWin[dat$ModeRecode2 == "Phone"])

mean(dat$SignedError,na.rm = TRUE)
mean(dat$AbsError,na.rm = TRUE)
mean(dat$CorrectWinner)
nrow(dat)

nrow(dat)
nrow(dat.nomisc)

# FIGURE 6
png(file = "~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/Fig6_AbsSignedErrorByModeStatePresVote.png",
    width = 1000, 
    height = 900)
par(mfrow = c(2,1))
boxplot(dat.nomisc$AbsError~dat.nomisc$Mode,
        ylim = c(0,20),
        ylab = "(Biden Poll - Trump Poll) - (Biden Vote - Trump Vote)",
        xlab = "",
        main = "Absolute Error by Mode in State Presidential Polls: \n Last Two Weeks",
        cex.main = 2)
text(seq(1,6),rep(0,times = 6),
     round(tapply(dat.nomisc$AbsError,dat.nomisc$Mode,mean),digits = 1),cex = 1.5)
abline(h = 0,lty = 2)
boxplot(dat.nomisc$SignedError~dat.nomisc$Mode,
        ylim = c(-10,20),
        ylab = "(Biden Poll - Trump Poll) - (Biden Vote - Trump Vote)",
        xlab = "",
        main = "Signed Error by Mode in State Presidential Polls: \n Last Two Weeks",
        cex.main = 2)
text(seq(1,6),rep(-10,times = 6),
     round(tapply(dat.nomisc$SignedError,dat.nomisc$Mode,mean),digits = 1),cex = 1.5)
abline(h = 0,lty = 2)
dev.off()

t.test(dat.nomisc$SignedError[dat.nomisc$Mode == "Phone/Online"],dat.nomisc$SignedError[dat.nomisc$Mode == "Live phone - RDD"])
t.test(dat.nomisc$AbsError[dat.nomisc$Mode == "IVR/Online"],dat.nomisc$AbsError[dat.nomisc$Mode == "Live phone - RDD"])

sum(as.logical(dat.nomisc$SignedError[dat.nomisc$Mode == "Phone/Online"] < 0))/sum(dat.nomisc$SignedError[dat.nomisc$Mode == "Phone/Online"])

table(dat.nomisc$Conducted[dat.nomisc$Mode == "Phone/Online"])
sum(table(dat.nomisc$Conducted[dat.nomisc$Mode == "Phone/Online"]))

# Using Violin plot instead of Boxplot to show distribution of error within mode

lab1 <- round(tapply(dat.nomisc$AbsError,dat.nomisc$Mode,median),digits = 1)
lab2 <- round(tapply(dat.nomisc$SignedError,dat.nomisc$Mode,median),digits = 1)

lab1 <- round(tapply(dat.nomisc$AbsError,dat.nomisc$Mode,mean),digits = 1)
lab2 <- round(tapply(dat.nomisc$SignedError,dat.nomisc$Mode,mean),digits = 1)
indx <- c(4,1,5,2,3)

library(vioplot)
png(file = "~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/Fig6Violin_AbsErrorByModeStatePresVote.png",
    width = 1000, 
    height = 900)
par(mfrow = c(1,1))
with(dat.nomisc,vioplot(
  AbsError[Mode == "Online"],AbsError[Mode == "IVR/Online"],AbsError[Mode == "Phone/Online"],AbsError[Mode == "Live phone - RBS"],AbsError[Mode == "Live phone - RDD"],
  names = c("Online","IVR/Online","Phone/Online","Live phone RBS","Live phone RDD"),
  main = "Absolute Error by Mode in State Presidential Polls: \n Last Two Weeks",
  cex.main = 2,
  cex.axis = 1.5,
  col = "lightgrey"
))
text(seq(1,6),rep(0,times = 6),
     lab1[indx],cex = 3)
abline(h = mean(dat.nomisc$AbsError))
dev.off()

png(file = "~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/Fig6Violin_SignedErrorByModeStatePresVote.png",
    width = 1000, 
    height = 900)
par(mfrow = c(1,1))
with(dat.nomisc,vioplot(
  SignedError[Mode == "Online"],SignedError[Mode == "IVR/Online"],SignedError[Mode == "Phone/Online"],SignedError[Mode == "Live phone - RBS"],SignedError[Mode == "Live phone - RDD"],
  names = c("Online","IVR/Online","Phone/Online","Live phone RBS","Live phone RDD"),
  main = "Signed Error by Mode in State Presidential Polls: \n Last Two Weeks",
  cex.main = 2,
  cex.axis = 1.5,
  col = "lightgrey"
))
text(seq(1,6),rep(-7,times = 6),
     lab2[indx],cex = 3)
abline(h = mean(dat.nomisc$SignedError))
abline(h = 0,lty = 2)
dev.off()

library(vioplot)
png(file = "~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/Fig6Violin_AbsSignedErrorByModeStatePresVote.png",
    width = 1000, 
    height = 900)
par(mfrow = c(2,1))
with(dat.nomisc,vioplot(
  AbsError[Mode == "Online"],AbsError[Mode == "IVR/Online"],AbsError[Mode == "Phone/Online"],AbsError[Mode == "Live phone - RBS"],AbsError[Mode == "Live phone - RDD"],
  names = c("Online","IVR/Online","Phone/Online","Live phone RBS","Live phone RDD"),
  main = "Absolute Error by Mode in State Presidential Polls: \n Last Two Weeks",
  cex.main = 2,
  col = "lightgrey"
))
text(seq(1,6),rep(0,times = 6),
     lab1[indx],cex = 1.5)
abline(h = mean(dat.nomisc$AbsError))

with(dat.nomisc,vioplot(
  SignedError[Mode == "Online"],SignedError[Mode == "IVR/Online"],SignedError[Mode == "Phone/Online"],SignedError[Mode == "Live phone - RBS"],SignedError[Mode == "Live phone - RDD"],
  names = c("Online","IVR/Online","Phone/Online","Live phone RBS","Live phone RDD"),
  main = "Signed Error by Mode in State Presidential Polls: \n Last Two Weeks",
  cex.main = 2,
  col = "lightgrey"
))
text(seq(1,6),rep(-7,times = 6),
     lab2[indx],cex = 1.5)
abline(h = mean(dat.nomisc$SignedError))
abline(h = 0,lty = 2)
dev.off()

# Look at Error by Mode for "Competitive" States
# Table 5B

table(dat.nomisc$Mode[dat.nomisc$StateType == "Competitive"])
tapply(dat.nomisc$SignedError[dat.nomisc$StateType == "Competitive"],dat.nomisc$Mode[dat.nomisc$StateType == "Competitive"],mean,na.rm = TRUE)
tapply(dat.nomisc$AbsError[dat.nomisc$StateType == "Competitive"],dat.nomisc$Mode[dat.nomisc$StateType == "Competitive"],mean)
tapply(dat.nomisc$CorrectWinner[dat.nomisc$StateType == "Competitive"],dat.nomisc$Mode[dat.nomisc$StateType == "Competitive"],mean)

# SECTION 5

table(dat$StateType)
lab2 <- round(tapply(dat$SignedError,dat$StateType,mean),digits = 1)
lab1 <- round(tapply(dat$AbsError,dat$StateType,mean),digits = 1)

t.test(dat$SignedError[dat$StateType == "Solid Dem"],dat$SignedError[dat$StateType == "Solid Rep"])
t.test(dat$AbsError[dat$StateType == "Solid Dem"],dat$AbsError[dat$StateType == "Solid Rep"])

# Figure 7
png(file = "~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/Fig7_ViolinPlotAbsSignedStateTypeStatePresPolls.png",
    width = 1000,
    height = 600)
par(mfrow = c(1,2))
with(dat,vioplot(
  AbsError[StateType == "Solid Dem"],AbsError[StateType == "Competitive"],AbsError[StateType == "Solid Rep"],
  names = c("Solid Dem","Competitive","Solid Rep"),
  main = "Absolute Error by 2016 Margin: \n Last Two Weeks",
  cex.main = 2,
  col = "lightgrey"
))
abline(h = mean(dat$AbsError))

text(seq(1,3),rep(0,times = 3),
     lab1,cex = 1.5)

with(dat,vioplot(
  SignedError[StateType == "Solid Dem"],SignedError[StateType == "Competitive"],SignedError[StateType == "Solid Rep"],
  names = c("Solid Dem","Competitive","Solid Rep"),
  main = "Signed Error by 2016 Margin: \n Last Two Weeks",
  cex.main = 2,
  col = "lightgrey"
))
abline(h = 0,lty = 2,col = "grey")
text(2,-4,"Estimated Margin \n Too Republican",cex = 1.5)
text(2,10,"Estimated Margin \n Too Democratic",cex = 1.5)
abline(h = mean(dat$SignedError))
text(seq(1,3),rep(-7,times = 3),
     lab2,cex = 1.5)
dev.off()


# Figure 7
png(file = "~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/Fig7_BoxplotAbsSignedStateTypeStatePresPolls.png",
    width = 1000,
    height = 600)
par(mfrow = c(1,2))
boxplot(dat$AbsError~dat$StateType,
        ylab = "",
        xlab = "",
        main = "Absolute Error",
        ylim = c(0,20),
        cex.main = 2,
        cex.axis = 1.5)
boxplot(dat$SignedError~dat$StateType,
        ylab = "",
        xlab = "",
        main = "Signed Error",
        ylim = c(-15,20),
        cex.main = 2,
        cex.axis = 1.5)
text(2,-10,"Poll Margin \n Too Republican",cex = 2)
text(2,10,"Poll Margin \n Too Democratic",cex = 2)
abline(h = 0)
dev.off()

######################################################################################
######################################################################################
# Error By State
######################################################################################
######################################################################################

# Number of Polls
dat$Constant <- 1
tapply(dat$Constant,dat$Geography,sum)

tapply(dat$DemError,dat$Geography,mean)
tapply(dat$RepError,dat$Geography,mean)

# Average Signed Error by State
tapply(dat$SignedError,dat$Geography,mean)
write.csv(tapply(dat$SignedError,dat$Geography,mean),file = "SignedErrorState.csv")

# Average State Error
mean(tapply(dat$SignedError,dat$Geography,mean))

######################################################################################
#     Plot State Error
#     Unit of observation is a state
######################################################################################

stateSignedError <- tapply(dat$SignedError,dat$Geography,mean)
stateRepError <- tapply(dat$RepError,dat$Geography,mean)
stateRepVote <- tapply(dat$RepVote,dat$Geography,mean)
stateTrump16 <- tapply(dat$Trump16,dat$Geography,mean)

# Figure 9

png(file = "~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/Fig8_RegressionSignedErrorByVoteByState.png",
    width = 600,
    height = 600)
par(mfrow = c(1,1))
plot(stateTrump16,stateSignedError,
     type = "n",
     ylab = "Average Signed Error",
     xlab = "2016 Trump Certified Vote",
     main = "Avg. Signed Error by 2016 Trump Vote: \n Last 2 Weeks",
     cex.main = 2,
     cex.lab = 1.5)
new1 <- data.frame(stateTrump16 <- seq(0:100))
pred1 <- predict(regmod, newdata = new1, se.fit = TRUE, type = "response")
polygon(c(seq(0:100), rev(seq(100:0))), 
        c(pred1$fit - (1.96 * pred1$se.fit), rev(pred1$fit + (1.96 * pred1$se.fit))), 
        col = "lightgray", border = NA)

stateSignedError <- tapply(dat$SignedError,dat$Geography,mean)
stateTrump16 <- tapply(dat$Trump16,dat$Geography,mean)

text(stateTrump16,stateSignedError,names(stateTrump16))
abline(lm(stateSignedError~stateTrump16))
abline(h = 0,lty = 2)
dev.off()

regmod <- lm(stateSignedError~stateTrump16)

summary(lm(stateSignedError~stateTrump16))

lines(new1$x1, pred1$fit + (1.96 * pred1$se.fit), lty = 2, col = "blue")
lines(new1$x1, pred1$fit - (1.96 * pred1$se.fit), lty = 2, col = "blue")

######################################################################################
#   Plot State Error By State: Signed Error
######################################################################################

state.error <- tapply(dat$SignedError,dat$Geography,mean)
state.error.indx <- sort(state.error,index.return = TRUE)
state.error <- state.error[state.error.indx$ix]
state.postal <- names(state.error)
signed.state.error <- state.error

# Figure 10

png(file = "~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/Fig9_SignedErrorByState.png",
    width = 1000,
    height = 600)
plot(c(1,length(state.postal)),c(-10,15),
     type = "n",
     main = "Avg. Signed Error by State: \n Last Two Weeks",
     axes = FALSE,
     xlab = "",
     ylab = "Signed Error",
     cex.main = 2)
axis(2)
for(i in 1:length(state.postal)) {
  state.dat <- subset(dat,dat$Geography == state.postal[i])
  for(j in 1:nrow(state.dat)){
    points(i,jitter(state.dat$SignedError[j]),pch = 18,col = "GREY")
  }
  points(i,state.error[i],pch = 1,cex = 3)
  text(i,state.error[i],round(state.error[i]),digits = 2,cex = .9)
  text(i,-10,state.postal[i],cex = .5)
}
abline(h = 0,lty = 2)
text(25,8,"Poll Margin Favors Biden",cex = 1.5)
text(25,-8,"Poll Margin Favors Trump",cex = 1.5)
dev.off()


png(file = "~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/Fig9Flip_SignedErrorByState.png",
    width = 1000,
    height = 1000)
plot(c(-10,15),c(1,length(state.postal)),
     type = "n",
     main = "Avg. Signed Error by State: \n Last Two Weeks",
     axes = FALSE,
     ylab = "",
     xlab = "Signed Error",
     cex.main = 2)
axis(1)
for(i in 1:length(state.postal)){
  state.dat <- subset(dat,dat$Geography == state.postal[i])
  for(j in 1:nrow(state.dat)){
    points(jitter(state.dat$SignedError[j]),i,pch = 18,col = "GREY")
  }
  points(state.error[i],i,pch=1,cex=3)
  text(state.error[i],i,round(state.error[i]),digits = 2,cex = .9)
  text(-10,i,state.postal[i],cex = 1)
}
abline(v = 0,lty = 2)
text(9,25,"Poll Margin \n Favors Biden",cex = 2)
text(-5,25,"Poll Margin \n Favors Trump",cex = 2)
dev.off()

######################################################################################
#   Plot State Error By State: Absolute Error
######################################################################################

state.error <- tapply(dat$AbsError,dat$Geography,mean)
state.error.indx <- sort(state.error,index.return = TRUE)
state.error <- state.error[state.error.indx$ix]
state.postal <- names(state.error)
absolute.state.error <- state.error

# Figure 10

png(file = "~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/Fig10_AbsErrorByState.png",
    width = 1000,
    height = 600)
plot(c(1,length(state.postal)),c(0,15),
     type = "n",
     main = "Avg. Absolute Error by State: \n Last Two Weeks",
     axes = FALSE,
     xlab = "",
     ylab = "Absolute Error",
     cex.main = 2)
axis(2)
for(i in 1:length(state.postal)){
  state.dat <- subset(dat,dat$Geography == state.postal[i])
  for(j in 1:nrow(state.dat)){
    points(i,jitter(state.dat$AbsError[j]),pch = 18,col = "GREY")
  }
  points(i,state.error[i],pch = 1,cex = 3)
  text(i,state.error[i],round(state.error[i]),digits = 2,cex = .9)
  text(i,0,state.postal[i],cex = .5)
}
dev.off()

# Figure 10

png(file="~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/Fig10Flip_AbsErrorByState.png",width=1000,height=1000)
plot(c(0,15),c(1,length(state.postal)),type="n",main="Avg. Absolute Error by State: \n Last Two Weeks",axes=FALSE,ylab="",xlab="Absolute Error",cex.main=2)
axis(1)
for(i in 1:length(state.postal)){
  state.dat <- subset(dat,dat$Geography==state.postal[i])
  for(j in 1:nrow(state.dat)){
    points(jitter(state.dat$AbsError[j]),i,pch=18,col="GREY")
  }
  points(state.error[i],i,pch=1,cex=3)
  text(state.error[i],i,round(state.error[i]),digits=2,cex=.9)
  text(0,i,state.postal[i],cex=1)
}
dev.off()


######################################################################################
#   Plot State Error By State: Biden Error
######################################################################################

state.error <- tapply(dat$DemError,dat$Geography,mean)
state.error.indx <- sort(state.error,index.return=TRUE)
state.error <- state.error[state.error.indx$ix]
state.postal <- names(state.error)
biden.state.error <- state.error

# Figure 13

png(file="~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/Fig13_BidenErrorByState.png",width=1000,height=600)
plot(c(1,length(state.postal)),c(-10,15),type="n",main="Biden Poll - Certified Vote: \n Last Two Weeks",axes=FALSE,xlab="",ylab="Poll - Vote Margin",cex.main=2)
axis(2)
for(i in 1:length(state.postal)){
  state.dat <- subset(dat,dat$Geography==state.postal[i])
  for(j in 1:nrow(state.dat)){
    points(i,jitter(state.dat$DemError[j]),pch=18,col="GREY")
  }
  points(i,state.error[i],pch=1,cex=3)
  text(i,state.error[i],round(state.error[i]),digits=2,cex=.9)
  text(i,-10,state.postal[i],cex=.5)
}
abline(h=0,lty=2)
text(25,8,"Poll Overpredicted Biden Vote Share",cex=1.5)
text(25,-8,"Poll Underpredicted Biden Vote Share",cex=1.5)
dev.off()

png(file = "~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/Fig13Flip_BidenErrorByState.png",
    width = 1000,
    height = 1000)
plot(c(-10,10),c(1,length(state.postal)),
     type = "n",
     main = "Biden Poll - Certified Vote: \n Last Two Weeks",
     axes = FALSE,
     ylab = "",
     xlab = "Signed Error",
     cex.main = 2)
axis(1)
for(i in 1:length(state.postal)){
  state.dat <- subset(dat,dat$Geography == state.postal[i])
  for(j in 1:nrow(state.dat)){
    points(jitter(state.dat$DemError[j]),i,pch = 18,col = "GREY")
  }
  points(state.error[i],i,pch=1,cex = 3)
  text(state.error[i],i,round(state.error[i]),digits = 2,cex = .9)
  text(-10,i,state.postal[i],cex = 1)
}
abline(v = 0,lty = 2)
text(9,25,"Poll Overstated \n Vote Share",cex = 2)
text(-5,25,"Poll Understated \n Vote Share",cex = 2)
dev.off()


state.error <- tapply(dat$DemNormError,dat$Geography,mean)
state.error.indx <- sort(state.error,index.return = TRUE)
state.error <- state.error[state.error.indx$ix]
state.postal <- names(state.error)

png(file = "~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/Fig13FlipB_BidenTwoPartyErrorByState.png",
    width = 1000,
    height = 1000)
plot(c(-10,10),c(1,length(state.postal)),
     type = "n",
     main = "Biden Two-Party Poll - Certified Vote: \n Last Two Weeks",
     axes = FALSE,
     xlab = "",
     ylab = "",
     xlab = "Poll - Vote Margin",
     cex.main = 2)
axis(1)
for(i in 1:length(state.postal)){
  state.dat <- subset(dat,dat$Geography == state.postal[i])
  for(j in 1:nrow(state.dat)){
    points(jitter(state.dat$DemNormError[j]),i,pch = 18,col = "GREY")
  }
  points(state.error[i],i,pch = 1,cex = 3)
  text(state.error[i],i,round(state.error[i]),digits = 2,cex = .9)
  text(-10,i,state.postal[i],cex = 1)
}
abline(v = 0,lty = 2)
text(9,25,"Poll Overstated \n Vote Share",cex = 2)
text(-5,25,"Poll Understated \n Vote Share",cex = 2)
dev.off()

######################################################################################
#   Plot State Error By State: Trump Error
#   THIS IS NEEDED FOR THE SENATE PLOT produced in AnalyzeStateSenGubPolls_FINAL.R
######################################################################################

state.error <- tapply(dat$RepError,dat$Geography,mean)
state.error.indx <- sort(state.error,index.return = TRUE)
state.error <- state.error[state.error.indx$ix]
state.postal <- names(state.error)
trump.state.error <- state.error

dat.bloomberg <- cbind(state.postal,trump.state.error)

write.csv(dat.bloomberg,file = "BloombergDataFig14.csv")


# Figure 12
png(file = "~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/Fig12_TrumpErrorByState.png",
    width = 1000,
    height = 600)

plot(c(1,length(state.postal)),c(-15,5),
     type = "n",
     main = "Trump Poll - Certified Vote: \n Last Two Weeks",
     axes = FALSE,
     xlab = "",
     ylab = "Poll - Vote Margin",
     cex.main = 2)
axis(2)

for(i in 1:length(state.postal)){
  state.dat <- subset(dat,dat$Geography == state.postal[i])
    for(j in 1:nrow(state.dat)){
    points(i,jitter(state.dat$RepError[j]),pch = 18,col = "GREY")
  }
  points(i,state.error[i],pch = 1,cex = 3)
  text(i,state.error[i],round(state.error[i]),digits = 2,cex = .9)
  text(i,-15,state.postal[i],cex = .5)
}
abline(h = 0,lty = 2)
text(25,2,"Poll Overpredicted Trump Vote Share",cex = 1.5)
text(25,-12,"Poll Underpredicted Trump Vote Share",cex = 1.5)
dev.off()

png(file = "~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/Fig12Flip_TrumpErrorByState.png",
    width = 1000,
    height = 1000)
plot(c(-10,10),c(1,length(state.postal)),
     type = "n",
     main = "Trump Poll - Certified Vote: \n Last Two Weeks",
     axes = FALSE,
     ylab = "",
     xlab = "Signed Error",
     cex.main = 2)
axis(1)

for(i in 1:length(state.postal)){
  state.dat <- subset(dat,dat$Geography == state.postal[i])
    for(j in 1:nrow(state.dat)){
    points(jitter(state.dat$RepError[j]),i,pch = 18,col = "GREY")
  }
  points(state.error[i],i,pch = 1,cex = 3)
  text(state.error[i],i,round(state.error[i]),digits = 2,cex = .9)
  text(-10,i,state.postal[i],cex = 1)
}
abline(v = 0,lty = 2)
text(9,25,"Poll Overstated \n Vote Share",cex = 2)
text(-5,25,"Poll Understated \n Vote Share",cex = 2)
dev.off()

state.error <- tapply(dat$RepNormError,dat$Geography,mean)
state.error.indx <- sort(state.error,index.return = TRUE)
state.error <- state.error[state.error.indx$ix]
state.postal <- names(state.error)

png(file = "~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/Fig12FlipB_TrumpTwoPartyErrorByState.png",
    width = 1000,
    height = 1000)
plot(c(-10,10),c(1,length(state.postal)),
     type = "n",
     main = "Trump Two-Party Poll - Certified Vote: \n Last Two Weeks",
     axes = FALSE,
     xlab = "",
     ylab = "",
     xlab = "Poll - Vote Margin",
     cex.main = 2)
axis(1)
for(i in 1:length(state.postal)){
  state.dat <- subset(dat,dat$Geography == state.postal[i])
  for(j in 1:nrow(state.dat)){
    points(jitter(state.dat$RepNormError[j]),i,pch = 18,col = "GREY")
  }
  points(state.error[i],i,pch = 1,cex = 3)
  text(state.error[i],i,round(state.error[i]),digits = 2,cex = .9)
  text(-10,i,state.postal[i],cex = 1)
}
abline(v = 0,lty = 2)
text(9,25,"Poll Overstated \n Vote Share",cex = 2)
text(-5,25,"Poll Understated \n Vote Share",cex = 2)
dev.off()
#################################################################################
######################################################################################
# Regression for State Level Polling Error
######################################################################################
#################################################################################

state.Signed.error <- tapply(dat$SignedError,dat$Geography,mean)
state.Abs.error <- tapply(dat$AbsError,dat$Geography,mean)
state.Dem.error <- tapply(dat$DemError,dat$Geography,mean)
state.Rep.error <- tapply(dat$RepError,dat$Geography,mean)
state.Trump20Vote <- tapply(dat$RepCertVote,dat$Geography,mean)
state.Trump16Vote <- tapply(dat$Trump16,dat$Geography,mean)
state.postal <- as.character(names(state.Trump20Vote))
state.npolls <- tapply(dat$Constant,dat$Geography,sum)

error.dat <- data.frame(cbind(state.postal,state.Trump20Vote,state.Trump16Vote,state.Signed.error,state.Abs.error,state.Dem.error,state.Rep.error,state.npolls))
rownames(error.dat) <- state.postal

StateCovariates <- read_excel("~/Google Drive/AAPOR.PreElectionPolling/Other Data/StateCovariates.xlsx")
rownames(StateCovariates) <- StateCovariates$state.postal
dat.state <- merge(StateCovariates,error.dat,by = "state.postal")
VBMbyState <- read_excel("~/Google Drive/AAPOR.PreElectionPolling/Other Data/VBMbyState.xlsx",sheet = "EarlyPct16Total")
dat.state <- merge(dat.state,VBMbyState,by = "state.postal")

# Create New Variables

# Covid cases per capita

dat.state$COVD.cases.percap <- dat.state$covid.cases/dat.state$population

# Voter File Update in Last Two weeks of October?

dat.state$RecentVFUpdate <- 0
dat.state$RecentVFUpdate[dat.state$MonthUpdate == "10" & as.numeric(dat.state$DateUpdate) > 15] <- 1

dat.state$state.Rep.error <- as.numeric(dat.state$state.Rep.error)
dat.state$state.Dem.error <- as.numeric(dat.state$state.Dem.error)

save(dat.state,
     file = "~/Google Drive/AAPOR.PreElectionPolling/2020 General Election Polling Data/StatePollingErrorData.Rdata")

# Table 6

summary(lm(state.Signed.error ~ as.numeric(state.Trump16Vote) + percent.black + log(population.density) + percent.bachelors + COVD.cases.percap, data = dat.state))
summary(lm(state.Abs.error ~ as.numeric(state.Trump16Vote) + percent.black + log(population.density) + percent.bachelors + COVD.cases.percap, data = dat.state))

# Drop Trump vote
summary(lm(state.Signed.error ~ percent.black + log(population.density) + percent.bachelors + COVD.cases.percap, data = dat.state))
summary(lm(state.Abs.error ~ percent.black + log(population.density) + percent.bachelors + COVD.cases.percap, data = dat.state))

# Missing Data because of CD
mean(dat.state$COVD.cases.percap,na.rm = TRUE)
sd(dat.state$COVD.cases.percap,na.rm = TRUE)
mean(dat.state$percent.black,na.rm = TRUE)
sd(dat.state$percent.black,na.rm = TRUE)
mean(log(dat.state$population.density),na.rm = TRUE)
sd(log(dat.state$population.density),na.rm = TRUE)
mean(dat.state$percent.bachelors,na.rm = TRUE)
sd(dat.state$percent.bachelors,na.rm = TRUE)
mean(as.numeric(state.Trump16Vote),na.rm = TRUE)
sd(as.numeric(state.Trump16Vote),na.rm = TRUE)


#
#     Run regressions on state-level average error
#     Note the number and type of polls will vary so the "all else equal" seems hard here

# Every plausible covariate?
m1 <- lm(state.Signed.error~as.numeric(state.Trump16Vote) + samedayreg + EarlyAsPct2016Total + COVI.rank + population.density + log(median.income) + percent.bachelors + 
           percent.black + percent.over.65 + COVD.cases.percap,
         data = dat.state)

m2 <- lm(as.numeric(state.Abs.error)~as.numeric(state.Trump16Vote) + samedayreg + EarlyAsPct2016Total + COVI.rank + population.density + log(median.income) + percent.bachelors + 
           percent.black + percent.over.65 + COVD.cases.percap,
         data = dat.state)

stargazer(m1,m2,
          type = "html",
          dep.var.labels = c("Signed Error","Absolute Error"),
          covariate.labels = c("Trump 2016 Vote",
                               "Same Day Registration State?","Early Vote as % of 2016 Total",
                               "Cost of Voting Rank","Population Density","log Median Income","% Bachelors","% Black","% Over 65","log(COVID per capita)"),
          digits = 2,
          model.numbers = FALSE,
          out = "~/Google Drive/AAPOR.PreElectionPolling/Writing/StateLevelErrorRegressionFull.html")

######################################################################################################
#   Error by Weighing Choices - Weight by Partisanship or Trump 16 Vote?
#   This is a weird analysis becasue those who weight may be doing so because they have to weight
######################################################################################################

dat$WeightByParty <- NA
dat$PartyID <- as.numeric(dat$PartyID)

dat$Wgt2016 <- dat$`16 Vote`
dat$Wgt2016 <- as.numeric(dat$Wgt2016)

table(dat$Wgt2016)
table(dat$PartyID)

mean(dat$SignedError,na.rm = TRUE)
mean(dat$AbsError,na.rm = TRUE)

dat$ErrorWgt <- NA
dat$ErrorWgt[dat$PartyID == 1 ] <- "Party ID"
dat$ErrorWgt[dat$Wgt2016 == 1] <- "Vote 2016"
dat$ErrorWgt[dat$Wgt2016 == 0 & dat$PartyID == 0] <- "Neither"
dat$ErrorWgt[is.na(dat$Wgt2016)] <- "Missing"

dat$ErrorWgt <- relevel(dat$ErrorWgt,"Party ID")

png(file = "~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/Fig18_WeightingErrors.png",
    width = 1000,
    height = 600)
par(mfrow = c(1,2))
boxplot(dat$SignedError~dat$ErrorWgt,
        xlab = "",
        ylab = "",
        main = "Signed Error",
        cex.main = 2,
        cex.axis = 1.5)
text(seq(1,4),rep(-8,4),tapply(dat$SignedError,dat$ErrorWgt,median),cex = 2)
boxplot(dat$AbsError~dat$ErrorWgt,
        xlab = "",
        ylab = "",
        main = "Absolute Error",
        cex.main = 2,
        cex.axis = 1.5)
text(seq(1,4),rep(0,4),tapply(dat$AbsError,dat$ErrorWgt,median),cex = 2)
dev.off()

# Using Violin plot

lab1 <- round(tapply(dat$SignedError,dat$ErrorWgt,mean),digits = 1)

library(vioplot)
png(file = "~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/Fig18Violin_WeightingErrors",
    width = 1000, 
    height = 800)
par(mfrow = c(1,2))
with(dat,vioplot(
  AbsError[ErrorWgt == "Missing"],AbsError[ErrorWgt == "Neither"],AbsError[ErrorWgt == "Party ID"],AbsError[ErrorWgt == "Vote 2016"],
  names = c("Missing","Neither","Party ID","16 Vote"),
  main = "Absolute Error",
  cex.main = 2,
  cex.axis = 1.25,
  col = "lightgrey"
))
abline(h = mean(dat$AbsError))

text(seq(1,4),rep(0,4),tapply(dat$AbsError,dat$ErrorWgt,median),cex = 2)
with(dat,vioplot(
  SignedError[ErrorWgt == "Missing"],SignedError[ErrorWgt == "Neither"],SignedError[ErrorWgt == "Party ID"],SignedError[ErrorWgt == "Vote 2016"],
  names = c("Missing","Neither","Party ID","16 Vote"),
  main = "Signed Error",
  cex.main = 2,
  cex.axis = 1.25,
  col = "lightgrey"
))
text(seq(1,4),rep(-8,4),tapply(dat$SignedError,dat$ErrorWgt,median),cex = 2)
abline(h = mean(dat$SignedError))

dev.off()

# Polling Error By Partisan Weighting

mean(dat$SignedError[dat$PartyID == 1],na.rm = TRUE)
mean(dat$AbsError[dat$PartyID == 1],na.rm = TRUE)
mean(dat$SignedError[dat$Wgt2016 == 1],na.rm = TRUE)
mean(dat$AbsError[dat$Wgt2016 == 1],na.rm = TRUE)
mean(dat$SignedError[dat$Wgt2016 == 0 & dat$PartyID == 0],na.rm = TRUE)
mean(dat$AbsError[dat$Wgt2016 == 0 & dat$PartyID == 0],na.rm = TRUE)

mean(dat$AbsError[is.na(dat$Wgt2016)],na.rm = TRUE)
mean(dat$SignedError[is.na(dat$Wgt2016)],na.rm = TRUE)
