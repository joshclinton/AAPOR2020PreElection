#########################################################################
#########################################################################
# AAPOR Task Force on 2020 Pre-Election Polls: 
# Performance of State Senatorial & Gubernatorial Polls
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

dat1 <- read_excel("~/Google Drive/AAPOR.PreElectionPolling/2020 General Election Polling Data/SenGovPolling.xlsx", sheet = "Senate")
dat2 <- read_excel("~/Google Drive/AAPOR.PreElectionPolling/2020 General Election Polling Data/SenGovPolling.xlsx", sheet = "Governor")

dat1$type <- "Senate"
dat2$type <- "Gov"

dat <- rbind(dat1,dat2)


dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("11/3/2020", "%m/%d/%Y")   
dat$DaysToElection <-  dat$EndDate - election.day

# Recode missing
dat[dat==-99]<-NA

# Calculate Margin of Error based on sample size if missing

dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/as.numeric(dat$SampleSize))*1.96
dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]


# Create measures of polling error
###############################################################################

# Signed Error: Certified Vote
dat$SignedError <- (dat$Dem - dat$Rep) - (dat$DemCertVote-dat$RepCertVote)

# Absolute Error: Certified Vote
dat$AbsError <-  abs(dat$SignedError)

# Partisan Error
dat$RepError <- dat$Rep - dat$RepCertVote
dat$DemError <- dat$Dem - dat$DemCertVote 

# Percent Correctly Predicted
dat$DemWinPoll <- as.logical(dat$Dem > dat$Rep)
dat$PollWin <- NA
dat$PollWin[dat$Dem > dat$Rep] <- "Dem"
dat$PollWin[dat$Dem < dat$Rep] <- "Rep"
dat$PollWin[dat$Dem == dat$Rep] <- "Tie"
dat$CorrectWinner <- as.logical(dat$PollWin == dat$Winner)
dat$AbsPollMargin <- abs(dat$Dem - dat$Rep)
nrow(dat)
length(table(dat$Conducted))

dat$DK <- 100 - dat$Dem - dat$Rep
mean(dat$DK,na.rm=TRUE)

############################################################################################
##    Last 2 weeks only
############################################################################################

dat <- subset(dat,dat$DaysToElection > -14)
mean(dat$DK,na.rm=TRUE)
nrow(dat)
length(table(dat$Conducted))

# Used to compare pres errors to sen/gub errors
state.with.sengub <- unique(dat$state)

mean(dat$SignedError,na.rm=TRUE)
mean(dat$AbsError,na.rm=TRUE)

median(dat$SignedError,na.rm=TRUE)
median(dat$AbsError,na.rm=TRUE)

mean(dat$RepError, na.rm=TRUE)
mean(dat$DemError, na.rm=TRUE)
mean(dat$CorrectWinner)
mean(dat$poll.predicted[dat$AbsPollMargin>=1])
mean(dat$poll.predicted[dat$AbsPollMargin>=5])
nrow(dat)


dat$PollMargin20 <- abs(dat$Dem - dat$Rep)
dat$Margin20 <- abs(dat$DemCertVote - dat$RepCertVote)
mean(dat$poll.predicted,na.rm=TRUE)
length(dat$poll.predicted)
mean(dat$poll.predicted[dat$PollMargin20 > dat$MoE],na.rm=TRUE)
length(dat$poll.predicted[dat$PollMargin20 > dat$MoE])
mean(dat$poll.predicted[dat$Margin20 > dat$MoE],na.rm=TRUE)
length(dat$poll.predicted[dat$Margin20 > dat$MoE])

mean(dat$poll.predicted[dat$PollMargin20 > 2*dat$MoE],na.rm=TRUE)
length(dat$poll.predicted[dat$PollMargin20 > 2*dat$MoE])
mean(dat$poll.predicted[dat$Margin20 > 2*dat$MoE],na.rm=TRUE)
length(dat$poll.predicted[dat$Margin20 > 2*dat$MoE])


dat.sen <- subset(dat,dat$type=="Senate")
mean(dat.sen$SignedError,na.rm=TRUE)
mean(dat.sen$AbsError,na.rm=TRUE)
mean(dat.sen$RepError, na.rm=TRUE)
mean(dat.sen$DemError, na.rm=TRUE)
nrow(dat.sen)

dat.sen$DK <- 100 - dat.sen$Dem - dat.sen$Rep
mean(dat$DK,na.rm=TRUE)

mean(dat$MoE)


mean(dat.sen$poll.predicted,na.rm=TRUE)
length(dat.sen$poll.predicted)
mean(dat.sen$poll.predicted[dat.sen$PollMargin20 > dat.sen$MoE],na.rm=TRUE)
length(dat.sen$poll.predicted[dat.sen$PollMargin20 > dat.sen$MoE])
mean(dat.sen$poll.predicted[dat.sen$Margin20 > dat.sen$MoE],na.rm=TRUE)
length(dat.sen$poll.predicted[dat.sen$Margin20 > dat.sen$MoE])

mean(dat.sen$poll.predicted[dat.sen$PollMargin20 > 2*dat.sen$MoE],na.rm=TRUE)
length(dat.sen$poll.predicted[dat.sen$PollMargin20 > 2*dat.sen$MoE])
mean(dat.sen$poll.predicted[dat.sen$Margin20 > 2*dat.sen$MoE],na.rm=TRUE)
length(dat.sen$poll.predicted[dat.sen$Margin20 > 2*dat.sen$MoE])


dat.gov <- subset(dat,dat$type=="Gov")
mean(dat.gov$SignedError,na.rm=TRUE)
mean(dat.gov$AbsError,na.rm=TRUE)
mean(dat.gov$RepError, na.rm=TRUE)
mean(dat.gov$DemError, na.rm=TRUE)
mean(dat.gov$CorrectWinner)
mean(dat.gov$CorrectWinner[dat.gov$AbsPollMargin>=1])
mean(dat.gov$CorrectWinner[dat.gov$AbsPollMargin>=5])
nrow(dat.gov)

mean(dat.gov$poll.predicted,na.rm=TRUE)
length(dat.gov$poll.predicted)
mean(dat.gov$poll.predicted[dat.gov$PollMargin20 > dat.gov$MoE],na.rm=TRUE)
length(dat.gov$poll.predicted[dat.gov$PollMargin20 > dat.gov$MoE])
mean(dat.gov$poll.predicted[dat.gov$Margin20 > dat.gov$MoE],na.rm=TRUE)
length(dat.gov$poll.predicted[dat.gov$Margin20 > dat.gov$MoE])

mean(dat.gov$poll.predicted[dat.gov$PollMargin20 > 2*dat.gov$MoE],na.rm=TRUE)
length(dat.gov$poll.predicted[dat.gov$PollMargin20 > 2*dat.gov$MoE])
mean(dat.gov$poll.predicted[dat.gov$Margin20 > 2*dat.gov$MoE],na.rm=TRUE)
length(dat.gov$poll.predicted[dat.gov$Margin20 > 2*dat.gov$MoE])

############################################################################################
##    Last week only
############################################################################################

dat2 <- subset(dat,dat$DaysToElection >= -7)
nrow(dat2)
length(table(dat2$Conducted))
mean(dat2$DK,na.rm=TRUE)

# Last 3 Days
dat3 <- subset(dat2,dat2$DaysToElection >= -3)
nrow(dat3)
length(table(dat3$Conducted))
mean(dat3$DK,na.rm=TRUE)


png(file="~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/Fig6_ SenGubSignAbsSummary.png",width=900,height=600)
par(mfrow=c(3,2))
hist(dat$SignedError,main="Last Two Weeks",xlab="Signed Error on Margin",xlim=c(-35,35),breaks=seq(-35,35,by=3),cex.lab=1.5,cex.main=2)
abline(v=mean(dat$SignedError))
abline(v=0,lwd=2)
text(-20,30,"Margin Too \n Republican",cex=2)
text(20,30,"Margin Too \n Democratic",cex=2)
hist(dat$AbsError,xlab="Absolute Error on Margin",main="Last Two Weeks",xlim=c(0,40),breaks=seq(0,39,by=3),cex.lab=1.5,cex.main=2)
abline(v=mean(dat$AbsError))
hist(dat2$SignedError,main="Last Week",xlab="Signed Error on Margin",xlim=c(-35,35),breaks=seq(-35,35,by=3),cex.lab=1.5,cex.main=2)
abline(v=mean(dat2$SignedError))
text(-20,15,"Margin Too \n Republican",cex=2)
text(20,15,"Margin Too \n Democratic",cex=2)
abline(v=0,lwd=2)
hist(dat2$AbsError,xlab="Absolute Error on Margin",main="Last Week",xlim=c(0,40),breaks=seq(0,39,by=3),cex.lab=1.5,cex.main=2)
abline(v=mean(dat2$AbsError))
hist(dat3$SignedError,main="Last 3 Days",xlab="Signed Error on Margin",xlim=c(-35,35),breaks=seq(-35,35,by=3),cex.lab=1.5,cex.main=2)
abline(v=mean(dat3$SignedError))
abline(v=0,lwd=2)
text(-20,10,"Margin Too \n Republican",cex=2)
text(20,10,"Margin Too \n Democratic",cex=2)
hist(dat3$AbsError,xlab="Absolute Error on Margin",main="Last 3 Days",xlim=c(0,40),breaks=seq(0,39,by=3),cex.lab=1.5,cex.main=2)
abline(v=mean(dat3$AbsError))
dev.off()


########################################################################
#   Plot State Error By State: Republican Senate Error
#########################################################################

# NOTE: Need to to run the Presidential State Level analysis code

state.error <- tapply(dat$RepError,dat$state,mean)
state.error.indx <- sort(state.error,index.return=TRUE)

state.error <- state.error[state.error.indx$ix]
state.name <- names(state.error)

repsen.state.error <- tapply(dat$RepError,dat$state,mean)
repsen.state.signed.error <- tapply(dat$SignedError,dat$state,mean)

##################################################################
# Compare Errors: Trump vs. Republican Senate Candidate
##################################################################

trump.state.error.indx <- sort(trump.state.error,index.return=TRUE)
trump.state.error <- trump.state.error[trump.state.error.indx$ix]
trump.state.name <- names(trump.state.error)

tmerge <- cbind(trump.state.name,trump.state.error)
colnames(tmerge) <- c("state","TrumpError")
rmerge <- cbind(names(repsen.state.error),repsen.state.error)
colnames(rmerge) <- c("state","RepSenError")

m.dat <- merge(tmerge,rmerge,sort=FALSE,all=TRUE)

mean(as.numeric(m.dat$TrumpError)-as.numeric(m.dat$RepSenError),na.rm=TRUE)

mean(as.numeric(m.dat$TrumpError))
mean(as.numeric(m.dat$RepSenError),na.rm=TRUE)

m.dat.both <- m.dat[1:29,]
m.dat.pres <- m.dat[30:49,]

i.sort <- sort(as.numeric(m.dat$TrumpError)[31:53],decreasing=FALSE,index.return=TRUE)
indx2 <- i.sort$ix + 30

indx <- c(seq(1,30),indx2)

m.dat <- m.dat[indx,]
m.dat

# Figure 10

png(file="~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/CompareTrumpRepSenateErrorByState.png",width=960,height=480)
plot(m.dat$TrumpError,seq(1,nrow(m.dat)),xlim=c(-15,5),axes=FALSE,xlab="",ylab="Poll - Certified Vote",main="Trump vs. Republican Senate Polling Error Compared")
axis(1)
for(i in 1:nrow(m.dat)){
  points(m.dat$RepSenError[i],i,pch=18)
  text(-15,1,m.dat$state[i],cex=.5)
}
abline(v=30.5)
abline(v=0,lty=2)
text(-13,13,"Senate Contest \n in State")
text(-13,40," No Senate Contest \n in State")
legend(43,5,pch=c(1,18),legend=c("Trump Error","Rep. Error"))
dev.off()


##################################################################
# Compare Errors: Signed Pres  vs. Signed Senate Candidate
##################################################################

signed.state.error.indx <- sort(signed.state.error,index.return=TRUE)
signed.state.error <- signed.state.error[signed.state.error.indx$ix]
signed.state.name <- names(signed.state.error)

tmerge <- cbind(signed.state.name,signed.state.error)
colnames(tmerge) <- c("state","SignedPresError")
rmerge <- cbind(names(repsen.state.signed.error),repsen.state.signed.error)
colnames(rmerge) <- c("state","SignedSenError")

m.dat <- merge(tmerge,rmerge,sort=FALSE,all=TRUE)

mean(as.numeric(m.dat$SignedPresError)-as.numeric(m.dat$SignedSenError),na.rm=TRUE)

mean(as.numeric(m.dat$SignedPresError))
mean(as.numeric(m.dat$SignedSenError),na.rm=TRUE)

m.dat.both <- m.dat[1:30,]
m.dat.pres <- m.dat[31:53,]

i.sort <- sort(as.numeric(m.dat$SignedPresError)[31:53],decreasing=FALSE,index.return=TRUE)
indx2 <- i.sort$ix + 30

indx <- c(seq(1,30),indx2)

m.dat <- m.dat[indx,]
m.dat

# Figure 10

png(file="~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/CompareSignedPresSenateErrorByState.png",width=1000,height=600)
plot(m.dat$SignedPresError,ylim=c(-5,15),axes=FALSE,xlab="",ylab="Poll - Certified Vote",main="Trump vs. Republican Senate Polling Error Compared")
axis(2)
for(i in 1:nrow(m.dat)){
  points(i,m.dat$SignedSenError[i],pch=18)
  text(i,-15,m.dat$state[i],cex=.5)
}
abline(v=30.5)
abline(h=0,lty=2)
text(13,15,"Senate Contest \n in State")
text(45,15,"No Senate Contest \n in State")
text(13,1,"Poll Margin \n Too Democratic")
text(13,-1,"Poll Margin \n Too Republican")
text(45,1,"Poll Margin \n Too Democratic")
text(45,-1,"Poll Margin \n Too Republican")

legend(23,-2,pch=c(1,18),legend=c("Trump Error","Rep. Error"))
dev.off()


# Figure 10 Flip

png(file="~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/Fig10Flip_CompareSignedPresSenateErrorByState.png",width=1000,height=1000)
plot(m.dat$SignedPresError,seq(1,nrow(m.dat)),xlim=c(-5,15),axes=FALSE,ylab="",xlab="Poll - Certified Vote",main="Trump vs. Republican Signed Polling Errors",cex.main=2)
axis(1)
for(i in 1:nrow(m.dat)){
  points(m.dat$SignedSenError[i],i,pch=18)
  text(-5,i,m.dat$state[i],cex=1)
}
abline(h=30.5)
abline(v=0,lty=2)
text(13,28,"Senate Contest \n in State",cex=2)
text(13,33,"No Senate Contest \n in State",cex=2)
text(2,13,"Poll Margin \n Too Democratic",cex=2)
text(-2,13,"Poll Margin \n Too Republican",cex=2)
dev.off()



######################################################################################
##        Error by Collapsed Mode
##        NOTE: THE MODE CATEGORIES ARE NOT AS UPDATED AS THE PRES ONES
######################################################################################

table(dat$Mode)

# Only if more than 10
dat$Mode[dat$Mode=="IVR/Text"] <-"Misc."
dat$Mode[dat$Mode=="Phone/Online"] <- "Misc."
dat$Mode[dat$Mode=="Text"] <- "Misc."
dat$Mode[dat$Mode=="IVR"] <- "Misc."
dat$Mode[dat$Mode=="NA"] <- "Misc."

table(dat$Mode)

dat.nomisc <- dat[-seq(1,nrow(dat))*as.logical(dat$Mode=="Misc." | is.na(dat$Mode)), ]

png(file="~/Google Drive/AAPOR.PreElectionPolling/Writing/Figures/SignedErrorByModeSenGub.png",width=1000,height=600)
boxplot(dat.nomisc$SignedError~dat.nomisc$Mode,ylim=c(-10,20),ylab="(Biden Poll - Trump Poll) - (Biden Vote - Trump Vote)",xlab="",main="Signed Error by Mode Gubernatorial & Senatorial:\n Last Two Weeks")
text(seq(1,5),rep(-10,times=5),
     round(tapply(dat.nomisc$SignedError,dat.nomisc$Mode,mean),digits=1),cex=1.5)
abline(h=0,lty=2)
dev.off()

#############################################################
#   Plot State Error By State: Democratic Sen/Gub Error
#############################################################

state.error <- tapply(dat$DemError,dat$state,mean)
state.error.indx <- sort(state.error,index.return=TRUE)
state.error <- state.error[state.error.indx$ix]
state.name <- names(state.error)

demsen.state.error <- tapply(dat$DemError,dat$state,mean)

png(file="Google Drive/AAPOR.PreElectionPolling/Writing/Figures/DemSenateErrorByState.png",width=1000,height=600)
plot(c(1,length(state.name)),c(-15,10),type="n",main="Dem Senate Poll - Vote: \n Last Two Weeks",axes=FALSE,xlab="",ylab="Poll - Vote Margin")
axis(2)
for(i in 1:length(state.name)){
  state.dat <- subset(dat,dat$state==state.name[i])
  for(j in 1:nrow(state.dat)){
    points(i,jitter(state.dat$DemError[j]),pch=18,col="GREY")
  }
  points(i,state.error[i],pch=1,cex=3)
  text(i,state.error[i],round(state.error[i]),digits=2,cex=.9)
  text(i,-15,state.name[i],cex=.5)
}
abline(h=0,lty=2)
text(15,-12,"Poll Underpredicted Democratic Candidate Vote Share",cex=1.5)
text(15,8,"Poll Overpredicted Democratic Candidate Vote Share",cex=1.5)
dev.off()

########################################################################
#   Plot State Error By State: Republican Sen/Gub Error
#########################################################################

# NOTE: Need to to run the Presidential State Level analysis code

state.error <- tapply(dat$RepError,dat$state,mean)
state.error.indx <- sort(state.error,index.return=TRUE)
state.error <- state.error[state.error.indx$ix]
state.name <- names(state.error)

repsen.state.error <- tapply(dat$RepError,dat$state,mean)

png(file="Google Drive/AAPOR.PreElectionPolling/Writing/Figures/RepSenateErrorByState.png",width=1000,height=600)
plot(c(1,length(state.name)),c(-25,10),type="n",main="Rep Senate Poll - Vote Margin: \n Last Two Weeks",axes=FALSE,xlab="",ylab="Poll - Vote Margin")
axis(2)
for(i in 1:length(state.name)){
  state.dat <- subset(dat,dat$state==state.name[i])
  for(j in 1:nrow(state.dat)){
    points(i,jitter(state.dat$RepError[j]),pch=18,col="GREY")
  }
  points(i,state.error[i],pch=1,cex=3)
  text(i,state.error[i],round(state.error[i]),digits=2,cex=.9)
  text(i,-25,state.name[i],cex=.5)
}
abline(h=0,lty=2)
text(15,-12,"Poll Underpredicted Republican Candidate Vote Share",cex=1.5)
text(15,5,"Poll Overpredicted Republican Candidiate Vote Share",cex=1.5)
dev.off()

##################################################################
# Compare Errors: Trump vs. Republican Senate Candidate
##################################################################

trump.state.error.indx <- sort(trump.state.error,index.return=TRUE)
trump.state.error <- trump.state.error[trump.state.error.indx$ix]
trump.state.name <- names(trump.state.error)

tmerge <- cbind(trump.state.name,trump.state.error)
colnames(tmerge) <- c("state","TrumpError")
rmerge <- cbind(names(repsen.state.error),repsen.state.error)
colnames(rmerge) <- c("state","RepSenError")

m.dat <- merge(tmerge,rmerge,sort=FALSE,all=TRUE)

mean(as.numeric(m.dat$TrumpError)-as.numeric(m.dat$RepSenError),na.rm=TRUE)

pdf(file="Google Drive/AAPOR.PreElectionPolling/Writing/Figures/CompareTrumpRepSenateErrorByState.pdf",width=15,height=7)
plot(m.dat$TrumpError,ylim=c(-15,5),axes=FALSE,xlab="",ylab="Poll - Vote",main="Trump vs. Republican Senate Polling Error Compared")
axis(2)
for(i in 1:nrow(m.dat)){
  points(i,m.dat$RepSenError[i],pch=18)
  text(i,-15,m.dat$state[i],cex=.5)
  }
abline(v=(1.5 + nrow(tmerge) - nrow(rmerge)))
abline(h=0,lty=2)
text(13,-13,"Senate Contest \n in State")
text(40,-13," No Senate Contest \n in State")
legend(43,5,pch=c(1,18),legend=c("Trump Error","Republican Error"))
dev.off()
