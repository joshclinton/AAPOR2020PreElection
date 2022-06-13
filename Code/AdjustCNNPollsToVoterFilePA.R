##############################################################################
##  Analyze CNN Pennsylvania Data
##  Use Certified Vote and Vote by Mode by County Party Strata
##  Reweight Poll to match true mode and vote by county strata
##  Examine impact on Demographics
##
##  Josh Clinton
##  Vanderbilt University
##  March 2021
##############################################################################

#install.packages("sjlabelled")
library(sjlabelled)
library(stargazer)
library(haven)
library(questionr)

CNN_PA <- read_spss("Google Drive/AAPOR.PreElectionPolling/Other Data/2020 General State Level Polls/CNN_Final Datasets/CNN_PA.sav")
CNN_PA$fips_code <- CNN_PA$merge_fips
CNN_PA$weight <- CNN_PA$WEIGHT

# Get voter file data
# NOTE: NO EARLY VOTE IN PA SO ALL OF THOSE VARIABLES ARE COMMENTED OUT

PA_Mode <- read.csv("Google Drive/AAPOR.PreElectionPolling/Other Data/StateCertifiedVoteTotalsMode/PA_TargetSmart_PartyStrataVote_2020.csv")
PA_Vote <- read.csv("Google Drive/AAPOR.PreElectionPolling/Other Data/StateCertifiedVoteTotalsMode/PA_NEAT_PartyStrataVote_2020.csv")

# Get NEAT data for county stratification
pa_ctyvote <- read.csv("Google Drive/AAPOR.PreElectionPolling/Other Data/StateCertifiedVoteTotalsMode/PA_NEAT_CountyVote_2020.csv")
pa_ctyvote$county_name <- tolower(pa_ctyvote$county_name)

# Extract information
pa_ctyinfo <- pa_ctyvote[,c("fips_code","county_name","party_stratum","party_stratum_name","geo_stratum","geo_stratum_name")]

Trump.Vote.Strata <- PA_Vote[,c("party_stratum","Trump")]
Biden.Vote.Strata <- PA_Vote[,c("party_stratum","Biden")]
Absentee.Vote.Strata <- PA_Mode[,c("party_stratum","Absentee")]
ElectionDay.Vote.Strata <- PA_Mode[,c("party_stratum","Election.Day")]
#Early.Vote.Strata <- PA_Mode[,c("party_stratum","Early")]
TotalVote <- sum(PA_Vote$Trump+PA_Vote$Biden)
TotalVote2 <- sum(PA_Mode$Absentee+PA_Mode$Election.Day)

# Define proportion of electorate in each cell

#Early.Vote.Strata <- Early.Vote.Strata[,2]/TotalVote
Absentee.Vote.Strata <- Absentee.Vote.Strata[,2]/TotalVote
ElectionDay.Vote.Strata <- ElectionDay.Vote.Strata[,2]/TotalVote
Trump.Vote.Strata <- Trump.Vote.Strata[,2]/TotalVote
Biden.Vote.Strata <- Biden.Vote.Strata[,2]/TotalVote

#################################
#   Now use survey data
##################################

# Merge strata onto CNN
CNN_PA$fips_code <- CNN_PA$merge_fips
nobs1 <- nrow(CNN_PA)

CNN_PA <- merge(CNN_PA,pa_ctyinfo,by="fips_code")
#CNN_PA$party_stratum <- CNN_PA$party_stratum.x
nobs2 <- nrow(CNN_PA)

nobs1-nobs2 # observations lost in merge

# LIKELY VOTERS

CNN_PA <- CNN_PA[CNN_PA$LV==1,]
nrow(CNN_PA)

#Poll.Early.Vote.Strata <- NULL
Poll.Absentee.Vote.Strata <- NULL
Poll.ElectionDay.Vote.Strata <- NULL
Poll.Trump.Vote.Strata <- NULL
Poll.Biden.Vote.Strata <- NULL

# NO EARLY VOTE, BUT LV WHO SAID THEY WOULD VOTE EARLY ARE TREATED AS ABSENTEE

prop.table(table(CNN_PA$presvote[CNN_PA$votemode==3]))
prop.table(table(CNN_PA$presvote[CNN_PA$votemode==2]))

CNN_PA$EDVote <- ifelse(CNN_PA$votemode==1,1,0)
#CNN_PA$EarlyVote <- ifelse(CNN_PA$votemode==3,1,0)
CNN_PA$Absentee <- ifelse(CNN_PA$votemode==2 | CNN_PA$votemode==3,1,0)

for(i in 1:5){
  datsub <- subset(CNN_PA,party_stratum==i)
  BidenVote <- sum(as.logical(datsub$presvote==1)*datsub$weight,na.rm=TRUE)
  TrumpVote <- sum(as.logical(datsub$presvote==2)*datsub$weight,na.rm=TRUE)
  EDVote <- sum(datsub$EDVote*datsub$weight,na.rm=TRUE)
 # EarlyVote <- sum(datsub$EarlyVote*datsub$weight,na.rm=TRUE)
  AbsenteeVote <- sum(datsub$Absentee*datsub$weight,na.rm=TRUE)

#  Poll.Early.Vote.Strata[i] <- EarlyVote
  Poll.Absentee.Vote.Strata[i] <- AbsenteeVote
  Poll.ElectionDay.Vote.Strata[i] <- EDVote
  Poll.Trump.Vote.Strata[i] <- TrumpVote
  Poll.Biden.Vote.Strata[i] <- BidenVote
}

# Normalize into a proportion
# THIS ONLY USES THOSE SUPPORTING TRUMP OR BIDEN SO SUMS TO 1, BUT POLL MARGINAL ONLY SUMS TO .93
# Could also sum to weight

TotalN <- sum(Poll.Trump.Vote.Strata + Poll.Biden.Vote.Strata)

#Poll.Early.Vote.Strata <- Poll.Early.Vote.Strata/TotalN
Poll.Absentee.Vote.Strata <- Poll.Absentee.Vote.Strata/TotalN
Poll.ElectionDay.Vote.Strata <- Poll.ElectionDay.Vote.Strata/TotalN
Poll.Trump.Vote.Strata <- Poll.Trump.Vote.Strata/TotalN
Poll.Biden.Vote.Strata <- Poll.Biden.Vote.Strata/TotalN 

AbsenteeSizeError <- Poll.Absentee.Vote.Strata  - Absentee.Vote.Strata 
ElectionDaySizeError <- Poll.ElectionDay.Vote.Strata  - ElectionDay.Vote.Strata 
#EarlySizeError <- Poll.Early.Vote.Strata  - Early.Vote.Strata 
TrumpError <- Poll.Trump.Vote.Strata - Trump.Vote.Strata
BidenError <- Poll.Biden.Vote.Strata - Biden.Vote.Strata

Size <- round(cbind(Poll.Absentee.Vote.Strata,Poll.ElectionDay.Vote.Strata,AbsenteeSizeError,ElectionDaySizeError),digits=3)
Vote <- round(cbind(Poll.Trump.Vote.Strata,Poll.Biden.Vote.Strata,TrumpError,BidenError),digits=3)


################################################################################################
#       Create Weights: Weight to Certified Vote By Mode by Strata
#################################################################################################

EDWgt <- ElectionDay.Vote.Strata /Poll.ElectionDay.Vote.Strata
AbsWgt <- Absentee.Vote.Strata /Poll.Absentee.Vote.Strata
#EarlyWgt <- Early.Vote.Strata/Poll.Early.Vote.Strata

CNN_PA$ModeWgt <- NA

for(i in 1:5){
  CNN_PA$ModeWgt[CNN_PA$party_stratum==i & CNN_PA$EDVote==1] <- EDWgt[i]
#  CNN_PA$ModeWgt[CNN_PA$party_stratum==i & CNN_PA$EarlyVote==1] <- EarlyWgt[i]
  CNN_PA$ModeWgt[CNN_PA$party_stratum==i & CNN_PA$Absentee==1] <- AbsWgt[i]
}

# How correlated are these?  
cor(CNN_PA$weight,CNN_PA$ModeWgt,use="pairwise.complete")

#
#     Now create a new weight that is the old weight X mode weight
#     GOAL: How does weighting by mode by strata affect accuracy?
#

CNN_PA$WeightModeWgt <- CNN_PA$ModeWgt*CNN_PA$weight

Poll2.Absentee.Vote.Strata <- NULL
#Poll2.Early.Vote.Strata <- NULL
Poll2.ElectionDay.Vote.Strata <- NULL
Poll2.Trump.Vote.Strata <- NULL
Poll2.Biden.Vote.Strata <- NULL

for(i in 1:5){
  datsub <- subset(CNN_PA,party_stratum==i)
  BidenVote2 <- sum(as.logical(datsub$presvote==1)*datsub$WeightModeWgt,na.rm=TRUE)
  TrumpVote2 <- sum(as.logical(datsub$presvote==2)*datsub$WeightModeWgt,na.rm=TRUE)
  EDVote2 <- sum(datsub$EDVote*datsub$WeightModeWgt,na.rm=TRUE)
#  EarlyVote2 <- sum(datsub$EarlyVote*datsub$WeightModeWgt,na.rm=TRUE)
  AbsVote2 <- sum(datsub$Absentee*datsub$WeightModeWgt,na.rm=TRUE)
  
# Poll2.Early.Vote.Strata[i] <- EarlyVote2
  Poll2.Absentee.Vote.Strata[i] <- AbsVote2
  Poll2.ElectionDay.Vote.Strata[i] <- EDVote2
  Poll2.Trump.Vote.Strata[i] <- TrumpVote2
  Poll2.Biden.Vote.Strata[i] <- BidenVote2
}

TotalN <- sum(Poll2.Trump.Vote.Strata + Poll2.Biden.Vote.Strata)

#Poll2.Early.Vote.Strata <- Poll2.Early.Vote.Strata/TotalN
Poll2.Absentee.Vote.Strata <- Poll2.Absentee.Vote.Strata/TotalN
Poll2.ElectionDay.Vote.Strata <- Poll2.ElectionDay.Vote.Strata/TotalN
Poll2.Trump.Vote.Strata <- Poll2.Trump.Vote.Strata/TotalN
Poll2.Biden.Vote.Strata <- Poll2.Biden.Vote.Strata/TotalN 

################################################################################################
#       Evaluate the Effect
#################################################################################################

ElectionDaySizeError2 <- Poll2.ElectionDay.Vote.Strata  - ElectionDay.Vote.Strata 
#EarlySizeError2 <- Poll2.Early.Vote.Strata  - Early.Vote.Strata
AbsSizeError2 <- Poll2.Absentee.Vote.Strata  - Absentee.Vote.Strata  
TrumpError2 <- Poll2.Trump.Vote.Strata - Trump.Vote.Strata
BidenError2 <- Poll2.Biden.Vote.Strata - Biden.Vote.Strata

# Trump Error Impact from weighting on Mode
sum(Poll2.Trump.Vote.Strata) - sum(Poll.Trump.Vote.Strata)

# Biden Error Impact from weighting on Mode
sum(Poll2.Biden.Vote.Strata) - sum(Poll.Biden.Vote.Strata)

################################################################################################
#       Create Weights: Now Weight mode-weighted data to the Outcome
#################################################################################################

TrumpWgt <- Trump.Vote.Strata /Poll2.Trump.Vote.Strata
BidenWgt <- Biden.Vote.Strata /Poll2.Biden.Vote.Strata

CNN_PA$VoteWgt <- NA

for(i in 1:5){
  CNN_PA$VoteWgt[CNN_PA$party_stratum==i & CNN_PA$presvote==1] <- BidenWgt[i]
  CNN_PA$VoteWgt[CNN_PA$party_stratum==i & CNN_PA$presvote==2] <- TrumpWgt[i]
}

# How correlated are these?  
cor(CNN_PA$weight,CNN_PA$VoteWgt,use="pairwise.complete")

CNN_PA$WeightVoteModeWgt <- CNN_PA$VoteWgt*CNN_PA$WeightModeWgt

Poll3.Absentee.Vote.Strata <- NULL
#Poll3.Early.Vote.Strata <- NULL
Poll3.ElectionDay.Vote.Strata <- NULL
Poll3.Trump.Vote.Strata <- NULL
Poll3.Biden.Vote.Strata <- NULL

for(i in 1:5){
  datsub <- subset(CNN_PA,party_stratum==i)
  BidenVote3 <- sum(as.logical(datsub$presvote==1)*datsub$WeightVoteModeWgt,na.rm=TRUE)
  TrumpVote3 <- sum(as.logical(datsub$presvote==2)*datsub$WeightVoteModeWgt,na.rm=TRUE)
  EDVote3 <- sum(datsub$EDVote*datsub$WeightVoteModeWgt,na.rm=TRUE)
#  EarlyVote3 <- sum(datsub$EarlyVote*datsub$WeightVoteModeWgt,na.rm=TRUE)
  AbsVote3 <- sum(datsub$Absentee*datsub$WeightVoteModeWgt,na.rm=TRUE)
  
  Poll3.Absentee.Vote.Strata[i] <- AbsVote3
#  Poll3.Early.Vote.Strata[i] <- EarlyVote3
  Poll3.ElectionDay.Vote.Strata[i] <- EDVote3
  Poll3.Trump.Vote.Strata[i] <- TrumpVote3
  Poll3.Biden.Vote.Strata[i] <- BidenVote3
}

TotalN <- sum(Poll3.Trump.Vote.Strata + Poll3.Biden.Vote.Strata)

Poll3.Absentee.Vote.Strata <- Poll3.Absentee.Vote.Strata/TotalN
#Poll3.Early.Vote.Strata <- Poll3.Early.Vote.Strata/TotalN
Poll3.ElectionDay.Vote.Strata <- Poll3.ElectionDay.Vote.Strata/TotalN
Poll3.Trump.Vote.Strata <- Poll3.Trump.Vote.Strata/TotalN
Poll3.Biden.Vote.Strata <- Poll3.Biden.Vote.Strata/TotalN 

################################################################################################
#       Evaluate the Effect
#       Should be no vote error now
#################################################################################################

ElectionDaySizeError3 <- Poll3.ElectionDay.Vote.Strata  - ElectionDay.Vote.Strata
#EarlySizeError3 <- Poll3.Early.Vote.Strata  - Early.Vote.Strata
AbsSizeError3 <- Poll3.Absentee.Vote.Strata  - Absentee.Vote.Strata 
TrumpError3 <- Poll3.Trump.Vote.Strata - Trump.Vote.Strata
BidenError3 <- Poll2.Biden.Vote.Strata - Biden.Vote.Strata

sum(TrumpError3)
sum(BidenError3)

sum(Poll3.Trump.Vote.Strata)
sum(Poll3.Biden.Vote.Strata)

# Trump Error Impact from weighting on Vote And Mode
sum(Poll3.Trump.Vote.Strata) - sum(Poll.Trump.Vote.Strata)

# Biden Error Impact from weighting on Mode
sum(Poll3.Biden.Vote.Strata) - sum(Poll.Biden.Vote.Strata)

# Election Day Error Impact from weighting on Vote And Mode
sum(Poll3.ElectionDay.Vote.Strata) - sum(ElectionDay.Vote.Strata)
sum(Poll3.Absentee.Vote.Strata) - sum(Absentee.Vote.Strata)
#sum(Poll3.Early.Vote.Strata) - sum(Early.Vote.Strata)


############################################################
#     NOW LOOK AT IMPACT ON DEMOGRAPHICS
#     Plot the change in proportions
#############################################################

png(file="Google Drive/AAPOR.PreElectionPolling/Writing/Figures/CNNPAdjust.png",width=600, height=900)
a <- -10
b <- 5
ncompare <- 28
plot(seq(1,ncompare),seq(1,ncompare),xlim=c(a,b),type="n",axes=FALSE,ylab="",
     xlab="Change in % After Reweighting",
     main="PA: Effect of Reweighting by Vote and Mode by County Strata")
axis(1)
abline(v=seq(-5,5,by=1),lty=2,col="GREY")
abline(v=0)

# White/Non-White
datsub <- na.omit(as.data.frame(cbind(CNN_PA$white,CNN_PA$weight,CNN_PA$WeightVoteModeWgt)))
colnames(datsub) <- c("white","OrigWgt","VoteModeWgt")
wtd.table(datsub$white,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt)
wtd.table(datsub$white,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
wtd.table(datsub$white,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$white,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)

tb <- wtd.table(datsub$white,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$white,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
tb <- tb*100
labt <- get_labels(CNN_PA$white)
points(as.numeric(tb),seq(1,3),pch=18)
text(rep(a,times=length(labt)),seq(1,3),labt,pos=4)
abline(h=3.5)

# Education
datsub <- na.omit(as.data.frame(cbind(CNN_PA$educ,CNN_PA$weight,CNN_PA$WeightVoteModeWgt)))
colnames(datsub) <- c("educ","OrigWgt","VoteModeWgt")
wtd.table(datsub$educ,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt)
wtd.table(datsub$educ,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
wtd.table(datsub$educ,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$educ,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)

tb <- wtd.table(datsub$educ,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$educ,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
tb <- tb*100
labt <- get_labels(CNN_PA$educ)
points(as.numeric(tb),seq(4,8),pch=18)
text(rep(a,times=length(labt)),seq(4,8),labt,pos=4)
abline(h=8.5)

# income
datsub <- na.omit(as.data.frame(cbind(CNN_PA$income3,CNN_PA$weight,CNN_PA$WeightVoteModeWgt)))
colnames(datsub) <- c("income","OrigWgt","VoteModeWgt")
wtd.table(datsub$income,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt)
wtd.table(datsub$income,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
wtd.table(datsub$income,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$income,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
tb <- wtd.table(datsub$income,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$income,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
tb <- tb*100
labt <- get_labels(CNN_PA$income3)
points(as.numeric(tb),seq(9,12),pch=18)
text(rep(a,times=length(labt)),seq(9,12),labt,pos=4)
abline(h=12.5)

# Gender
datsub <- na.omit(as.data.frame(cbind(CNN_PA$sex,CNN_PA$weight,CNN_PA$WeightVoteModeWgt)))
colnames(datsub) <- c("sex","OrigWgt","VoteModeWgt")
wtd.table(datsub$sex,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt)
wtd.table(datsub$sex,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
wtd.table(datsub$sex,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$sex,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)

tb <- wtd.table(datsub$sex,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$sex,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
tb <- tb*100
labt <- get_labels(CNN_PA$sex)
points(as.numeric(tb),seq(13,14),pch=18)
text(rep(a,times=length(labt)),seq(13,14),labt,pos=4)
abline(h=14.5)

# Age
datsub <- na.omit(as.data.frame(cbind(CNN_PA$age,CNN_PA$weight,CNN_PA$WeightVoteModeWgt)))
colnames(datsub) <- c("age","OrigWgt","VoteModeWgt")
wtd.table(datsub$age,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt)
wtd.table(datsub$age,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
wtd.table(datsub$age,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$age,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)

tb <- wtd.table(datsub$age,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$age,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
tb <- tb*100
labt <- get_labels(CNN_PA$age)
points(as.numeric(tb),seq(15,19),pch=18)
text(rep(a,times=length(labt)),seq(15,19),labt,pos=4)
abline(h=19.5)

# ideology
datsub <- na.omit(as.data.frame(cbind(CNN_PA$ideology,CNN_PA$weight,CNN_PA$WeightVoteModeWgt)))
colnames(datsub) <- c("ideology","OrigWgt","VoteModeWgt")
wtd.table(datsub$ideology,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt)
wtd.table(datsub$ideology,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
wtd.table(datsub$ideology,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$ideology,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)

tb <- wtd.table(datsub$ideology,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$ideology,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
tb <- tb*100
labt <- get_labels(CNN_PA$ideology)
points(as.numeric(tb),seq(20,23),pch=18)
text(rep(a,times=length(labt)),seq(20,23),labt,pos=4)
abline(h=23.5)

# PARTY5
datsub <- na.omit(as.data.frame(cbind(CNN_PA$party,CNN_PA$weight,CNN_PA$WeightVoteModeWgt)))
colnames(datsub) <- c("party","OrigWgt","VoteModeWgt")
wtd.table(datsub$party,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt)
wtd.table(datsub$party,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
wtd.table(datsub$party,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$party,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
tb <- wtd.table(datsub$party,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$party,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
tb <- tb*100
labt <- get_labels(CNN_PA$party)
labt <- c("Dem", "Lean Dem","Ind (no lean)","Lean Rep","Rep")
points(as.numeric(tb),seq(24,28),pch=18)
text(rep(a,times=length(labt)),seq(24,28),labt,pos=4)
tb.party5.PA <- tb
dev.off()

