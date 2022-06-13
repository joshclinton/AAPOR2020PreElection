##############################################################################
##  Analyze CNN Michigan Data
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

CNN_MI <- read_spss("Google Drive/AAPOR.PreElectionPolling/Other Data/2020 General State Level Polls/CNN_Final Datasets/CNN_MI.sav")
CNN_MI$fips_code <- CNN_MI$merge_fips

# Get voter file data

MI_Mode <- read.csv("Google Drive/AAPOR.PreElectionPolling/Other Data/StateCertifiedVoteTotalsMode/MI_TargetSmart_PartyStrataVote_2020.csv")
MI_Vote <- read.csv("Google Drive/AAPOR.PreElectionPolling/Other Data/StateCertifiedVoteTotalsMode/MI_NEAT_PartyStrataVote_2020.csv")

# Get NEAT data for county stratification
mi_ctyvote <- read.csv("Google Drive/AAPOR.PreElectionPolling/Other Data/StateCertifiedVoteTotalsMode/MI_NEAT_CountyVote_2020.csv")
mi_ctyvote$county_name <- tolower(mi_ctyvote$county_name)

# Extract information
mi_ctyinfo <- mi_ctyvote[,c("fips_code","county_name","party_stratum","party_stratum_name","geo_stratum","geo_stratum_name")]

Trump.Vote.Strata <- MI_Vote[,c("party_stratum","Trump")]
Biden.Vote.Strata <- MI_Vote[,c("party_stratum","Biden")]
Absentee.Vote.Strata <- MI_Mode[,c("party_stratum","Absentee")]
ElectionDay.Vote.Strata <- MI_Mode[,c("party_stratum","Election.Day")]
#Early.Vote.Strata <- MI_Mode[,c("party_stratum","Early")]
TotalVote <- sum(MI_Vote$Trump+MI_Vote$Biden)
TotalVote2 <- sum(MI_Mode$Absentee+MI_Mode$Election.Day)

# Define proportion of electorate in each cell

Absentee.Vote.Strata <- Absentee.Vote.Strata[,2]/TotalVote
ElectionDay.Vote.Strata <- ElectionDay.Vote.Strata[,2]/TotalVote
Trump.Vote.Strata <- Trump.Vote.Strata[,2]/TotalVote
Biden.Vote.Strata <- Biden.Vote.Strata[,2]/TotalVote

#################################
#   Now use survey data
##################################

# Merge strata onto CNN
CNN_MI$fips_code <- CNN_MI$merge_fips
nobs1 <- nrow(CNN_MI)

CNN_MI <- merge(CNN_MI,mi_ctyinfo,by="fips_code")
nobs2 <- nrow(CNN_MI)

nobs1-nobs2 # observations lost in merge

# LIKELY VOTERS

CNN_MI <- CNN_MI[CNN_MI$LV==1,]
nrow(CNN_MI)

Poll.Absentee.Vote.Strata <- NULL
Poll.ElectionDay.Vote.Strata <- NULL
Poll.Trump.Vote.Strata <- NULL
Poll.Biden.Vote.Strata <- NULL

CNN_MI$EDVote <- ifelse(CNN_MI$votemode==1,1,0)
CNN_MI$EarlyVote <- ifelse(as.logical(CNN_MI$votemode==2 | CNN_MI$votemode==3 | CNN_MI$votemode==4),1,0)

for(i in 1:5){
  datsub <- subset(CNN_MI,party_stratum==i)
  BidenVote <- sum(as.logical(datsub$presvote==1)*datsub$weight,na.rm=TRUE)
  TrumpVote <- sum(as.logical(datsub$presvote==2)*datsub$weight,na.rm=TRUE)
  EDVote <- sum(datsub$EDVote*datsub$weight,na.rm=TRUE)
  EarlyVote <- sum(datsub$EarlyVote*datsub$weight,na.rm=TRUE)

  Poll.Absentee.Vote.Strata[i] <- EarlyVote
  Poll.ElectionDay.Vote.Strata[i] <- EDVote
  Poll.Trump.Vote.Strata[i] <- TrumpVote
  Poll.Biden.Vote.Strata[i] <- BidenVote
}

# Normalize into a proportion
# THIS ONLY USES THOSE SUPPORTING TRUMP OR BIDEN SO SUMS TO 1, BUT POLL MARGINAL ONLY SUMS TO .93
# Could also sum to weight

TotalN <- sum(Poll.Trump.Vote.Strata + Poll.Biden.Vote.Strata)

Poll.Absentee.Vote.Strata <- Poll.Absentee.Vote.Strata/TotalN
Poll.ElectionDay.Vote.Strata <- Poll.ElectionDay.Vote.Strata/TotalN
Poll.Trump.Vote.Strata <- Poll.Trump.Vote.Strata/TotalN
Poll.Biden.Vote.Strata <- Poll.Biden.Vote.Strata/TotalN 

ElectionDaySizeError <- Poll.ElectionDay.Vote.Strata  - ElectionDay.Vote.Strata 
EarlySizeError <- Poll.Absentee.Vote.Strata  - Absentee.Vote.Strata 
TrumpError <- Poll.Trump.Vote.Strata - Trump.Vote.Strata
BidenError <- Poll.Biden.Vote.Strata - Biden.Vote.Strata

Size <- round(cbind(Poll.Absentee.Vote.Strata,Poll.ElectionDay.Vote.Strata,EarlySizeError,ElectionDaySizeError),digits=3)
Vote <- round(cbind(Poll.Trump.Vote.Strata,Poll.Biden.Vote.Strata,TrumpError,BidenError),digits=3)

################################################################################################
#       Create Weights: Weight to Certified Vote By Mode by Strata
#################################################################################################

EDWgt <- ElectionDay.Vote.Strata /Poll.ElectionDay.Vote.Strata
EarlyWgt <- Absentee.Vote.Strata /Poll.Absentee.Vote.Strata

CNN_MI$ModeWgt <- NA

for(i in 1:5){
  CNN_MI$ModeWgt[CNN_MI$party_stratum==i & CNN_MI$EDVote==1] <- EDWgt[i]
  CNN_MI$ModeWgt[CNN_MI$party_stratum==i & CNN_MI$EarlyVote==1] <- EarlyWgt[i]
}

# How correlated are these?  
cor(CNN_MI$weight,CNN_MI$ModeWgt,use="pairwise.complete")

#
#     Now create a new weight that is the old weight X mode weight
#     GOAL: How does weighting by mode by strata affect accuracy?
#

CNN_MI$WeightModeWgt <- CNN_MI$ModeWgt*CNN_MI$weight

Poll2.Absentee.Vote.Strata <- NULL
Poll2.ElectionDay.Vote.Strata <- NULL
Poll2.Trump.Vote.Strata <- NULL
Poll2.Biden.Vote.Strata <- NULL

for(i in 1:5){
  datsub <- subset(CNN_MI,party_stratum==i)
  BidenVote2 <- sum(as.logical(datsub$presvote==1)*datsub$WeightModeWgt,na.rm=TRUE)
  TrumpVote2 <- sum(as.logical(datsub$presvote==2)*datsub$WeightModeWgt,na.rm=TRUE)
  EDVote2 <- sum(datsub$EDVote*datsub$WeightModeWgt,na.rm=TRUE)
  EarlyVote2 <- sum(datsub$EarlyVote*datsub$WeightModeWgt,na.rm=TRUE)
  
  Poll2.Absentee.Vote.Strata[i] <- EarlyVote2
  Poll2.ElectionDay.Vote.Strata[i] <- EDVote2
  Poll2.Trump.Vote.Strata[i] <- TrumpVote2
  Poll2.Biden.Vote.Strata[i] <- BidenVote2
}

TotalN <- sum(Poll2.Trump.Vote.Strata + Poll2.Biden.Vote.Strata)

Poll2.Absentee.Vote.Strata <- Poll2.Absentee.Vote.Strata/TotalN
Poll2.ElectionDay.Vote.Strata <- Poll2.ElectionDay.Vote.Strata/TotalN
Poll2.Trump.Vote.Strata <- Poll2.Trump.Vote.Strata/TotalN
Poll2.Biden.Vote.Strata <- Poll2.Biden.Vote.Strata/TotalN 

################################################################################################
#       Evaluate the Effect
#       Error Increases!
#       More understatement of Trump/More overstatement of Biden
#################################################################################################

ElectionDaySizeError2 <- Poll2.ElectionDay.Vote.Strata  - ElectionDay.Vote.Strata 
EarlySizeError2 <- Poll2.Absentee.Vote.Strata  - Absentee.Vote.Strata 
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

CNN_MI$VoteWgt <- NA

for(i in 1:5){
  CNN_MI$VoteWgt[CNN_MI$party_stratum==i & CNN_MI$presvote==1] <- BidenWgt[i]
  CNN_MI$VoteWgt[CNN_MI$party_stratum==i & CNN_MI$presvote==2] <- TrumpWgt[i]
}

# How correlated are these?  
cor(CNN_MI$weight,CNN_MI$VoteWgt,use="pairwise.complete")

CNN_MI$WeightVoteModeWgt <- CNN_MI$VoteWgt*CNN_MI$WeightModeWgt

Poll3.Absentee.Vote.Strata <- NULL
Poll3.ElectionDay.Vote.Strata <- NULL
Poll3.Trump.Vote.Strata <- NULL
Poll3.Biden.Vote.Strata <- NULL

for(i in 1:5){
  datsub <- subset(CNN_MI,party_stratum==i)
  BidenVote3 <- sum(as.logical(datsub$presvote==1)*datsub$WeightVoteModeWgt,na.rm=TRUE)
  TrumpVote3 <- sum(as.logical(datsub$presvote==2)*datsub$WeightVoteModeWgt,na.rm=TRUE)
  EDVote3 <- sum(datsub$EDVote*datsub$WeightVoteModeWgt,na.rm=TRUE)
  EarlyVote3 <- sum(datsub$EarlyVote*datsub$WeightVoteModeWgt,na.rm=TRUE)
  
  Poll3.Absentee.Vote.Strata[i] <- EarlyVote3
  Poll3.ElectionDay.Vote.Strata[i] <- EDVote3
  Poll3.Trump.Vote.Strata[i] <- TrumpVote3
  Poll3.Biden.Vote.Strata[i] <- BidenVote3
}

TotalN <- sum(Poll3.Trump.Vote.Strata + Poll3.Biden.Vote.Strata)

Poll3.Absentee.Vote.Strata <- Poll3.Absentee.Vote.Strata/TotalN
Poll3.ElectionDay.Vote.Strata <- Poll3.ElectionDay.Vote.Strata/TotalN
Poll3.Trump.Vote.Strata <- Poll3.Trump.Vote.Strata/TotalN
Poll3.Biden.Vote.Strata <- Poll3.Biden.Vote.Strata/TotalN 

################################################################################################
#       Evaluate the Effect
#       Error Increases!
#       More understatement of Trump/More overstatement of Biden
#################################################################################################

ElectionDaySizeError3 <- Poll3.ElectionDay.Vote.Strata  - ElectionDay.Vote.Strata 
EarlySizeError3 <- Poll3.Absentee.Vote.Strata  - Absentee.Vote.Strata 
TrumpError3 <- Poll3.Trump.Vote.Strata - Trump.Vote.Strata
BidenError3 <- Poll2.Biden.Vote.Strata - Biden.Vote.Strata

sum(Poll3.Trump.Vote.Strata)
sum(Poll3.Biden.Vote.Strata)

# Trump Error Impact from weighting on Vote And Mode
sum(Poll3.Trump.Vote.Strata) - sum(Poll.Trump.Vote.Strata)

# Biden Error Impact from weighting on Mode
sum(Poll3.Biden.Vote.Strata) - sum(Poll.Biden.Vote.Strata)

# Election Day Error Impact from weighting on Vote And Mode
sum(Poll3.ElectionDay.Vote.Strata) - sum(ElectionDay.Vote.Strata)
sum(Poll3.Absentee.Vote.Strata) - sum(Absentee.Vote.Strata)


############################################################
#     NOW LOOK AT IMPACT ON DEMOGRAPHICS
#     Plot the change in proportions
#############################################################

png(file="Google Drive/AAPOR.PreElectionPolling/Writing/Figures/CNNMIAdjust.png",width=600, height=900)

a <- -10
b <- 5
ncompare <- 28
plot(seq(1,ncompare),seq(1,ncompare),xlim=c(a,b),type="n",axes=FALSE,ylab="",
     xlab="Change in % After Reweighting",
     main="MI: Effect of Reweighting by Vote and Mode by County Strata")
axis(1)
abline(v=seq(-5,5,by=1),lty=2,col="GREY")
abline(v=0)

# White/Non-White
datsub <- na.omit(as.data.frame(cbind(CNN_MI$white,CNN_MI$weight,CNN_MI$WeightVoteModeWgt)))
colnames(datsub) <- c("white","OrigWgt","VoteModeWgt")
wtd.table(datsub$white,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt)
wtd.table(datsub$white,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
wtd.table(datsub$white,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$white,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)

tb <- wtd.table(datsub$white,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$white,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
tb<- tb*100
labt <- get_labels(CNN_MI$white)
points(as.numeric(tb),seq(1,3),pch=18)
text(rep(a,times=length(labt)),seq(1,3),labt,pos=4)
abline(h=3.5)

# Education
datsub <- na.omit(as.data.frame(cbind(CNN_MI$educ,CNN_MI$weight,CNN_MI$WeightVoteModeWgt)))
colnames(datsub) <- c("educ","OrigWgt","VoteModeWgt")
wtd.table(datsub$educ,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt)
wtd.table(datsub$educ,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
wtd.table(datsub$educ,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$educ,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)

tb <- wtd.table(datsub$educ,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$educ,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
tb<- tb*100
labt <- get_labels(CNN_MI$educ)
points(as.numeric(tb),seq(4,8),pch=18)
text(rep(a,times=length(labt)),seq(4,8),labt,pos=4)
abline(h=8.5)

# income
datsub <- na.omit(as.data.frame(cbind(CNN_MI$income3,CNN_MI$weight,CNN_MI$WeightVoteModeWgt)))
colnames(datsub) <- c("income","OrigWgt","VoteModeWgt")
wtd.table(datsub$income,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt)
wtd.table(datsub$income,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
wtd.table(datsub$income,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$income,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
tb <- wtd.table(datsub$income,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$income,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
labt <- get_labels(CNN_MI$income3)
points(as.numeric(tb),seq(9,12),pch=18)
text(rep(a,times=length(labt)),seq(9,12),labt,pos=4)
abline(h=12.5)

# Gender
datsub <- na.omit(as.data.frame(cbind(CNN_MI$sex,CNN_MI$weight,CNN_MI$WeightVoteModeWgt)))
colnames(datsub) <- c("sex","OrigWgt","VoteModeWgt")
wtd.table(datsub$sex,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt)
wtd.table(datsub$sex,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
wtd.table(datsub$sex,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$sex,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)

tb <- wtd.table(datsub$sex,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$sex,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
tb<- tb*100
labt <- get_labels(CNN_MI$sex)
points(as.numeric(tb),seq(13,14),pch=18)
text(rep(a,times=length(labt)),seq(13,14),labt,pos=4)
abline(h=14.5)

# Age
datsub <- na.omit(as.data.frame(cbind(CNN_MI$age,CNN_MI$weight,CNN_MI$WeightVoteModeWgt)))
colnames(datsub) <- c("age","OrigWgt","VoteModeWgt")
wtd.table(datsub$age,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt)
wtd.table(datsub$age,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
wtd.table(datsub$age,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$age,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)

tb <- wtd.table(datsub$age,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$age,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
tb<- tb*100
labt <- get_labels(CNN_MI$age)
points(as.numeric(tb),seq(15,19),pch=18)
text(rep(a,times=length(labt)),seq(15,19),labt,pos=4)
abline(h=19.5)

# ideology
datsub <- na.omit(as.data.frame(cbind(CNN_MI$ideology,CNN_MI$weight,CNN_MI$WeightVoteModeWgt)))
colnames(datsub) <- c("ideology","OrigWgt","VoteModeWgt")
wtd.table(datsub$ideology,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt)
wtd.table(datsub$ideology,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
wtd.table(datsub$ideology,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$ideology,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)

tb <- wtd.table(datsub$ideology,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$ideology,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
tb<- tb*100
labt <- get_labels(CNN_MI$ideology)
points(as.numeric(tb),seq(20,23),pch=18)
text(rep(a,times=length(labt)),seq(20,23),labt,pos=4)
abline(h=23.5)

# PARTY5
datsub <- na.omit(as.data.frame(cbind(CNN_MI$party,CNN_MI$weight,CNN_MI$WeightVoteModeWgt)))
colnames(datsub) <- c("party","OrigWgt","VoteModeWgt")
wtd.table(datsub$party,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt)
wtd.table(datsub$party,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
wtd.table(datsub$party,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$party,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
tb <- wtd.table(datsub$party,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$party,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
tb<- tb*100
labt <- get_labels(CNN_MI$party)
labt <- c("Dem", "Lean Dem","Ind (no lean)","Lean Rep","Rep")
points(as.numeric(tb),seq(24,28),pch=18)
text(rep(a,times=length(labt)),seq(24,28),labt,pos=4)
tb.party5.MI <- tb
dev.off()

