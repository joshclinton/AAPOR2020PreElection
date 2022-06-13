##############################################################################
##  Analyze Siena/NYT Kansas Data
##  Use Certified Vote and Vote by Mode by County Party Strata
##  Reweight Poll to match true mode and vote by county strata
##  Examine impact on Demographics
##
##  Josh Clinton
##  Vanderbilt University
##  March 2021
##############################################################################


#install.packages("sjlabelled")
#install.packages("zipcodeR")
library(sjlabelled)
library(stargazer)
library(haven)
library(questionr)

dat <- read_sav("Google Drive/AAPOR.PreElectionPolling/Other Data/2020 General State Level Polls/Siena_Final Datasets/datasetKS.sav")
dat$county_name <- tolower(dat$County)

# Get voter file data

KS_Mode <- read.csv("Google Drive/AAPOR.PreElectionPolling/Other Data/StateCertifiedVoteTotalsMode/2020 Vote History/KS/Strata/KS_TargetSmart_PartyStrataVote_2020.csv")
KS_Vote <- read.csv("Google Drive/AAPOR.PreElectionPolling/Other Data/StateCertifiedVoteTotalsMode/2020 Vote History/KS/Strata/KS_NEAT_PartyStrataVote_2020.csv")

# Get NEAT data for county stratification
ks_ctyvote <- read.csv("Google Drive/AAPOR.PreElectionPolling/Other Data/StateCertifiedVoteTotalsMode/2020 Vote History/KS/County/KS_NEAT_CountyVote_2020.csv")
ks_ctyvote$county_name <- tolower(ks_ctyvote$county_name)

# Extract information
ks_ctyinfo <- ks_ctyvote[,c("fips_code","county_name","party_stratum","party_stratum_name","geo_stratum","geo_stratum_name")]

Trump.Vote.Strata <- KS_Vote[,c("party_stratum","Trump")]
Biden.Vote.Strata <- KS_Vote[,c("party_stratum","Biden")]
Absentee.Vote.Strata <- KS_Mode[,c("party_stratum","Absentee")]
ElectionDay.Vote.Strata <- KS_Mode[,c("party_stratum","Voted..Unknown.Method")] # used unknown
Early.Vote.Strata <- KS_Mode[,c("party_stratum","Early")]
TotalVote <- sum(KS_Vote$Trump+KS_Vote$Biden)
TotalVote2 <- sum(KS_Mode$Absentee+KS_Mode$Election.Day + KS_Mode$Early)

# Define proportion of electorate in each cell

Early.Vote.Strata <- Early.Vote.Strata[,2]/TotalVote
Absentee.Vote.Strata <- Absentee.Vote.Strata[,2]/TotalVote
ElectionDay.Vote.Strata <- ElectionDay.Vote.Strata[,2]/TotalVote
Trump.Vote.Strata <- Trump.Vote.Strata[,2]/TotalVote
Biden.Vote.Strata <- Biden.Vote.Strata[,2]/TotalVote

#################################
#   Now use survey data
##################################

# Merge strata onto Siena
nobs1 <- nrow(dat)

Siena_KS <- merge(dat,ks_ctyinfo,by="county_name")

nobs2 <- nrow(dat)

nobs1-nobs2 # observations lost in merge

# LIKELY VOTERS -- They used everyone

Poll.Early.Vote.Strata <- NULL
Poll.Absentee.Vote.Strata <- NULL
Poll.ElectionDay.Vote.Strata <- NULL
Poll.Trump.Vote.Strata <- NULL
Poll.Biden.Vote.Strata <- NULL

Siena_KS$EDVote <- 0
Siena_KS$EDVote[Siena_KS$QMETH2==1] <- 1
Siena_KS$EarlyVote <- 0
Siena_KS$EarlyVote[Siena_KS$QEVMETH==1] <- 1
Siena_KS$EarlyVote[Siena_KS$QMETH2==2] <- 1
Siena_KS$AbsenteeVote <- 0
Siena_KS$AbsenteeVote[Siena_KS$QMETH2==3] <- 1
Siena_KS$AbsenteeVote[Siena_KS$QEVMETH==2] <- 1

for(i in 1:5){
  datsub <- subset(Siena_KS,party_stratum==i)
  BidenVote <- sum(as.logical(datsub$QPRES==1)*datsub$final_weight,na.rm=TRUE)
  TrumpVote <- sum(as.logical(datsub$QPRES==2)*datsub$final_weight,na.rm=TRUE)
  EDVote <- sum(datsub$EDVote*datsub$final_weight,na.rm=TRUE)
  EarlyVote <- sum(datsub$EarlyVote*datsub$final_weight,na.rm=TRUE)
  AbsenteeVote <- sum(datsub$AbsenteeVote*datsub$final_weight,na.rm=TRUE)
  
  Poll.Absentee.Vote.Strata[i] <- AbsenteeVote
  Poll.Early.Vote.Strata[i] <- EarlyVote
  Poll.ElectionDay.Vote.Strata[i] <- EDVote
  Poll.Trump.Vote.Strata[i] <- TrumpVote
  Poll.Biden.Vote.Strata[i] <- BidenVote
}

# Normalize into a proportion
# THIS ONLY USES THOSE SUPPORTING TRUMP OR BIDEN SO SUMS TO 1, BUT POLL MARGINAL ONLY SUMS TO .93
# Could also sum to weight

TotalN <- sum(Poll.Trump.Vote.Strata + Poll.Biden.Vote.Strata)

Poll.Absentee.Vote.Strata <- Poll.Absentee.Vote.Strata/TotalN
Poll.Early.Vote.Strata <- Poll.Early.Vote.Strata/TotalN
Poll.ElectionDay.Vote.Strata <- Poll.ElectionDay.Vote.Strata/TotalN
Poll.Trump.Vote.Strata <- Poll.Trump.Vote.Strata/TotalN
Poll.Biden.Vote.Strata <- Poll.Biden.Vote.Strata/TotalN 

EarlySizeError <- Poll.Early.Vote.Strata  - Early.Vote.Strata 
ElectionDaySizeError <- Poll.ElectionDay.Vote.Strata  - ElectionDay.Vote.Strata 
AbsenteeSizeError <- Poll.Absentee.Vote.Strata  - Absentee.Vote.Strata 
TrumpError <- Poll.Trump.Vote.Strata - Trump.Vote.Strata
BidenError <- Poll.Biden.Vote.Strata - Biden.Vote.Strata

sum(Poll.Trump.Vote.Strata)
sum(Trump.Vote.Strata)
sum(Poll.Biden.Vote.Strata)
sum(Biden.Vote.Strata)
sum(TrumpError)
sum(BidenError)


Size <- round(cbind(Poll.Absentee.Vote.Strata,Poll.Early.Vote.Strata,Poll.ElectionDay.Vote.Strata,AbsenteeSizeError,EarlySizeError,ElectionDaySizeError),digits=3)
Vote <- round(cbind(Poll.Trump.Vote.Strata,Poll.Biden.Vote.Strata,TrumpError,BidenError),digits=3)

################################################################################################
#       Create Weights: Weight to Certified Vote By Mode by Strata
#################################################################################################

EDWgt <- ElectionDay.Vote.Strata /Poll.ElectionDay.Vote.Strata
AbsWgt <- Absentee.Vote.Strata /Poll.Absentee.Vote.Strata
EarlyWgt <- Early.Vote.Strata/Poll.Early.Vote.Strata

Siena_KS$ModeWgt <- NA

for(i in 1:5){
  Siena_KS$ModeWgt[Siena_KS$party_stratum==i & Siena_KS$EDVote==1] <- EDWgt[i]
  Siena_KS$ModeWgt[Siena_KS$party_stratum==i & Siena_KS$AbsenteeVote==1] <- AbsWgt[i]
  Siena_KS$ModeWgt[Siena_KS$party_stratum==i & Siena_KS$EarlyVote==1] <- EarlyWgt[i]
}

# How correlated are these?  
cor(Siena_KS$final_weight,Siena_KS$ModeWgt,use="pairwise.complete")


#
#     Now create a new weight that is the old weight X mode weight
#     GOAL: How does weighting by mode by strata affect accuracy?
#

Siena_KS$WeightModeWgt <- Siena_KS$ModeWgt*Siena_KS$final_weight

Poll2.Absentee.Vote.Strata <- NULL
Poll2.Early.Vote.Strata <- NULL
Poll2.ElectionDay.Vote.Strata <- NULL
Poll2.Trump.Vote.Strata <- NULL
Poll2.Biden.Vote.Strata <- NULL

for(i in 1:5){
  datsub <- subset(Siena_KS,party_stratum==i)
  BidenVote2 <- sum(as.logical(datsub$QPRES==1)*datsub$WeightModeWgt,na.rm=TRUE)
  TrumpVote2 <- sum(as.logical(datsub$QPRES==2)*datsub$WeightModeWgt,na.rm=TRUE)
  EDVote2 <- sum(datsub$EDVote*datsub$WeightModeWgt,na.rm=TRUE)
  AbsenteeVote2 <- sum(datsub$AbsenteeVote*datsub$WeightModeWgt,na.rm=TRUE)
  EarlyVote2 <- sum(datsub$EarlyVote*datsub$WeightModeWgt,na.rm=TRUE)
  
  Poll2.Absentee.Vote.Strata[i] <- AbsenteeVote2
  Poll2.Early.Vote.Strata[i] <- EarlyVote2
  Poll2.ElectionDay.Vote.Strata[i] <- EDVote2
  Poll2.Trump.Vote.Strata[i] <- TrumpVote2
  Poll2.Biden.Vote.Strata[i] <- BidenVote2
}

TotalN <- sum(Poll2.Trump.Vote.Strata + Poll2.Biden.Vote.Strata)

Poll2.Absentee.Vote.Strata <- Poll2.Absentee.Vote.Strata/TotalN
Poll2.Early.Vote.Strata <- Poll2.Early.Vote.Strata/TotalN
Poll2.ElectionDay.Vote.Strata <- Poll2.ElectionDay.Vote.Strata/TotalN
Poll2.Trump.Vote.Strata <- Poll2.Trump.Vote.Strata/TotalN
Poll2.Biden.Vote.Strata <- Poll2.Biden.Vote.Strata/TotalN 

################################################################################################
#       Evaluate the Effect
#       Error Increases!
#       More understatement of Trump/More overstatement of Biden
#################################################################################################

sum(Poll2.Trump.Vote.Strata)
sum(Trump.Vote.Strata)
sum(Poll2.Biden.Vote.Strata)
sum(Biden.Vote.Strata)

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

Siena_KS$VoteWgt <- NA

for(i in 1:5){
  Siena_KS$VoteWgt[Siena_KS$party_stratum==i & Siena_KS$QPRES==1] <- BidenWgt[i]
  Siena_KS$VoteWgt[Siena_KS$party_stratum==i & Siena_KS$QPRES==2] <- TrumpWgt[i]
}

# How correlated are these?  
cor(Siena_KS$final_weight,Siena_KS$VoteWgt,use="pairwise.complete")

Siena_KS$WeightVoteModeWgt <- Siena_KS$VoteWgt*Siena_KS$WeightModeWgt

Poll3.Absentee.Vote.Strata <- NULL
Poll3.ElectionDay.Vote.Strata <- NULL
Poll3.Early.Vote.Strata <- NULL
Poll3.Trump.Vote.Strata <- NULL
Poll3.Biden.Vote.Strata <- NULL

for(i in 1:5){
  datsub <- subset(Siena_KS,party_stratum==i)
  BidenVote3 <- sum(as.logical(datsub$QPRES==1)*datsub$WeightVoteModeWgt,na.rm=TRUE)
  TrumpVote3 <- sum(as.logical(datsub$QPRES==2)*datsub$WeightVoteModeWgt,na.rm=TRUE)
  EDVote3 <- sum(datsub$EDVote*datsub$WeightVoteModeWgt,na.rm=TRUE)
  AbsenteeVote3 <- sum(datsub$AbsenteeVote*datsub$WeightVoteModeWgt,na.rm=TRUE)
  EarlyVote3 <- sum(datsub$EarlyVote*datsub$WeightVoteModeWgt,na.rm=TRUE)
  
  Poll3.Absentee.Vote.Strata[i] <- AbsenteeVote3
  Poll3.Early.Vote.Strata[i] <- EarlyVote3
  Poll3.ElectionDay.Vote.Strata[i] <- EDVote3
  Poll3.Trump.Vote.Strata[i] <- TrumpVote3
  Poll3.Biden.Vote.Strata[i] <- BidenVote3
}

TotalN <- sum(Poll3.Trump.Vote.Strata + Poll3.Biden.Vote.Strata)

Poll3.Absentee.Vote.Strata <- Poll3.Absentee.Vote.Strata/TotalN
Poll3.ElectionDay.Vote.Strata <- Poll3.ElectionDay.Vote.Strata/TotalN
Poll3.Trump.Vote.Strata <- Poll3.Trump.Vote.Strata/TotalN
Poll3.Biden.Vote.Strata <- Poll3.Biden.Vote.Strata/TotalN
Poll3.Early.Vote.Strata <- Poll3.Early.Vote.Strata/TotalN

################################################################################################
#       Evaluate the Effect
#       Error Increases!
#       More understatement of Trump/More overstatement of Biden
#################################################################################################

sum(Poll3.Trump.Vote.Strata)
sum(Trump.Vote.Strata)
sum(Poll3.Biden.Vote.Strata)
sum(Biden.Vote.Strata)

ElectionDaySizeError3 <- Poll3.ElectionDay.Vote.Strata  - ElectionDay.Vote.Strata 
AbsSizeError3 <- Poll3.Absentee.Vote.Strata  - Absentee.Vote.Strata 
EarlySizeError3 <- Poll3.Early.Vote.Strata  - Early.Vote.Strata 
TrumpError3 <- Poll3.Trump.Vote.Strata - Trump.Vote.Strata
BidenError3 <- Poll2.Biden.Vote.Strata - Biden.Vote.Strata

# Trump Vote Impact from weighting on Vote And Mode
sum(Poll3.Trump.Vote.Strata) - sum(Poll.Trump.Vote.Strata)

# Biden Vote Impact from weighting on Mode
sum(Poll3.Biden.Vote.Strata) - sum(Poll.Biden.Vote.Strata)

# Election Day Error Impact from weighting on Vote And Mode
sum(Poll3.ElectionDay.Vote.Strata) - sum(ElectionDay.Vote.Strata)
sum(Poll3.Absentee.Vote.Strata) - sum(Absentee.Vote.Strata)
sum(Poll3.Early.Vote.Strata) - sum(Early.Vote.Strata)

# Where were errors fixed?
TrumpStrata.KS <- Poll3.Trump.Vote.Strata - Poll.Trump.Vote.Strata
BidenStrata.KS <- Poll3.Biden.Vote.Strata - Poll.Biden.Vote.Strata

############################################################
#     NOW LOOK AT IMPACT ON DEMOGRAPHICS
#     Plot the change in proportions
#############################################################

# Recode Demographics for Plotting
# White/Non-White

Siena_KS$QRACE[Siena_KS$QRACE==4] <- 5
Siena_KS$QRACE[Siena_KS$QRACE==6] <- 5
Siena_KS$QRACE[Siena_KS$QRACE==9] <- NA
Siena_KS$GENDER[Siena_KS$GENDER==3] <- NA

Siena_KS$QVOTE16[Siena_KS$QVOTE16==5] <- 8
Siena_KS$QVOTE16[Siena_KS$QVOTE16==7] <- 6

png(file="Google Drive/AAPOR.PreElectionPolling/Writing/Figures/KSAdjust_Siena.png",width=600, height=900)

a <- -10
b <- 5
ncompare <- 26
plot(seq(1,ncompare),seq(1,ncompare),xlim=c(a,b),type="n",axes=FALSE,ylab="",
     xlab="Change in % After Reweighting to 2020 Outcome",
     main="KS: Effect of Reweighting by Vote and Mode by County Strata")
axis(1)
abline(v=seq(-5,5,by=1),lty=2,col="GREY")
abline(v=0)

# Race

datsub <- na.omit(as.data.frame(cbind(Siena_KS$QRACE,Siena_KS$final_weight,Siena_KS$WeightVoteModeWgt)))
colnames(datsub) <- c("race","OrigWgt","VoteModeWgt")
wtd.table(datsub$race,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt)
wtd.table(datsub$race,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
wtd.table(datsub$race,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$race,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)

tb <- wtd.table(datsub$race,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$race,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
tb <- tb*100
labt <- c("White","Black","Other")
points(as.numeric(tb),seq(1,length(labt)),pch=18)
text(rep(a,times=length(labt)),seq(1,length(labt)),labt,pos=4)
abline(h=3.5)
tb.race.KS <- tb

# Education
datsub <- na.omit(as.data.frame(cbind(Siena_KS$QEDUC4,Siena_KS$final_weight,Siena_KS$WeightVoteModeWgt)))
colnames(datsub) <- c("educ","OrigWgt","VoteModeWgt")
wtd.table(datsub$educ,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt)
wtd.table(datsub$educ,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
wtd.table(datsub$educ,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$educ,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)

tb <- wtd.table(datsub$educ,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$educ,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
tb <- tb*100
labt <- get_labels(Siena_KS$QEDUC4)
points(as.numeric(tb),seq(4,8),pch=18)
text(rep(a,times=length(labt)),seq(4,8),labt,pos=4)
abline(h=8.5)
tb.educ.KS <- tb

# Gender
datsub <- na.omit(as.data.frame(cbind(Siena_KS$GENDER,Siena_KS$final_weight,Siena_KS$WeightVoteModeWgt)))
colnames(datsub) <- c("sex","OrigWgt","VoteModeWgt")
wtd.table(datsub$sex,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt)
wtd.table(datsub$sex,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
wtd.table(datsub$sex,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$sex,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)

tb <- wtd.table(datsub$sex,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$sex,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
tb <- tb*100
labt <- c("Male","Female")
points(as.numeric(tb),seq(9,10),pch=18)
text(rep(a,times=length(labt)),seq(9,10),labt,pos=4)
abline(h=10.5)
tb.gender.KS <- tb

# Age
datsub <- na.omit(as.data.frame(cbind(Siena_KS$AGE4,Siena_KS$final_weight,Siena_KS$WeightVoteModeWgt)))
colnames(datsub) <- c("age","OrigWgt","VoteModeWgt")
wtd.table(datsub$age,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt)
wtd.table(datsub$age,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
wtd.table(datsub$age,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$age,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)

tb <- wtd.table(datsub$age,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$age,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
tb <- tb*100
labt <- get_labels(Siena_KS$AGE4)[1:4]
points(as.numeric(tb)[1:4],seq(11,14),pch=18)
text(rep(a,times=length(labt)),seq(11,14),labt,pos=4)
abline(h=14.5)
tb.age.KS <- tb

# No ideology, but do have Trump 16 vote
datsub <- na.omit(as.data.frame(cbind(Siena_KS$QVOTE16,Siena_KS$final_weight,Siena_KS$WeightVoteModeWgt)))
colnames(datsub) <- c("vote16","OrigWgt","VoteModeWgt")
wtd.table(datsub$vote16,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt)
wtd.table(datsub$vote16,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
wtd.table(datsub$vote16,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$vote16,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)

tb <- wtd.table(datsub$vote16,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$vote16,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
tb <- tb*100
labt <- c("Trump","Clinton","Johnson","Stein","Other","Did Not Vote","Refused")
points(as.numeric(tb),seq(15,21),pch=18)
text(rep(a,times=length(labt)),seq(15,21),labt,pos=4)
abline(h=21.5)
tb.trump16.KS <- tb

# PARTY
datsub <- na.omit(as.data.frame(cbind(Siena_KS$QPARTYID,Siena_KS$final_weight,Siena_KS$WeightVoteModeWgt)))
colnames(datsub) <- c("party","OrigWgt","VoteModeWgt")
wtd.table(datsub$party,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt)
wtd.table(datsub$party,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
wtd.table(datsub$party,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$party,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
tb <- wtd.table(datsub$party,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$party,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
tb <- tb*100
labt <- get_labels(Siena_KS$QPARTYID)
labt <- c("Dem", "Rep","Ind","Other","Don't Know")
points(as.numeric(tb),seq(22,26),pch=18)
text(rep(a,times=length(labt)),seq(22,26),labt,pos=4)
tb.party.KS <- tb
dev.off()

# Note that the implied 2016 vote would be even more different from the actual 2016 vote to make this true
datsub <- na.omit(as.data.frame(cbind(Siena_KS$QVOTE16,Siena_KS$final_weight,Siena_KS$WeightVoteModeWgt)))
colnames(datsub) <- c("vote16","OrigWgt","VoteModeWgt")
nrow(datsub)
datsub <- datsub[-seq(1,nrow(datsub))*as.logical(datsub$vote16==8 | datsub$vote16==9),]
nrow(datsub)
table(datsub$vote16)/sum(table(datsub$vote16))
wtd.table(datsub$vote16,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt)
wtd.table(datsub$vote16,weights=datsub$OrigWgt)/sum(weights=datsub$OrigWgt)
ChangeKS <- table(Vote2106=Siena_KS$QVOTE16,Vote2020=Siena_KS$QPRES)

# New Voters
sum(as.logical(Siena_KS$QVOTE16==8)*Siena_KS$final_weight)/sum(Siena_KS$final_weight)
sum(as.logical(Siena_KS$QVOTE16==8 & Siena_KS$QPRES==1)*Siena_KS$final_weight)/sum(as.logical(Siena_KS$QVOTE16==8)*Siena_KS$final_weight)
sum(as.logical(Siena_KS$QVOTE16==8 & Siena_KS$QPRES==2)*Siena_KS$final_weight)/sum(as.logical(Siena_KS$QVOTE16==8)*Siena_KS$final_weight)
sum(as.logical(Siena_KS$QVOTE16==8 & Siena_KS$QPRES==9)*Siena_KS$final_weight)/sum(as.logical(Siena_KS$QVOTE16==8)*Siena_KS$final_weight)
sum(as.logical(Siena_KS$QVOTE16==8 & Siena_KS$QPRES==9 & Siena_KS$QPRESLN==1)*Siena_KS$final_weight)/sum(as.logical(Siena_KS$QVOTE16==8 & Siena_KS$QPRES==9)*Siena_KS$final_weight)
sum(as.logical(Siena_KS$QVOTE16==8 & Siena_KS$QPRES==9 & Siena_KS$QPRESLN==2)*Siena_KS$final_weight)/sum(as.logical(Siena_KS$QVOTE16==8 & Siena_KS$QPRES==9)*Siena_KS$final_weight)


# New voters went 68 to 24 for Biden (unweighted)
# 62 refused to answer (unweighted)

