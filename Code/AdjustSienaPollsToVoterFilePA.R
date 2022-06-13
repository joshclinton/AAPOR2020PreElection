##############################################################################
##  Analyze Siena/NYT Pennsylvania Data
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

dat <- read_sav("~/Google Drive/AAPOR.PreElectionPolling/Other Data/2020 General State Level Polls/Siena_Final Datasets/datasetPA.sav")
dat$county_name <- tolower(dat$County)

# Get voter file data

PA_Mode <- read.csv("~/Google Drive/AAPOR.PreElectionPolling/Other Data/StateCertifiedVoteTotalsMode/PA_TargetSmart_PartyStrataVote_2020.csv")
PA_Vote <- read.csv("~/Google Drive/AAPOR.PreElectionPolling/Other Data/StateCertifiedVoteTotalsMode/PA_NEAT_PartyStrataVote_2020.csv")

# Get NEAT data for county stratification
pa_ctyvote <- read.csv("~/Google Drive/AAPOR.PreElectionPolling/Other Data/StateCertifiedVoteTotalsMode/PA_NEAT_CountyVote_2020.csv")
pa_ctyvote$county_name <- tolower(pa_ctyvote$county_name)

# Extract information
pa_ctyinfo <- pa_ctyvote[,c("fips_code","county_name","party_stratum","party_stratum_name","geo_stratum","geo_stratum_name")]

Trump.Vote.Strata <- PA_Vote[,c("party_stratum","Trump")]
Biden.Vote.Strata <- PA_Vote[,c("party_stratum","Biden")]
Absentee.Vote.Strata <- PA_Mode[,c("party_stratum","Absentee")]
ElectionDay.Vote.Strata <- PA_Mode[,c("party_stratum","Election.Day")]
TotalVote <- sum(PA_Vote$Trump+PA_Vote$Biden)
TotalVote2 <- sum(PA_Mode$Absentee+PA_Mode$Election.Day)

# Define proportion of electorate in each cell

Absentee.Vote.Strata <- Absentee.Vote.Strata[,2]/TotalVote
ElectionDay.Vote.Strata <- ElectionDay.Vote.Strata[,2]/TotalVote
Trump.Vote.Strata <- Trump.Vote.Strata[,2]/TotalVote
Biden.Vote.Strata <- Biden.Vote.Strata[,2]/TotalVote

#################################
#   Now use survey data
##################################

# Merge strata onto Siena
nobs1 <- nrow(dat)

Siena_PA <- merge(dat,pa_ctyinfo,by="county_name")

nobs2 <- nrow(dat)

nobs1-nobs2 # observations lost in merge

# LIKELY VOTERS -- They used everyone

table(Siena_PA$QLIKELY)
table(dat$QEVMETH)
table(dat$QEVMETH)
table(dat$QMETH2)

Poll.Absentee.Vote.Strata <- NULL
Poll.ElectionDay.Vote.Strata <- NULL
Poll.Trump.Vote.Strata <- NULL
Poll.Biden.Vote.Strata <- NULL

Siena_PA$EDVote <- 0
Siena_PA$EDVote[Siena_PA$QMETH2==1] <- 1
Siena_PA$AbsenteeVote <- 0
Siena_PA$AbsenteeVote[Siena_PA$QEVMETH==1] <- 1
Siena_PA$AbsenteeVote[Siena_PA$QEVMETH==2] <- 1
Siena_PA$AbsenteeVote[Siena_PA$QMETH2==2] <- 1
Siena_PA$AbsenteeVote[Siena_PA$QMETH2==3] <- 1

table(Siena_PA$EDVote,Siena_PA$AbsenteeVote)  # 33 have neither!

for(i in 1:5){
  datsub <- subset(Siena_PA,party_stratum==i)
  BidenVote <- sum(as.logical(datsub$QPRES==1)*datsub$final_weight,na.rm=TRUE)
  TrumpVote <- sum(as.logical(datsub$QPRES==2)*datsub$final_weight,na.rm=TRUE)
  EDVote <- sum(datsub$EDVote*datsub$final_weight,na.rm=TRUE)
  AbsenteeVote <- sum(datsub$AbsenteeVote*datsub$final_weight,na.rm=TRUE)
  
  Poll.Absentee.Vote.Strata[i] <- AbsenteeVote
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

sum(Poll.Trump.Vote.Strata)
sum(Trump.Vote.Strata)
sum(Poll.Biden.Vote.Strata)
sum(Biden.Vote.Strata)
sum(TrumpError)
sum(BidenError)

Size <- round(cbind(Poll.Absentee.Vote.Strata,Poll.ElectionDay.Vote.Strata,EarlySizeError,ElectionDaySizeError),digits=3)
Vote <- round(cbind(Poll.Trump.Vote.Strata,Poll.Biden.Vote.Strata,TrumpError,BidenError),digits=3)

################################################################################################
#       Create Weights: Weight to Certified Vote By Mode by Strata
#################################################################################################

EDWgt <- ElectionDay.Vote.Strata /Poll.ElectionDay.Vote.Strata
AbsWgt <- Absentee.Vote.Strata /Poll.Absentee.Vote.Strata

Siena_PA$ModeWgt <- NA

for(i in 1:5){
  Siena_PA$ModeWgt[Siena_PA$party_stratum==i & Siena_PA$EDVote==1] <- EDWgt[i]
  Siena_PA$ModeWgt[Siena_PA$party_stratum==i & Siena_PA$AbsenteeVote==1] <- AbsWgt[i]
}

# How correlated are these?  
cor(Siena_PA$final_weight,Siena_PA$ModeWgt,use="pairwise.complete")


#
#     Now create a new weight that is the old weight X mode weight
#     GOAL: How does weighting by mode by strata affect accuracy?
#

Siena_PA$WeightModeWgt <- Siena_PA$ModeWgt*Siena_PA$final_weight

Poll2.Absentee.Vote.Strata <- NULL
Poll2.ElectionDay.Vote.Strata <- NULL
Poll2.Trump.Vote.Strata <- NULL
Poll2.Biden.Vote.Strata <- NULL

for(i in 1:5){
  datsub <- subset(Siena_PA,party_stratum==i)
  BidenVote2 <- sum(as.logical(datsub$QPRES==1)*datsub$WeightModeWgt,na.rm=TRUE)
  TrumpVote2 <- sum(as.logical(datsub$QPRES==2)*datsub$WeightModeWgt,na.rm=TRUE)
  EDVote2 <- sum(datsub$EDVote*datsub$WeightModeWgt,na.rm=TRUE)
  AbsenteeVote2 <- sum(datsub$AbsenteeVote*datsub$WeightModeWgt,na.rm=TRUE)
  
  Poll2.Absentee.Vote.Strata[i] <- AbsenteeVote2
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

Siena_PA$VoteWgt <- NA

for(i in 1:5){
  Siena_PA$VoteWgt[Siena_PA$party_stratum==i & Siena_PA$QPRES==1] <- BidenWgt[i]
  Siena_PA$VoteWgt[Siena_PA$party_stratum==i & Siena_PA$QPRES==2] <- TrumpWgt[i]
}

# How correlated are these?  
cor(Siena_PA$final_weight,Siena_PA$VoteWgt,use="pairwise.complete")

Siena_PA$WeightVoteModeWgt <- Siena_PA$VoteWgt*Siena_PA$WeightModeWgt

Poll3.Absentee.Vote.Strata <- NULL
Poll3.ElectionDay.Vote.Strata <- NULL
Poll3.Trump.Vote.Strata <- NULL
Poll3.Biden.Vote.Strata <- NULL

for(i in 1:5){
  datsub <- subset(Siena_PA,party_stratum==i)
  BidenVote3 <- sum(as.logical(datsub$QPRES==1)*datsub$WeightVoteModeWgt,na.rm=TRUE)
  TrumpVote3 <- sum(as.logical(datsub$QPRES==2)*datsub$WeightVoteModeWgt,na.rm=TRUE)
  EDVote3 <- sum(datsub$EDVote*datsub$WeightVoteModeWgt,na.rm=TRUE)
  AbsenteeVote3 <- sum(datsub$AbsenteeVote*datsub$WeightVoteModeWgt,na.rm=TRUE)
  
  Poll3.Absentee.Vote.Strata[i] <- AbsenteeVote3
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

sum(Poll3.Trump.Vote.Strata)
sum(Trump.Vote.Strata)
sum(Poll3.Biden.Vote.Strata)
sum(Biden.Vote.Strata)

ElectionDaySizeError3 <- Poll3.ElectionDay.Vote.Strata  - ElectionDay.Vote.Strata 
EarlySizeError3 <- Poll3.Absentee.Vote.Strata  - Absentee.Vote.Strata 
TrumpError3 <- Poll3.Trump.Vote.Strata - Trump.Vote.Strata
BidenError3 <- Poll2.Biden.Vote.Strata - Biden.Vote.Strata

# Trump Vote Impact from weighting on Vote And Mode
sum(Poll3.Trump.Vote.Strata) - sum(Poll.Trump.Vote.Strata)

# Biden Vote Impact from weighting on Mode
sum(Poll3.Biden.Vote.Strata) - sum(Poll.Biden.Vote.Strata)

# Election Day Error Impact from weighting on Vote And Mode
sum(Poll3.ElectionDay.Vote.Strata) - sum(ElectionDay.Vote.Strata)
sum(Poll3.Absentee.Vote.Strata) - sum(Absentee.Vote.Strata)

# Where were errors fixed?
TrumpStrata.PA <- Poll3.Trump.Vote.Strata - Poll.Trump.Vote.Strata
BidenStrata.PA <- Poll3.Biden.Vote.Strata - Poll.Biden.Vote.Strata

############################################################
#     NOW LOOK AT IMPACT ON DEMOGRAPHICS
#     Plot the change in proportions
#############################################################

# Recode Demographics for Plotting
# White/Non-White

Siena_PA$QRACE[Siena_PA$QRACE==4] <- 5
Siena_PA$QRACE[Siena_PA$QRACE==6] <- 5
Siena_PA$QRACE[Siena_PA$QRACE==9] <- NA
Siena_PA$GENDER[Siena_PA$GENDER==3] <- NA

Siena_PA$QVOTE16[Siena_PA$QVOTE16==5] <- 8
Siena_PA$QVOTE16[Siena_PA$QVOTE16==7] <- 6

png(file="Google Drive/AAPOR.PreElectionPolling/Writing/Figures/PAAdjust_Siena.png",width=600, height=900)

a <- -10
b <- 5
ncompare <- 26
plot(seq(1,ncompare),seq(1,ncompare),xlim=c(a,b),type="n",axes=FALSE,ylab="",
     xlab="Change in % After Reweighting to 2020 Outcome",
     main="PA: Effect of Reweighting by Vote and Mode by County Strata")
axis(1)
abline(v=seq(-5,5,by=1),lty=2,col="GREY")
abline(v=0)

# Race

datsub <- na.omit(as.data.frame(cbind(Siena_PA$QRACE,Siena_PA$final_weight,Siena_PA$WeightVoteModeWgt)))
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
tb.race.PA <- tb

# Education
datsub <- na.omit(as.data.frame(cbind(Siena_PA$QEDUC4,Siena_PA$final_weight,Siena_PA$WeightVoteModeWgt)))
colnames(datsub) <- c("educ","OrigWgt","VoteModeWgt")
wtd.table(datsub$educ,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt)
wtd.table(datsub$educ,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
wtd.table(datsub$educ,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$educ,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)

tb <- wtd.table(datsub$educ,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$educ,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
tb <- tb*100
labt <- get_labels(Siena_PA$QEDUC4)
points(as.numeric(tb),seq(4,8),pch=18)
text(rep(a,times=length(labt)),seq(4,8),labt,pos=4)
abline(h=8.5)
tb.educ.PA <- tb

# Gender
datsub <- na.omit(as.data.frame(cbind(Siena_PA$GENDER,Siena_PA$final_weight,Siena_PA$WeightVoteModeWgt)))
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
tb.gender.PA <- tb

# Age
datsub <- na.omit(as.data.frame(cbind(Siena_PA$AGE4,Siena_PA$final_weight,Siena_PA$WeightVoteModeWgt)))
colnames(datsub) <- c("age","OrigWgt","VoteModeWgt")
wtd.table(datsub$age,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt)
wtd.table(datsub$age,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
wtd.table(datsub$age,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$age,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)

tb <- wtd.table(datsub$age,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$age,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
tb <- tb*100
labt <- get_labels(Siena_PA$AGE4)[1:4]
points(as.numeric(tb),seq(11,14),pch=18)
text(rep(a,times=length(labt)),seq(11,14),labt,pos=4)
abline(h=14.5)
tb.age.PA <- tb

# No ideology, but do have Trump 16 vote
datsub <- na.omit(as.data.frame(cbind(Siena_PA$QVOTE16,Siena_PA$final_weight,Siena_PA$WeightVoteModeWgt)))
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
tb.trump16.PA <- tb

# PARTY
datsub <- na.omit(as.data.frame(cbind(Siena_PA$QPARTYID,Siena_PA$final_weight,Siena_PA$WeightVoteModeWgt)))
colnames(datsub) <- c("party","OrigWgt","VoteModeWgt")
table(datsub$party)/length(datsub$party)
wtd.table(datsub$party,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt)
wtd.table(datsub$party,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
wtd.table(datsub$party,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$party,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
tb <- wtd.table(datsub$party,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt) - wtd.table(datsub$party,weights=datsub$OrigWgt)/sum(datsub$OrigWgt)
tb <- tb*100
labt <- get_labels(Siena_PA$QPARTYID)
labt <- c("Dem", "Rep","Ind","Other","Don't Know")
points(as.numeric(tb),seq(22,26),pch=18)
text(rep(a,times=length(labt)),seq(22,26),labt,pos=4)
tb.party.PA <- tb
dev.off()

# Note that the implied 2016 vote would be even more different from the actual 2016 vote to make this true
# To make this work would imply a 6+ point Trump win in 2016
datsub <- na.omit(as.data.frame(cbind(Siena_PA$QVOTE16,Siena_PA$final_weight,Siena_PA$WeightVoteModeWgt)))
colnames(datsub) <- c("vote16","OrigWgt","VoteModeWgt")
nrow(datsub)
datsub <- datsub[-seq(1,nrow(datsub))*as.logical(datsub$vote16==8 | datsub$vote16==9),]
nrow(datsub)
table(datsub$vote16)/sum(table(datsub$vote16))
wtd.table(datsub$vote16,weights=datsub$VoteModeWgt)/sum(weights=datsub$VoteModeWgt)
wtd.table(datsub$vote16,weights=datsub$OrigWgt)/sum(weights=datsub$OrigWgt)
ChangePA <- table(Vote2016=Siena_PA$QVOTE16,Vote2020=Siena_PA$QPRES)

Siena_PA$QVOTE16[Siena_PA$QVOTE16==5] <- 8
Siena_PA$QVOTE16[Siena_PA$QVOTE16==9] <- 8

100*table(dat$QPRES[Siena_PA$QVOTE16==8])/sum(table(dat$QPRES[Siena_PA$QVOTE16==8]))


# New Voters
sum(as.logical(Siena_PA$QVOTE16==8)*Siena_PA$final_weight)/sum(Siena_PA$final_weight)
sum(as.logical(Siena_PA$QVOTE16==8 & Siena_PA$QPRES==1)*Siena_PA$final_weight)/sum(as.logical(Siena_PA$QVOTE16==8)*Siena_PA$final_weight)
sum(as.logical(Siena_PA$QVOTE16==8 & Siena_PA$QPRES==2)*Siena_PA$final_weight)/sum(as.logical(Siena_PA$QVOTE16==8)*Siena_PA$final_weight)
sum(as.logical(Siena_PA$QVOTE16==8 & Siena_PA$QPRES==9)*Siena_PA$final_weight)/sum(as.logical(Siena_PA$QVOTE16==8)*Siena_PA$final_weight)


# New voters went 187 to 141 for Biden (unweighted)
# 62 refused to answer (unweighted)

