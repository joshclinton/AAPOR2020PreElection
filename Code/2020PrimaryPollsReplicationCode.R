#########################################################################
#########################################################################
# AAPOR Task Force on 2020 Pre-Election Polls: 
# Performance of Democratic Primary Polls
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
##  Creating primary poll datasets
######################################################################################

##  ALABAMA

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "AL")     # Change for each state
# Calculate Margin of Error based on sample size if missing
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/3/2020", "%m/%d/%Y")       # Change for each state

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

dat <- dat[dat$DaysToElection > -14,]

dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)

dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]))){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]))){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

AL <- dat
saveRDS(AL, file="Rdata/ALpolls14.RDS")
rm(dat)

##  ARKANSAS

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "AR")    
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/3/2020", "%m/%d/%Y")    

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

dat <- dat[dat$DaysToElection > -14,]

dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)

dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]))){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]))){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

AR <- dat
saveRDS(AR, file="Rdata/ARpolls14.RDS")
rm(dat)

##  CALIFORNIA

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "CA")   
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/3/2020", "%m/%d/%Y")    

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

dat <- dat[dat$DaysToElection > -14,]

dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$poll.vote.margin <- (dat$Sanders-dat$Biden) - (dat$sanders.vote - dat$biden.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Sanders[i] >= max(c(dat$Buttigieg[i],dat$Biden[i],dat$Warren[i]))){dat$correct.pred[i]<-1}
  if(dat$Sanders[i] < max(c(dat$Buttigieg[i],dat$Biden[i],dat$Warren[i]))){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

CA <- dat

saveRDS(CA, file="Rdata/CApolls14.RDS")
rm(dat)

##  COLORADO

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "CO")     
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/3/2020", "%m/%d/%Y")    

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

dat <- dat[dat$DaysToElection > -14,]

dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$poll.vote.margin <- (dat$Sanders-dat$Biden) - (dat$sanders.vote - dat$biden.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Sanders[i] >= max(c(dat$Bloomberg[i],dat$Biden[i],dat$Warren[i]))){dat$correct.pred[i]<-1}
  if(dat$Sanders[i] < max(c(dat$Bloomberg[i],dat$Biden[i],dat$Warren[i]))){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

CO <- dat
saveRDS(CO, file="Rdata/COpolls14.RDS")
rm(dat)

##  IOWA

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "IA")     
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("02/03/2020", "%m/%d/%Y")       

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

dat <- dat[dat$DaysToElection > -14,]

dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$poll.vote.margin <- (dat$Sanders - dat$Buttigieg) - (dat$sanders.vote - dat$buttigieg.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Sanders[i] >= max(c(dat$Buttigieg[i],dat$Biden[i],dat$Warren[i]))){dat$correct.pred[i]<-1}
  if(dat$Sanders[i] < max(c(dat$Buttigieg[i],dat$Biden[i],dat$Warren[i]))){dat$correct.pred[i]<-0}
}
mean(dat$correct.pred)

dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Buttigieg.error <- dat$buttigieg.vote - dat$Buttigieg
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren 
dat$Klobuchar.error <- dat$klobuchar.vote - dat$Klobuchar

IA <- dat

saveRDS(IA, file="Rdata/IApolls14.RDS")
rm(dat)

##  IDAHO

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "ID")     
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/10/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

dat <- dat[dat$DaysToElection > -14,]

dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= dat$Sanders[i]){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < dat$Sanders[i]){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden

ID <- dat

saveRDS(ID, file="Rdata/IDpolls14.RDS")
rm(dat)


##  MASSACHUSSETTS

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "MA")     
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/3/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

dat <- dat[dat$DaysToElection > -14,]

dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]))){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]))){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

MA <- dat

saveRDS(MA, file="Rdata/MApolls14.RDS")
rm(dat)

##  MAINE

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "ME")     

dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/3/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

dat <- dat[dat$DaysToElection > -14,]

dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]))){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]))){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

ME <- dat

saveRDS(ME, file="Rdata/MEpolls14.RDS")
rm(dat)

##  MICHIGAN

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "MI")    
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/10/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

dat <- dat[dat$DaysToElection > -14,]

dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=TRUE)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=TRUE)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

MI <- dat

saveRDS(MI, file="Rdata/MIpolls14.RDS")
rm(dat)


##  MINNESOTA

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "MN")     
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/3/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

dat <- dat[dat$DaysToElection > -14,]

dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=TRUE)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=TRUE)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

MN <- dat

saveRDS(MN, file="Rdata/MNpolls14.RDS")
rm(dat)

##  MISSOURI

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "MO")     # Change for each state
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/10/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

dat <- dat[dat$DaysToElection > -14,]

dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=TRUE)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=TRUE)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

MO <- dat

saveRDS(MO, file="Rdata/MOpolls14.RDS")
rm(dat)


##  MISSISSIPPI

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "MS")     
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/10/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

dat <- dat[dat$DaysToElection > -14,]

dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=TRUE)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=TRUE)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

MS <- dat

saveRDS(MS, file="Rdata/MSpolls14.RDS")
rm(dat)

##  NORTH CAROLINA

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "NC")     
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/3/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

dat <- dat[dat$DaysToElection > -14,]

dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=TRUE)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=TRUE)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

NC <- dat

saveRDS(NC, file="Rdata/NCpolls14.RDS")
rm(dat)

##  NORTH DAKOTA

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "ND")     
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/10/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

dat <- dat[dat$DaysToElection > -14,]

dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=TRUE)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=TRUE)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

ND <- dat

saveRDS(ND, file="Rdata/NDpolls14.RDS")
rm(dat)

##  New Hampshire

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "NH")   

dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("02/11/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

dat <- dat[dat$DaysToElection > -14,]

dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)

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

dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Buttigieg.error <- dat$buttigieg.vote - dat$Buttigieg
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren 
dat$Klobuchar.error <- dat$klobuchar.vote - dat$Klobuchar

NH <- dat

saveRDS(NH, file="Rdata/NHpolls14.RDS")
rm(dat)

##  Nevada

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "NV")   

dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("02/22/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

dat <- dat[dat$DaysToElection > -14,]

dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)

dat$poll.vote.margin <- (dat$Sanders - dat$Buttigieg) - (dat$sanders.vote - dat$buttigieg.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Sanders[i] >= max(c(dat$Buttigieg[i],dat$Biden[i],dat$Warren[i]))){dat$correct.pred[i]<-1}
  if(dat$Sanders[i] < max(c(dat$Buttigieg[i],dat$Biden[i],dat$Warren[i]))){dat$correct.pred[i]<-0}
}
mean(dat$correct.pred)

dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Buttigieg.error <- dat$buttigieg.vote - dat$Buttigieg
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren 
dat$Klobuchar.error <- dat$klobuchar.vote - dat$Klobuchar

NV <- dat

saveRDS(NV, file="Rdata/NVpolls14.RDS")
rm(dat)

##  OKLAHOMA

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "OK")     
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/3/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

dat <- dat[dat$DaysToElection > -14,]

dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=TRUE)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=TRUE)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

OK <- dat

saveRDS(OK, file="Rdata/OKpolls14.RDS")
rm(dat)

##  SOUTH CAROLINA

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "SC")     
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("02/29/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

dat <- dat[dat$DaysToElection > -14,]

dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Buttigieg[i],dat$Sanders[i],dat$Warren[i]))){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Buttigieg[i],dat$Sanders[i],dat$Warren[i]))){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Buttigieg.error <- Buttigieg.vote - dat$Buttigieg
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Klobuchar.error <- Klobuchar.vote - dat$Klobuchar

SC <- dat

saveRDS(SC, file="Rdata/SCpolls14.RDS")
rm(dat)

##  TENNESSEE

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "TN")     
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/3/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

dat <- dat[dat$DaysToElection > -14,]

dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

TN <- dat
saveRDS(TN, file="Rdata/TNpolls14.RDS")
rm(dat)

##  TEXAS

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "TX")     
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/3/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

dat <- dat[dat$DaysToElection > -14,]

dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

TX <- dat

saveRDS(TX, file="Rdata/TXpolls14.RDS")
rm(dat)

##  UTAH

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "UT")     
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/3/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

dat <- dat[dat$DaysToElection > -14,]

dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

dat$poll.vote.margin <- (dat$Sanders-dat$Biden) - (dat$sanders.vote - dat$biden.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Sanders[i] >= max(c(dat$Buttigieg[i],dat$Biden[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-1}
  if(dat$Sanders[i] < max(c(dat$Buttigieg[i],dat$Biden[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

UT <- dat

saveRDS(UT, file="Rdata/UTpolls14.RDS")
rm(dat)

##  VIRGINA

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "VA")     
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/3/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

dat <- dat[dat$DaysToElection > -14,]

dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Buttigieg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Buttigieg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

VA <- dat

saveRDS(VA, file="Rdata/VApolls14.RDS")
rm(dat)

##  VERMONT

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "VT")    
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/3/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

dat <- dat[dat$DaysToElection > -14,]

dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

dat$poll.vote.margin <- (dat$Sanders-dat$Biden) - (dat$sanders.vote - dat$biden.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Sanders[i] >= max(c(dat$Bloomberg[i],dat$Biden[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-1}
  if(dat$Sanders[i] < max(c(dat$Bloomberg[i],dat$Biden[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-0}}


mean(dat$correct.pred)

dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

VT <- dat

saveRDS(VT, file="Rdata/VTpolls14.RDS")
rm(dat)

##  WASHINGTON

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "WA")     
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/10/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

dat <- dat[dat$DaysToElection > -14,]

dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

WA <- dat

saveRDS(WA, file="Rdata/WApolls14.RDS")
rm(dat)

##  ARIZONA

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "AZ")     
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/17/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

dat <- dat[dat$DaysToElection > -14,]

dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

AZ <- dat

saveRDS(AZ, file="Rdata/AZpolls14.RDS")
rm(dat)

##  FLORIDA

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "FL")     
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/17/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

dat <- dat[dat$DaysToElection > -14,]

dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

FL <- dat

saveRDS(FL, file="Rdata/FLpolls14.RDS")
rm(dat)

##  ILLINOIS

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "IL")    
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]
dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("3/17/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

dat <- dat[dat$DaysToElection > -14,]

dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

IL <- dat

saveRDS(IL, file="Rdata/ILpolls14.RDS")
rm(dat)

##  WISCONSIN

dat <- read_excel("Dem Primary Polling.xlsx", sheet = "WI")     
dat$MoE <- as.numeric(dat$MoE)
dat$moe <- 100*sqrt(.25/dat$SampleSize)*1.96

dat$MoE[is.na(dat$MoE)*seq(1:nrow(dat))] <- dat$moe[is.na(dat$MoE)*seq(1:nrow(dat))]

dat$EndDate <- as.Date(dat$EndDate, "%m/%d/%Y")
dat$StartDate <- as.Date(dat$StartDate, "%m/%d/%Y")
election.day <- as.Date("4/7/2020", "%m/%d/%Y")

dat$DaysToElection <-  dat$EndDate - election.day
dat$DaysInField <- dat$EndDate - dat$StartDate

dat <- dat[dat$DaysToElection > -14,]

dat$Buttigieg <- as.numeric(dat$Buttigieg)
dat$Klobuchar <- as.numeric(dat$Klobuchar)
dat$Bloomberg <- as.numeric(dat$Bloomberg)

dat$Warren <- recode(dat$Warren, "not asked" = "NA")
dat$Warren <- as.numeric(dat$Warren)

dat$poll.vote.margin <- (dat$Biden-dat$Sanders) - (dat$biden.vote - dat$sanders.vote)
dat$abs.poll.vote.margin <- abs(dat$poll.vote.margin)

mean(dat$poll.vote.margin,na.rm=TRUE)
median(dat$poll.vote.margin,na.rm=TRUE)

mean(abs(dat$poll.vote.margin),na.rm=TRUE)
median(abs(dat$poll.vote.margin),na.rm=TRUE)

dat$correct.pred <- NA

for(i in 1:nrow(dat)){
  if(dat$Biden[i] >= max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-1}
  if(dat$Biden[i] < max(c(dat$Bloomberg[i],dat$Sanders[i],dat$Warren[i]), na.rm=T)){dat$correct.pred[i]<-0}
}

mean(dat$correct.pred)

dat$Sanders.error <-  dat$sanders.vote - dat$Sanders
dat$Biden.error <- dat$biden.vote - dat$Biden
dat$Warren.error <- dat$warren.vote - dat$Warren
dat$Bloomberg.error <- dat$bloomberg.vote - dat$Bloomberg

WI <- dat

saveRDS(WI, file="Rdata/WIpolls14.RDS")
rm(dat)

#########################################################################
## Aggregating state primary polls
#########################################################################

setwd("~/RData")

files <- list.files()

for(i in 1:length(files)){
  readRDS(files[i])
}

AL$state <- "AL"
AR$state <- "AR"
CA$state <- "CA"
CO$state <- "CO"
IA$state <- "IA"
ID$state <- "ID"
MA$state <- "MA"
ME$state <- "ME"
MI$state <- "MI"
MN$state <- "MN"
MO$state <- "MO"
MS$state <- "MS"
NC$state <- "NC"
ND$state <- "ND"
NH$state <- "NH"
NV$state <- "NV"
OK$state <- "OK"
SC$state <- "SC"
TN$state <- "TN"
TX$state <- "TX"
UT$state <- "UT"
VA$state <- "VA"
VT$state <- "VT"
WA$state <- "WA"
AZ$state <- "AZ"
FL$state <- "FL"
IL$state <- "IL"
WI$state <- "WI"

# Binding datasets & filling in NAs for uncommon columns
allpolls <- rbind.fill(AL,AR,CA,CO,IA,ID,MA,ME,MI,MN,MO,MS,NC,ND,NH,NV,OK,SC,TN,TX,UT,VA,VT,WA,AZ,FL,IL,WI)

# Define Mode
allpolls$ModeRecode <- recode(allpolls$Mode, 
                              "Live phone" = "Live Phone",
                              "Live Phone/IVR" = "Other/Misc.",
                              "IVR" = "IVR/Online",
                              "Phone" = "Other/Misc.",
                              "proprietary technology" = "Other/Misc.",
                              "Phone/Online" = "Other/Misc.",
                              "unsure - see note" = "Other/Misc.")

allpolls$ModeRecode <- ifelse(is.na(allpolls$ModeRecode), "Other/Misc.", allpolls$ModeRecode)

# Indicator for polls conducted within a week of the election
allpolls$finalweek <- ifelse(allpolls$DaysToElection > -7, 1, 0)

finalweek <- subset(allpolls, DaysToElection > -7)

# Subset last poll only
lastpoll <- allpolls %>% 
  group_by(state) %>%
  slice(which.max(StartDate))

lastpoll <- as.data.frame(lastpoll)

saveRDS(allpolls,file="allpolls.RDS")
saveRDS(finalweek,file="finalweek.RDS")
saveRDS(lastpoll,file="lastpoll.RDS")

#########################################################################
## 2020 Pre-election Primary Polls: Number and Mode
#########################################################################

# Table 1. Mode of Data Collection for Pre-Election Primary Polls by Timing

all  <- table(allpolls$ModeRecode)
lastweek <- table(finalweek$ModeRecode)
final <- table(lastpoll$ModeRecode)

Num.Polls <- c(nrow(allpolls),nrow(finalweek),nrow(lastpoll))

goo <- rbind(all,lastweek,final)
goo <- cbind(goo,Num.Polls)
rownames(goo) <- c("Last Two Weeks","Last Week","Last Poll")
goo

# Figure 1: Number of polls by mode and state contest

supertuesdaystates <- c("AL","AR","CA","CO","ME","MA","MN","NC","OK","TN","TX","UT","VT","VA")

otherstates <- subset(allpolls, !state %in% supertuesdaystates)
a <- c("IA","NH","SC", "NV")
otherstates <- subset(otherstates, !state %in% a)

allpolls$group <- ifelse(allpolls$state %in% supertuesdaystates, "ST",
                         ifelse(allpolls$state %in% a, allpolls$state, "PostST"))

allpolls$group <- factor(allpolls$group, levels = c("PostST", "ST", "SC", "NV", "NH", "IA"))

groups <- allpolls %>% 
  group_by(group,ModeRecode) %>%
  tally()
groups <- data.frame(groups)

ggplot(groups, aes(fill=ModeRecode,x=group,y=n)) +
  geom_bar(position = "stack", stat="identity") +
  scale_x_discrete(labels=c("Post-Super \nTuesday", "Super \nTuesday", "SC", "NV", "NH", "IA")) +
  scale_fill_grey() +
  theme(text = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.ticks = element_blank()) +
  ylab("Number of Polls")  + ylim(0,80) + coord_flip()

#########################################################################
## Polling Errors: Overall & Historical
#########################################################################

# Table 2: Error outcomes for 2020 Democratic primary election polls

error2020 <- matrix(NA,nrow=3,ncol=3)

# Percent Correctly Predicted
error2020[1,1] <- round(mean(allpolls$correct.pred),digits=2)

# Absolute errors
error2020[2,1] <- round(mean(abs(allpolls$poll.vote.margin)),digits=2) # Mean Absolute Error
error2020[3,1] <- round(median(abs(allpolls$poll.vote.margin)),digits=2) # Median Absolute Error

# Signed errors
#error2020[4,1] <- round(mean(allpolls$poll.vote.margin),digits=2)  # Mean Signed Error
#error2020[5,1] <- round(median(allpolls$poll.vote.margin),digits=2) # Median Signed Error

# Percent Correctly Predicted
error2020[1,2] <- round(mean(finalweek$correct.pred),digits=2)

# Absolute errors
error2020[2,2] <- round(mean(abs(finalweek$poll.vote.margin)),digits=2) # Mean Absolute Error
error2020[3,2] <- round(median(abs(finalweek$poll.vote.margin)),digits=2) # Median Absolute Error

# Signed errors
#error2020[4,2] <- round(mean(finalweek$poll.vote.margin),digits=2)  # Mean Signed Error
#error2020[5,2] <- round(median(finalweek$poll.vote.margin),digits=2) # Median Signed Error

# Percent Correctly Predicted
error2020[1,3] <- round(mean(lastpoll$correct.pred),digits=2)

# Absolute errors
error2020[2,3] <- round(mean(abs(lastpoll$poll.vote.margin)),digits=2) # Mean Absolute Error
error2020[3,3] <- round(median(abs(lastpoll$poll.vote.margin)),digits=2) # Median Absolute Error

# Signed errors
#error2020[4,3] <- round(mean(lastpoll$poll.vote.margin),digits=2)  # Mean Signed Error
#error2020[5,3] <- round(median(lastpoll$poll.vote.margin),digits=2) # Median Signed Error

rownames(error2020) <- c("% Predicted Correct Winner",
                         "Average. Absolute Error",
                         "Median Absolute Error")

colnames(error2020) <- c("Last Two Weeks","Last Week","Last Poll")

error2020 <- rbind(error2020,Num.Polls)

error2020

# Table 3. US Primary Polling Errors Over Time, 2000-2020

PCP <- round(c(.99,1,.79,.64,.86,mean(allpolls$correct.pred),mean(finalweek$correct.pred)),digits=2)
AvgAbsErr <- round(c(7.7,7.0,7.6,8.3,9.3,mean(abs(allpolls$poll.vote.margin)),mean(abs(finalweek$poll.vote.margin))),digits=1)
Polls <- c(172,129,555,95,457,nrow(allpolls),nrow(finalweek))
histdat <- cbind(Polls,PCP,AvgAbsErr)
colnames(histdat) <-c("Num. Polls",
                      "% Correctly Predicted Winner",
                      "Avg. Absolute Error")

rownames(histdat) <-  c("2000","2004","2008","2012","2016","2020 (last two weeks)","2020 (last week)")

histdat

#########################################################################
## Polling Errors by Primary Contest
#########################################################################

# Table 4

setwd("/~RDS")

files <- list.files()
states <- str_sub(files, start=1L, end=2L)

AL <- readRDS(file=files[1])
AR <- readRDS(file=files[2])
AZ <- readRDS(file=files[3])
CA <- readRDS(file=files[4])
CO <- readRDS(file=files[5])
FL <- readRDS(file=files[6])
IA <- readRDS(file=files[7])
ID <- readRDS(file=files[8])
IL <- readRDS(file=files[9])
MA <- readRDS(file=files[10])
ME <- readRDS(file=files[11])
MI <- readRDS(file=files[12])
MN <- readRDS(file=files[13])
MO <- readRDS(file=files[14])
MS <- readRDS(file=files[15])
NC <- readRDS(file=files[16])
ND <- readRDS(file=files[17])
NH <- readRDS(file=files[18])
NV <- readRDS(file=files[19])
OK <- readRDS(file=files[20])
SC <- readRDS(file=files[21])
TN <- readRDS(file=files[22])
TX <- readRDS(file=files[23])
UT <- readRDS(file=files[24])
VA <- readRDS(file=files[25])
VT <- readRDS(file=files[26])
WA <- readRDS(file=files[27])
WI <- readRDS(file=files[28])

supertuesday <- subset(allpolls, state %in% supertuesdaystates)
otherstates <- subset(otherstates, !state %in% a)

ResultTable <- matrix(NA,nrow=6,ncol=4)
colnames(ResultTable) <- c("Polls","% Predicted","Signed Error","Abs Error")
rownames(ResultTable) <- c("Iowa","New Hampshire","Nevada","South Carolina","Super Tuesday","Post Super Tuesday")
ResultTable[1,1] <- nrow(IA)
ResultTable[1,2] <- round(mean(IA$correct.pred),digits=2)
ResultTable[1,3] <- round(mean(IA$poll.vote.margin,na.rm=TRUE),digits=1)
ResultTable[1,4] <- round(mean(abs(IA$poll.vote.margin),na.rm=TRUE),digits=1)
ResultTable[2,1] <- nrow(NH)
ResultTable[2,2] <- round(mean(NH$correct.pred),digits=2)
ResultTable[2,3] <- round(mean(NH$poll.vote.margin,na.rm=TRUE),digits=1)
ResultTable[2,4] <- round(mean(abs(NH$poll.vote.margin),na.rm=TRUE),digits=1)
ResultTable[3,1] <- nrow(NV)
ResultTable[3,2] <- round(mean(NV$correct.pred),digits=2)
ResultTable[3,3] <- round(mean(NV$poll.vote.margin,na.rm=TRUE),digits=1)
ResultTable[3,4] <- round(mean(abs(NV$poll.vote.margin),na.rm=TRUE),digits=1)
ResultTable[4,1] <- nrow(SC)
ResultTable[4,2] <- round(mean(SC$correct.pred),digits=2)
ResultTable[4,3] <- round(mean(SC$poll.vote.margin,na.rm=TRUE),digits=1)
ResultTable[4,4] <- round(mean(abs(SC$poll.vote.margin),na.rm=TRUE),digits=1)
ResultTable[5,1] <- nrow(supertuesday)
ResultTable[5,2] <- round(mean(supertuesday$correct.pred),digits=2)
ResultTable[5,3] <- round(mean(supertuesday$poll.vote.margin,na.rm=TRUE),digits=1)
ResultTable[5,4] <- round(mean(abs(supertuesday$poll.vote.margin),na.rm=TRUE),digits=1)
ResultTable[6,1] <- nrow(otherstates)
ResultTable[6,2] <- round(mean(otherstates$correct.pred),digits=2)
ResultTable[6,3] <- round(mean(otherstates$poll.vote.margin,na.rm=TRUE),digits=1)
ResultTable[6,4] <- round(mean(abs(otherstates$poll.vote.margin),na.rm=TRUE),digits=1)

ResultTable <- ResultTable[,-3]

colnames(ResultTable) <-c("Num. Polls",
                          "% Correctly Pred. Winner",
                          #    "Avg. Signed Error",
                          "Avg. Absolute Error")
ResultTable

# Figure 2. Average Absolute Error, early vs. late, by contest

earlypolls <- subset(allpolls, finalweek==0)
latepolls <- subset(allpolls, finalweek==1)

allpolls$time <- ifelse(allpolls$finalweek==0, "Second Week", "Final Week")

p <- ggerrorplot(allpolls, x = "group", y = "abs.poll.vote.margin",
                 color = "time", size = .7, desc_stat = "mean_ci") + 
  scale_x_discrete(labels=c("Post-Super \nTuesday", "Super \nTuesday", "South Carolina", "Nevada", "New \nHampshire", "Iowa")) +
  scale_color_grey() +
  xlab("") + ylab("Error (Absolute)") + theme(text = element_text(size = 12)) +  
  ggtitle("Polling Error by Contest and Timing \n(95% Confidence interval)") + 
  geom_hline(aes(yintercept=0),color="grey20", linetype="dashed") +
  coord_flip()

ggpar(p, legend = "right", legend.title = "Poll Release", yticks.by = 5)

#########################################################################
## Polling Errors by Mode
#########################################################################

# Table 5

corrpred <- round(tapply(allpolls$correct.pred, allpolls$ModeRecode, mean),digits=2)

meanabserror <- round(tapply(allpolls$abs.poll.vote.margin, allpolls$ModeRecode, mean),digits=1)
medabserror <- round(tapply(allpolls$abs.poll.vote.margin, allpolls$ModeRecode, median),digits=1)

errormode <- cbind(corrpred,meanabserror,medabserror)
errormode <- data.frame(errormode)

colnames(errormode) <- c("% Correctly Predict","Avg. Absolute Error","Median Absolute Error")
errormode

# Graphic visualization

npoll <- table(allpolls$ModeRecode)
ypos <- c(3,2,1,4) # To match plot

ggerrorplot(allpolls, x = "ModeRecode", y = "abs.poll.vote.margin",
            color = "ModeRecode", size = 1, desc_stat = "mean_ci") +
  xlab("") + ylab("Error (Absolute)") + theme(text = element_text(size = 12),
                                              legend.position = "none") +
  scale_colour_grey() + annotate("text",label=npoll,y=rep(12,times=4),x=ypos+.25) +
  coord_flip()

#########################################################################
## Appendix
#########################################################################

# Recoding mode

allpolls$ModeRecode <- recode(allpolls$Mode, 
                              "Live phone" = "Live Phone",
                              "Live Phone/IVR" = "Other/Misc.",
                              "IVR" = "IVR/Online",
                              "Phone" = "Other/Misc.",
                              "proprietary technology" = "Other/Misc.",
                              "Phone/Online" = "Other/Misc.",
                              "unsure - see note" = "Other/Misc.")

allpolls$ModeRecode <- ifelse(is.na(allpolls$Mode), "Other/Misc.", allpolls$Mode)

# Polling Errors by State Primary

allpolls$state <- factor(allpolls$state, levels = c("IA","NH","NV","SC",
                                                    "AL","AR","CA","CO",
                                                    "ME","MA","MN","NC",
                                                    "OK","TN","TX","UT",
                                                    "VT","VA","ID","MI",
                                                    "MS","MO","WA","ND",
                                                    "AZ","FL","IL","WI"))

nstatepoll <- table(allpolls$state)
nstatepoll <- nstatepoll[-c(24,28)]
ypos <- seq(1,length(nstatepoll))


ggerrorplot(allpolls, x = "state", y = "abs.poll.vote.margin",
            color = "state", size = .7, desc_stat = "mean_ci",
            remove = c("ND","WI")) + geom_hline(yintercept=0, linetype="dashed") +
  geom_vline(aes(xintercept=1), color="grey20", linetype="dotted") + geom_vline(aes(xintercept=2),color="grey20", linetype="dotted") +
  geom_vline(aes(xintercept=3),color="grey20", linetype="dotted") + geom_vline(aes(xintercept=4),color="grey20", linetype="dotted") +
  geom_vline(aes(xintercept=18),color="grey20", linetype="dotted") + geom_vline(aes(xintercept=23),color="grey20", linetype="dotted") +
  geom_vline(aes(xintercept=26),color="grey20", linetype="dotted") +
  xlab("") + ylab("Error (Absolute)") + theme(text = element_text(size = 10),
                                              legend.position = "none") +
  annotate("text", x=c(.75,1.75,2.75,3.75,17.75,22.75,25.75), y=-75,
           label=c("Feb. 3","Feb. 11","Feb. 22","Feb. 29","March 3","March 10","March 17"), 
           size=3, color="black") + scale_colour_grey() +  annotate("text",label=nstatepoll,y=rep(75,times=length(nstatepoll)),x=ypos,cex=3) +
  coord_flip()







