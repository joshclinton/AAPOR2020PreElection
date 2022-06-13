setwd("/Users/clintojd/")
source(file="Google Drive/AAPOR.PreElectionPolling/Code/AdjustSienaPollsToVoterFileGA.R")
source(file="Google Drive/AAPOR.PreElectionPolling/Code/AdjustSienaPollsToVoterFileNV.R")
source(file="Google Drive/AAPOR.PreElectionPolling/Code/AdjustSienaPollsToVoterFileNC.R")
source(file="Google Drive/AAPOR.PreElectionPolling/Code/AdjustSienaPollsToVoterFileMI.R")
source(file="Google Drive/AAPOR.PreElectionPolling/Code/AdjustSienaPollsToVoterFilePA.R")
source(file="Google Drive/AAPOR.PreElectionPolling/Code/AdjustSienaPollsToVoterFileKS.R")
source(file="Google Drive/AAPOR.PreElectionPolling/Code/AdjustSienaPollsToVoterFileFL.R")
source(file="Google Drive/AAPOR.PreElectionPolling/Code/AdjustSienaPollsToVoterFileAZ.R")
source(file="Google Drive/AAPOR.PreElectionPolling/Code/AdjustSienaPollsToVoterFileWI.R")
source(file="Google Drive/AAPOR.PreElectionPolling/Code/AdjustSienaPollsToVoterFileIA.R")
source('~/Google Drive/AAPOR.PreElectionPolling/Code/AdjustSienaPollsToVoterFileMT.R')

# New Voters
sum(ChangeAZ[6,])/sum(ChangeAZ)
ChangePA[6,]/sum(ChangeAZ[6,])

sum(ChangeGA[6,])/sum(ChangeGA)
ChangeGA[6,]/sum(ChangeGA[6,])

a <- 1.5



png(file="Google Drive/AAPOR.PreElectionPolling/Writing/Figures/Fig16_SienaAdjustmentsTrump2016.png",
    width=700, height=1000)
plot(as.numeric(tb.trump16.MI)[c(1,2,6)],c(1,2,3),axes=FALSE,ylab="",cex.main=2,cex.lab=1.5,
     xlab="Difference in Reweighted %",
     main="Effect of Reweighting to 2020 Outcome: \n 2016 Self-Reported Vote",
     xlim=c(-10,5),ylim=c(1,4),type="n")
polygon(c(-20,20,20,-20),c(1,1,2,2),col="lightgrey",border=NA)
polygon(c(-20,20,20,-20),c(3,3,4,4),col="lightgrey",border=NA)
axis(1,at=c(-5,0,5),lab=c("-5","0","5"))
abline(v=seq(-4,4,by=1),lty=2,col="grey")
abline(v=0,col="Grey")

text(as.numeric(tb.trump16.GA)[c(1,2,6)],seq(1.05,3.05),c("GA","GA","GA"),cex=a)
text(as.numeric(tb.trump16.NV)[c(1,2,6)],seq(1.1,3.1),c("NV","NV","NV"),cex=a)
text(as.numeric(tb.trump16.NC)[c(1,2,6)],seq(1.2,3.2),c("NC","NC","NC"),cex=a)
text(as.numeric(tb.trump16.MI)[c(1,2,6)],seq(1.3,3.3),c("MI","MI","MI"),cex=a)
text(as.numeric(tb.trump16.PA)[c(1,2,6)],seq(1.4,3.4),c("PA","PA","PA"),cex=a)
text(as.numeric(tb.trump16.KS)[c(1,2,6)],seq(1.5,3.5),c("KS","KS","KS"),cex=a)
text(as.numeric(tb.trump16.FL)[c(1,2,6)],seq(1.6,3.6),c("FL","FL","FL"),cex=a)
text(as.numeric(tb.trump16.AZ)[c(1,2,6)],seq(1.7,3.7),c("AZ","AZ","AZ"),cex=a)
text(as.numeric(tb.trump16.WI)[c(1,2,6)],seq(1.8,3.8),c("WI","WI","WI"),cex=a)
text(as.numeric(tb.trump16.IA)[c(1,2,6)],seq(1.88,3.88),c("IA","IA","IA"),cex=a)
text(as.numeric(tb.trump16.MT)[c(1,2,6)],seq(1.95,3.95),c("MT","MT","MT"),cex=a)
text(-10,1.5,"Trump 2016 Vote",pos=4,cex=1.5)
text(-10,2.5,"Clinton 2016 Vote",pos=4,cex=1.5)
text(-10,3.5,"No 2016 Vote",pos=4,cex=1.5)
dev.off()

TrumpStrata.PA
BidenStrata.PA
TrumpStrata.MT
BidenStrata.MT


png(file="Google Drive/AAPOR.PreElectionPolling/Writing/Figures/Fig3B_SienaAdjustmentsStrataTrump2020.png",
    width=700, height=1000)
plot(as.numeric(TrumpStrata.PA),seq(1,5),axes=FALSE,ylab="",cex.main=2,cex.lab=1.5,
     xlab="Difference in Reweighted %",
     main="Effect of Reweighting to 2020 Outcome: \n 2020 Trump Vote",
     xlim=c(-10,5),ylim=c(1,6),type="n")
polygon(c(-20,20,20,-20),c(1,1,2,2),col="grey81",border=NA)
polygon(c(-20,20,20,-20),c(3,3,4,4),col="grey81",border=NA)
polygon(c(-20,20,20,-20),c(5,5,6,6),col="grey81",border=NA)
axis(1)
abline(v=seq(-4,4,by=1),lty=2,col="Grey")
abline(v=0,col="Grey")
text(TrumpStrata.FL*100,seq(1.1,5.1),c("FL","FL","FL","FL","FL"),cex=a)
text(TrumpStrata.GA*100,seq(1.2,5.2),c("GA","GA","GA","GA","GA"),cex=a)
text(TrumpStrata.IA*100,seq(1.3,5.3),c("IA","IA","IA","IA","IA"),cex=a)
text(TrumpStrata.KS*100,seq(1.4,5.4),c("KS","KS","KS","KS","KS"),cex=a)
text(TrumpStrata.MI*100,seq(1.5,5.5),c("MI","MI","MI","MI","MI"),cex=a)
text(TrumpStrata.MT*100,seq(1.6,5.6),c("MT","MT","MT","MT","MT"),cex=a)
text(TrumpStrata.NC*100,seq(1.7,5.7),c("NC","NC","NC","NC","NC"),cex=a)
text(TrumpStrata.PA*100,seq(1.8,5.8),c("PA","PA","PA","PA","PA"),cex=a)
text(TrumpStrata.WI*100,seq(1.9,5.9),c("WI","WI","WI","WI","WI"),cex=a)

text(-10,1.5,"Solid Democrat",pos=4,cex=1.5)
text(-10,2.5,"Lean Democrat",pos=4,cex=1.5)
text(-10,3.5,"Toss Up",pos=4,cex=1.5)
text(-10,4.5,"Lean Republican",pos=4,cex=1.5)
text(-10,5.5,"Solid Republican",pos=4,cex=1.5)
dev.off()

ChangeNC 



png(file="Google Drive/AAPOR.PreElectionPolling/Writing/Figures/Fig15_SienaAdjustmentsEducation.png",
    width=700, height=1000)
plot(as.numeric(tb.educ.MI)[-5],seq(1,4),axes=FALSE,ylab="",cex.main=2,cex.lab=1.5,
     xlab="Difference in Reweighted %",
     main="Effect of Reweighting to 2020 Outcome: \n Education",
     xlim=c(-10,5),ylim=c(1,5),type="n")
polygon(c(-20,20,20,-20),c(1,1,2,2),col="grey81",border=NA)
polygon(c(-20,20,20,-20),c(3,3,4,4),col="grey81",border=NA)
axis(1,at=c(-5,0,5),lab=c("-5","0","5"))
abline(v=0,col="Grey")
abline(v=seq(-4,4,by=1),lty=2,col="Grey")


text(as.numeric(tb.educ.GA)[-5],seq(1.05,4.05),c("GA","GA","GA","GA"),cex=a)
text(as.numeric(tb.educ.NV)[-5],seq(1.1,4.1),c("NV","NV","NV","NV"),cex=a)
text(as.numeric(tb.educ.NC)[-5],seq(1.2,4.2),c("NC","NC","NC","NC"),cex=a)
text(as.numeric(tb.educ.MI)[-5],seq(1.3,4.3),c("MI","MI","MI","MI"),cex=a)
text(as.numeric(tb.educ.PA)[-5],seq(1.4,4.4),c("PA","PA","PA","PA"),cex=a)
text(as.numeric(tb.educ.KS)[-5],seq(1.5,4.5),c("KS","KS","KS","KS"),cex=a)
text(as.numeric(tb.educ.FL)[-5],seq(1.6,4.6),c("FL","FL","FL","FL"),cex=a)
text(as.numeric(tb.educ.AZ)[-5],seq(1.7,4.7),c("AZ","AZ","AZ","AZ"),cex=a)
text(as.numeric(tb.educ.WI)[-5],seq(1.8,4.8),c("WI","WI","WI","WI"),cex=a)
text(as.numeric(tb.educ.IA)[-5],seq(1.88,4.88),c("IA","IA","IA","IA"),cex=a)
text(as.numeric(tb.educ.MT)[-5],seq(1.95,4.95),c("MT","MT","MT","MT"),cex=a)
text(-10,1.5,"HS or less",pos=4,cex=1.5)
text(-10,2.5,"Some college",pos=4,cex=1.5)
text(-10,3.5,"Bachelors' degree",pos=4,cex=1.5)
text(-10,4.5,"Graduate degree",pos=4,cex=1.5)
dev.off()

#
#     CNN
#

source(file="Google Drive/AAPOR.PreElectionPolling/Code/AdjustCNNPollsToVoterFileMI.R")
source(file="Google Drive/AAPOR.PreElectionPolling/Code/AdjustCNNPollsToVoterFileNC.R")
source(file="Google Drive/AAPOR.PreElectionPolling/Code/AdjustCNNPollsToVoterFileFL.R")
source(file="Google Drive/AAPOR.PreElectionPolling/Code/AdjustCNNPollsToVoterFileWI.R")
source(file="Google Drive/AAPOR.PreElectionPolling/Code/AdjustCNNPollsToVoterFilePA.R")
source(file="Google Drive/AAPOR.PreElectionPolling/Code/AdjustCNNPollsToVoterFileAZ.R")
tb.party5.MI
tb.party5.NC
tb.party5.FL
tb.party5.WI
tb.party5.PA
tb.party5.AZ

png(file="Google Drive/AAPOR.PreElectionPolling/Writing/Figures/Fig17_CNNAdjustmentsParty5.png",
    width=700, height=1000)
plot(as.numeric(tb.party5.AZ),c(1,2,3,4,5),axes=FALSE,ylab="",cex.main=2,cex.lab=1.5,
     xlab="Difference in Reweighted %",
     main="Effect of Reweighting to 2020 Outcome: \n Self-Reported Partisanship",
     xlim=c(-.1,.05),ylim=c(1,6),type="n")
polygon(c(-.2,.2,.2,-.2),c(1,1,2,2),col="grey81",border=NA)
polygon(c(-.2,.2,.2,-.2),c(3,3,4,4),col="grey81",border=NA)
polygon(c(-.2,.2,.2,-.2),c(5,5,6,6),col="grey81",border=NA)
axis(1,at=c(-.05,0,.05),lab=c("-5","0","5"))
abline(v=seq(-.04,.04,by=.01),lty=2,col="Grey")
abline(v=0,col="Grey")

text(as.numeric(tb.party5.AZ),seq(1.1,5.1),c("AZ","AZ","AZ","AZ","AZ"),cex=a)
text(as.numeric(tb.party5.NC),seq(1.3,5.3),c("NC","NC","NC","NC","NC"),cex=a)
text(as.numeric(tb.party5.FL),seq(1.4,5.4),c("FL","FL","FL","FL","FL"),cex=a)
text(as.numeric(tb.party5.MI),seq(1.6,5.6),c("MI","MI","MI","MI","MI"),cex=a)
text(as.numeric(tb.party5.PA),seq(1.75,5.75),c("PA","PA","PA","PA","PA"),cex=a)
text(as.numeric(tb.party5.PA),seq(1.9,5.9),c("WI","WI","WI","WI","WI"),cex=a)

text(-.1,1.5,"Strong Dem.",pos=4,cex=1.5)
text(-.1,2.5,"Democrat",pos=4,cex=1.5)
text(-.1,3.5,"Ind. (No Lean)",pos=4,cex=1.5)
text(-.1,4.5,"Republican",pos=4,cex=1.5)
text(-.1,5.5,"Strong Rep.",pos=4,cex=1.5)
dev.off()

