#####################################
# PLSC 502 -- Fall 2016
#
# Day Twenty materials
#####################################
# Packages, etc.:

require(RCurl) 
require(DescTools)
require(car)

options(digits=5)

# Simulate a bunch of 2x2 crosstabs, calculating gamma
# and the taus for each, then plot them against each
# other:

NTs <- 101
GAMMA <- numeric(NTs)
TauA<- numeric(NTs)
TauB<- numeric(NTs)
TauC<- numeric(NTs)

for (i in 1:NTs) { 
  ST <- as.table(rbind(c(NTs-i,i-1),c(i-1,NTs-i)))
  GAMMA[i] <- GoodmanKruskalGamma(ST)
  TauA[i] <- KendallTauA(ST)
  TauB[i] <- KendallTauB(ST)
  TauC[i] <- StuartTauC(ST)
}

dsts <- data.frame(Gamma=GAMMA,TauA=TauA,TauB=TauB,TauC=TauC)

pdf("GammasAndTaus.pdf",7,7)
par(mar=c(4,4,2,2))
scatterplotMatrix(dsts)
dev.off()

# Get Palin data:

SPURL<-"https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2016-git/master/Data/Palin.csv"
temp<-getURL(SPURL)
MamaGriz<-read.csv(textConnection(temp))
rm(temp,SPURL)

MamaGriz$palin <- ordered(MamaGriz$palin,
                  levels=c("Very Unfavorable","Somewhat Unfavorable",
                           "Somewhat Favorable","Very Favorable"))
MamaGriz$pid <- ordered(MamaGriz$pid,levels=c("Democrat",
                        "Independent","GOP"))
MamaGriz$female <- ordered(MamaGriz$female,levels=c("Male",
                                          "Female"))
summary(MamaGriz)

palinpid<-with(MamaGriz, xtabs(~palin+pid))
addmargins(palinpid)

# Gamma:

GoodmanKruskalGamma(palinpid,conf.level=0.95)

#Tau-A:

KendallTauA(palinpid,conf.level=0.95)

# Tau-B:

KendallTauB(palinpid,conf.level=0.95)

# Tau-C:

StuartTauC(palinpid,conf.level=0.95)

# Male vs. Female:

palinfemale<-with(MamaGriz, xtabs(~palin+female))
addmargins(palinfemale)

GoodmanKruskalGamma(palinfemale,conf.level=0.95)
KendallTauA(palinfemale,conf.level=0.95)
KendallTauB(palinfemale,conf.level=0.95)
StuartTauC(palinfemale,conf.level=0.95)

