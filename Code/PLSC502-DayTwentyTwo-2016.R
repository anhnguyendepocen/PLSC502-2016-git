#####################################
# PLSC 502 -- Fall 2016
#
# Day Twenty-Two materials
#####################################
# Packages, etc.:

options(digits=2)

require(RCurl) 
require(DescTools)
require(car)
require(mvtnorm)

# First hypothetical scatterplot with regression lines:

set.seed(7222009)
X<-runif(100,-5,5)
Y<-6+X+rnorm(100) # B0=6, B1=1

pdf("VariousRegressionLinesR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(X,Y,pch=19)
abline(v=0,lty=3,col="grey")
abline(lm(Y~X),lwd=3)
abline(a=8,b=1,lwd=3,lty=2,col="red")
abline(a=6,b=2,lwd=3,lty=3,col="darkgreen")
dev.off()

# World's simplest regression:

x <- c(1,2)
y <- c(3,5)
d <- data.frame(x=x,y=y)
d

pdf("WorldsSimplestRegressionR.pdf",5,5)
par(mar=c(4,4,2,2))
plot(d,pch=20,xlab="X",ylab="Y",xlim=c(0,3),ylim=c(2,6))
dev.off()

# Court examples:

##########################
# This is code to aggregate SCOTUS votes from the
# raw vote data.
require(plyr)

SCURL<-"https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2016-git/master/Data/SCDB2016.csv"
temp<-getURL(SCURL)
SCDB<-read.csv(textConnection(temp))
rm(temp,SCURL)

CivLib<-SCDB[SCDB$issueArea==2,]
CivLib$LibVote<-CivLib$direction-1
Justices <- ddply(CivLib,.(justiceName),summarize,
                  justice = mean(justice),
                  civlibs = mean(LibVote,na.rm=TRUE)*100)

SC<-read.csv("https://gist.githubusercontent.com/jeremyjbowers/f36efe6db30056b1a587/raw/12c06863f944515bbd3122ac7f0461219c424edd/segal_cover_scores.csv")
SC<-SC[,c(1,7)]
SCPct <- merge(Justices,SC,by=c("justice"),all=TRUE)
SCPct <- SCPct[complete.cases(SCPct),] # ditch missing data
write.csv(SCPct,"NewSCOTUS.csv")

##########################
library(data.table)
SCOTUS<-fread("https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2016-git/master/Data/NewSCOTUS.csv")
SCOTUS$V1 <- NULL
summary(SCOTUS)

pdf("SCOTUSScatterR.pdf",6,5)
par(mar=c(4,4,2,2))
with(SCOTUS, plot(ideology_score,civlibs,pch="",xlim=c(-0.2,1.2),
                  xlab="Editorial-Based Liberalism",
                  ylab="Pro-Civil Rights Voting Percentage"))
with(SCOTUS, text(ideology_score,civlibs,labels=justiceName))
abline(v=mean(SCOTUS$ideology_score),lty=3)
abline(h=mean(SCOTUS$civlibs),lty=3)
abline(lm(civlibs~ideology_score, SCOTUS),lwd=2)
dev.off()

# Beta1:

Beta1 <- with(SCOTUS, (sum((ideology_score - mean(ideology_score)) * 
                           (civlibs - mean(civlibs))) / 
                        sum((ideology_score - mean(ideology_score))^2)))
Beta1

Beta0 <- with(SCOTUS, mean(civlibs) - (Beta1 * mean(ideology_score)))
Beta0

# Residuals:

SCOTUS$Yhats <- with(SCOTUS, Beta0 + Beta1*ideology_score)
SCOTUS$Uhats <- with(SCOTUS, civlibs - Yhats)
describe(SCOTUS$civlibs)
describe(SCOTUS$Yhats)
describe(SCOTUS$Uhats)

pdf("SCOTUSYhats.pdf",6,5)
par(mar=c(4,4,2,2))
with(SCOTUS, hist(Yhats,col="grey",main="",
                  breaks=8,xlab="Predicted Y Values"))
dev.off()

pdf("SCOTUSresids.pdf",6,5)
par(mar=c(4,4,2,2))
with(SCOTUS, hist(Uhats,col="grey",main="",
     breaks=8,xlab="Residual Values"))
dev.off()

# Sums of squares:

TotalYVar <- with(SCOTUS, sum((civlibs - mean(civlibs))^2))
TotalYVar

TotalUVar <- with(SCOTUS, sum((Uhats)^2))
TotalUVar

TotalModelVar <- with(SCOTUS, sum((Yhats - mean(civlibs))^2))
TotalModelVar

RSE <- with(SCOTUS, sqrt(TotalUVar / (nrow(SCOTUS)-2)))
RSE
  
# Using lm: 

with(SCOTUS, summary(lm(civlibs~ideology_score)))
