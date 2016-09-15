#####################################
# PLSC 502 -- Fall 2016
#
# Day Six materials
#####################################

library(RCurl)

# Get NFL (2016) data:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2016-git/master/Data/NFL2016.csv")
NFL<-read.csv(text=temp, header=TRUE)
rm(temp)

# Create the big table of values:

NFL$mediandiff<- NFL$points-median(NFL$points)
NFL$meandiff<- NFL$points-mean(NFL$points)
NFL$absmeandiff<- abs(NFL$points-mean(NFL$points))
NFL$meandiffsquared<- (NFL$points-mean(NFL$points))^2

NFLtotalpoints<-sum(NFL$points)
NFLtotalmediandiff<-sum(NFL$mediandiff)
NFLtotalmeandiff<-sum(NFL$meandiff)
NFLtotalabsmeandiff<-sum(NFL$absmeandiff)
NFLtotalmeandiffsq<-sum(NFL$meandiffsquared)

# Variance and SD:

with(NFL, summary(points))
with(NFL, var(points))
with(NFL, sd(points))

# Hypothetical Detroit:

NFL$points2<-NFL$points
NFL$points2<-ifelse(NFL$team=="Lions",89,NFL$points2)
summary(NFL$points2)
sd(NFL$points2)

# Plots illustrating skewness:

library(moments)

set.seed(7222009)
p<-10
q<-1.5
m<-(p+q)/2
ndraws<-500
left<-rbeta(ndraws,p,q)
noskew<-rbeta(ndraws,m,m)
right<-rbeta(ndraws,q,p)
lskew<-round(skewness(left),3) # skewness...
sskew<-round(skewness(noskew),3)
rskew<-round(skewness(right),3)

pdf("EmpiricalMeanMedianModeR.pdf",7,5)
par(mfrow=c(3,1))
par(mar=c(4,4,2,2))
# right-skewed:
hist(right,main="Right-Skewed",prob=TRUE,ylim=c(0,5),
     xlab="X",ylab="Density",col="grey",xlim=c(0,1))
lines(density(right),lwd=3)
legend("topright",legend=paste0("Skewness = ",rskew),bty="n")
# symmetrical:
par(mar=c(4,2,2,2))
hist(noskew,main="Symmetrical",prob=TRUE,ylim=c(0,3),
     xlab="X",col="grey",xlim=c(0,1))
lines(density(noskew),lwd=3)
legend("topright",legend=paste0("Skewness = ",sskew),bty="n")
# left-skewed:
par(mar=c(4,2,2,2))
hist(left,main="Left-Skewed",prob=TRUE,ylim=c(0,5.5),
     xlab="X",col="grey",xlim=c(0,1))
lines(density(left),lwd=3)
legend("topleft",legend=paste0("Skewness = ",lskew),bty="n")
dev.off()

# Plots illustrating kurtosis:

z<-seq(-5,5,by=0.01)
lw<-3
pdf("KurtosisR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(z,dcauchy(z,scale=0.5),t="l",lwd=lw,col="red",
  ylab="Probability",xlab="X")
lines(z,dnorm(z),lwd=lw,col="black",lty=2)
lines(z,dunif(z,min=-2,max=2),lwd=lw,col="darkgreen",lty=3)
legend("topleft",bty="n",lty=c(1,2,3),lwd=lw,
       legend=c("Leptokurtic","Mesokurtic","Platykurtic"),
       col=c("red","black","darkgreen"))
dev.off()

# Alternative (not on the slides...):

ndraws<-200
set.seed(7222009)
lepto<-rcauchy(ndraws,0,1)
meso<-rnorm(ndraws,0,1)
platy<-runif(ndraws,-1,1)
lkurt<-round(kurtosis(lepto),3)
mkurt<-round(kurtosis(meso),3)
pkurt<-round(kurtosis(platy),3)

bins<-30
pdf("EmpiricalKurtosisR.pdf",6,8)
par(mfrow=c(3,1))
par(mar=c(4,4,2,2))
hist(lepto,prob=TRUE,main="Leptokurtic",xlab="X",breaks=bins,
     ylim=c(0,0.28),xlim=c(-20,20))
lines(density(lepto),lwd=2,xlim=c(0,1))
legend("topleft",legend=paste0("Kurtosis = ",lkurt),bty="n")
hist(meso,prob=TRUE,main="Mesokurtic",xlab="X",breaks=bins)
lines(density(meso),lwd=2,xlim=c(0,1))
legend("topleft",legend=paste0("Kurtosis = ",mkurt),bty="n")
hist(platy,prob=TRUE,main="Platykurtic",xlab="X",breaks=bins)
lines(density(platy),lwd=2,xlim=c(0,1))
legend("topright",legend=paste0("Kurtosis = ",pkurt),bty="n")
dev.off()

# Skewness and kurtiosis of NFL points variable:

library(moments)
with(NFL, skewness(points))
with(NFL, kurtosis(points))


# Add the NFC variable to the NFL data:

NFCteams<-as.character(c("Redskins","Giants","Eagles","Cowboys",
            "Bears","Vikings","Lions","Packers",
            "Panthers","Saints","Falcons","Buccaneers",
            "Rams","49ers","Seahawks","Cardinals"))
NFL$NFC<-ifelse(NFL$team %in% NFCteams,1,0)

# Summary statistics for "NFC":

with(NFL, summary(NFC))
with(NFL, var(NFC))
with(NFL, sd(NFC))
with(NFL, mad(NFC))
with(NFL, skewness(NFC))
with(NFL, kurtosis(NFC))

