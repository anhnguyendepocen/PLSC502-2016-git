#####################################
# PLSC 502 -- Fall 2016
#
# Day Twelve materials
#####################################
#
# Packages:

require(Hmisc)

# Warren & Burger Court data:

WBURL<-"https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2016-git/master/Data/WarrenBurger.csv"
temp<-getURL(WBURL)
WB<-read.csv(textConnection(temp))
rm(temp,WBURL)

summary(WB)

# Histograms

pdf("FedPetHistogram.pdf",5,4)
par(mar=c(4,4,2,2))
with(WB, barplot(table(fedpet),
         names.arg=c("No Federal Petitioner","Federal Petitioner")))
dev.off()

pdf("SumAmHistogram.pdf",5,4)
par(mar=c(4,4,2,2))
with(WB, hist(sumam,col="grey",main="",
              xlab="Total Amici Filed"))
dev.off()

# Draw 1000 random samples of fedpet, each with N=10, 
# and calculate the mean for each:

set.seed(7222009)
MFP10<-numeric(1000)
for (i in 1:1000){
  MFP10[i]<- with(WB, mean(sample(fedpet,10,replace=F)))
}

# describe(MFP10)

pdf("FedPetMeansN-is-10R.pdf",5,4)
par(mar=c(4,4,2,2))
hist(MFP10,breaks=7,xlab="Sample Means of FedPet",col="grey",
     main="",freq=FALSE)
abline(v = mean(WB$fedpet),lwd=3,lty=2)
legend("topright",bty="n",lwd=3,col="black",lty=2,
       legend="Population mean")
dev.off()

# Same with N = 20:

set.seed(7222009)
MFP20<-numeric(1000)
for (i in 1:1000){
  MFP20[i]<- with(WB, mean(sample(fedpet,20,replace=F)))
}

# describe(MFP20)

pdf("FedPetMeansN-is-20R.pdf",5,4)
par(mar=c(4,4,2,2))
hist(MFP20,breaks=10,xlab="Sample Means of FedPet",col="grey",
     main="",freq=FALSE)
abline(v = mean(WB$fedpet),lwd=3,lty=2)
legend("topright",bty="n",lwd=3,col="black",lty=2,
       legend="Population mean")
dev.off()

# Same with N = 100:

set.seed(7222009)
MFP100<-numeric(1000)
for (i in 1:1000){
  MFP100[i]<- with(WB, mean(sample(fedpet,100,replace=F)))
}

# describe(MFP100)

pdf("FedPetMeansN-is-100R.pdf",5,4)
par(mar=c(4,4,2,2))
hist(MFP100,breaks=20,xlab="Sample Means of FedPet",col="grey",
     main="",freq=FALSE)
abline(v = mean(WB$fedpet),lwd=3,lty=2)
lines(seq(0,0.5,by=0.001),dnorm(seq(0,0.5,by=0.001),
      mean=mean(WB$fedpet),sd=(sd(WB$fedpet)/sqrt(100))),
      lwd=2,col="red")
legend("topright",bty="n",lwd=c(3,2),col=c("black","red"),
       lty=c(2,1),
       legend=c("Population mean","Normal density"))
dev.off()

# Same with SUMAM (N = 100):

set.seed(7222009)
AM100<-numeric(1000)
for (i in 1:1000){
  AM100[i]<- with(WB, mean(sample(sumam,100,replace=F)))
}

# describe(AM100)

pdf("SumAmMeansN-is-100R.pdf",5,4)
par(mar=c(4,4,2,2))
hist(AM100,breaks=20,xlab="Sample Means of SumAm",col="grey",
     main="",freq=FALSE,ylim=c(0,2))
abline(v = mean(WB$sumam),lwd=3,lty=2)
lines(seq(0,3,by=0.01),dnorm(seq(0,3,by=0.01),
      mean=mean(WB$sumam),sd=(sd(WB$sumam)/sqrt(100))),
      lwd=2,col="red")
legend("topright",bty="n",lwd=c(3,2),col=c("black","red"),
       lty=c(2,1),
       legend=c("Population mean","Normal density"))
dev.off()


# Next, variances...
#
# Remember that [(N-1)*s^2] / sigma^2 is chi-square with N-1 d.f.
#
# Draw 1000 random samples of fedpet, each with N=20, and 
# calculate the scaled variance for each:

s20<-numeric(1000)
for(i in 1:1000){ 
   s20[i]<- with(WB, 
          (19*var(sample(fedpet,20,replace=F)))/(var(fedpet)))
}

pdf("FedPetVarsN20R.pdf",5,4)
par(mar=c(4,4,4,2))
hist(s20,breaks=10,col="grey",freq=FALSE,ylim=c(0,0.08),main="N=20",
     xlab="Rescaled Sample Variances of FedPet")
lines(seq(0,40,length=401),dchisq(seq(0,40,length=401),19),
         lwd=2,col="red")
abline(v=19,lwd=3,lty=2,col="black")
legend("topleft",bty="n",lwd=2,col="red",
       legend="Chi-Square(19)")
dev.off()

# Same with larger samples (N = 500):

s500<-numeric(1000)
for(i in 1:1000){ 
  s500[i]<- with(WB, 
            (499*var(sample(fedpet,500,replace=F)))/(var(fedpet)))
}

pdf("FedPetVarsN500R.pdf",5,4)
par(mar=c(4,4,4,2))
hist(s500,breaks=20,col="grey",freq=FALSE,ylim=c(0,0.02),main="N=500",
     xlab="Rescaled Sample Variances of FedPet")
lines(seq(350,600,by=1),dchisq(seq(350,600,by=1),499),
      lwd=2,col="red")
abline(v=499,lwd=3,lty=2)
legend("topleft",bty="n",lwd=2,col="red",
       legend="Chi-Square(499)")
dev.off()

###################################
# Stratified sampling...

# Summary of the CONSTIT variable:

table(WB$constit)

# install.packages("sampling") # <-- run if necessary
require(sampling)

# Draw a single stratified random sample, with 10 observations
# from constit=0 and 10 from constit=1

set.seed(7222009)
sample<-strata(WB,stratanames=c("constit"),
                        size=c(10,10),method="srswor")
sample.data<-getdata(WB,sample)
summary(sample.data)


