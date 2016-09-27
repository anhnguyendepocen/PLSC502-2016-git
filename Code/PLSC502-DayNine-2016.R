#####################################
# PLSC 502 -- Fall 2016
#
# Day Nine materials
#####################################

# Bernoulli PDF (pi = 0.6):

Bern <- data.frame(X = c(0,1),
                   P = c(0.4,0.6))
pdf("BernoulliPDF-R.pdf",6,5)
par(mar=c(4,4,2,2))
with(Bern, plot(X,P,pch=20,xlim=c(-0.5,1.5),ylim=c(0,1),
     cex=3,xlab="Values of X",ylab="Pr(X=x)"))
segments(0,0,0,0.4,lty=2,lwd=4)
segments(1,0,1,0.6,lty=2,lwd=4)
dev.off()

# Binomial:

binomdata<-data.frame(count=seq(0,12),
                      B404 = dbinom(seq(0,12),4,0.4),
                      B1004 = dbinom(seq(0,12),10,0.4),
                      B408 = dbinom(seq(0,12),4,0.8),
                      B1008 = dbinom(seq(0,12),10,0.8))

pdf("BinomialsR.pdf",9,9)
par(mfrow=c(2,2))
par(mar=c(4,4,4,2))
# n=4, p=0.4
with(binomdata, plot(count,B404,pch=20,cex=3,xlab="Successes",
                     ylim=c(0,0.42),main="n = 4, pi = 0.4"))
for (i in 1:13) {
  with(binomdata, segments(count[i],0,count[i],B404[i],
                           lty=2,lwd=4))
}
# n=10, p=0.4
with(binomdata, plot(count,B1004,pch=20,cex=3,xlab="Successes",
                     ylim=c(0,0.42),main="n = 10, pi = 0.4"))
for (i in 1:13) {
  with(binomdata, segments(count[i],0,count[i],B1004[i],
                           lty=2,lwd=4))
}
# n=4, p=0.8
with(binomdata, plot(count,B408,pch=20,cex=3,xlab="Successes",
                     ylim=c(0,0.42),main="n = 4, pi = 0.8"))
for (i in 1:13) {
  with(binomdata, segments(count[i],0,count[i],B408[i],
                           lty=2,lwd=4))
}
# n=10, p=0.8
with(binomdata, plot(count,B1008,pch=20,cex=3,xlab="Successes",
                     ylim=c(0,0.42),main="n = 10, pi = 0.8"))
for (i in 1:13) {
  with(binomdata, segments(count[i],0,count[i],B1008[i],
                           lty=2,lwd=4))
}
dev.off()

# Geometrics:

geomdata<-data.frame(count=seq(0,12),
                    G02 = dgeom(seq(0,12),0.2),
                    G05 = dgeom(seq(0,12),0.5),
                    G08 = dgeom(seq(0,12),0.8))

pdf("GeometricsR.pdf",8,6)
par(mfrow=c(1,3))
par(mar=c(4,4,4,2))
# p=0.2
with(geomdata, plot(count,G02,pch=20,cex=3,xlab="Trials",
                     ylim=c(0,1),main="pi = 0.2"))
for (i in 1:13) {
  with(geomdata, segments(count[i],0,count[i],G02[i],
                           lty=2,lwd=3))
}
# p=0.5
with(geomdata, plot(count,G05,pch=20,cex=3,xlab="Trials",
                    ylim=c(0,1),main="pi = 0.5"))
for (i in 1:13) {
  with(geomdata, segments(count[i],0,count[i],G05[i],
                          lty=2,lwd=3))
}
# p=0.8
with(geomdata, plot(count,G08,pch=20,cex=3,xlab="Trials",
                    ylim=c(0,1),main="pi = 0.8"))
for (i in 1:13) {
  with(geomdata, segments(count[i],0,count[i],G08[i],
                          lty=2,lwd=3))
}
dev.off()

# Various Poisson histograms:

set.seed(7222009)
N<-1000
LP05<-rpois(N,0.5)
LP1<-rpois(N,1)
LP5<-rpois(N,5)
LP10<-rpois(N,10)

pdf("PoissonHistogramsR.pdf",7,6)
par(mfrow=c(2,2))
hist(LP05,col="grey",xlim=c(0,25),breaks=seq(0,25,by=1),
     ylim=c(0,1000),xlab="Count",main="Lambda = 0.5")
hist(LP1,col="grey",xlim=c(0,25),breaks=seq(0,25,by=1),
     ylim=c(0,1000),xlab="Count",main="Lambda = 1.0")
hist(LP5,col="grey",xlim=c(0,25),breaks=seq(0,25,by=1),
     ylim=c(0,1000),xlab="Count",main="Lambda = 5")
hist(LP10,col="grey",xlim=c(0,25),breaks=seq(0,25,by=1),
     ylim=c(0,1000),xlab="Count",main="Lambda = 10")
dev.off()

