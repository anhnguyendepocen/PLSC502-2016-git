#####################################
# PLSC 502 -- Fall 2016
#
# Day Twenty-One materials
#####################################
# Packages, etc.:

require(RCurl) 
require(DescTools)
require(car)
require(mvtnorm)

# Linear, Logarithmic and Exponential plots:

set.seed(7222009)
N <- 100
X <- runif(N,0,5)
Ylin <- X + runif(N)
Ylog <- log(X)+runif(N)
Yexp <- exp(X)+runif(N,0,20)

pdf("XYLinLogExp.pdf",7,5)
par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
plot(X,Ylin,pch=19,xlim=c(0,5),ylab="Y")
legend("bottomright",bty="n",legend="Y = X + u")
plot(X,Ylog,pch=19,xlim=c(0,5),ylab="Y")
legend("bottomright",bty="n",legend="Y = ln(X) + u")
plot(X,Yexp,pch=19,xlim=c(0,5),ylab="Y")
legend("topleft",bty="n",legend="Y = exp(X) + u")
dev.off()

# Other relationships:

Ystep <- ifelse(X>2.5,4+runif(N),2+runif(N))
Ypoly <- 5 + 5*X - X^2 + 2*runif(N)
Ythresh <- ifelse(X>2.5,mean(X)-2.5+(X)+runif(N),mean(X)+runif(N))

pdf("XYOthers.pdf",7,5)
par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
plot(X,Ystep,pch=19,xlim=c(0,5),ylab="Y")
legend("bottomright",bty="n",legend="Step Function")
plot(X,Ypoly,pch=19,xlim=c(0,5),ylab="Y")
legend("topright",bty="n",legend="Polynomial")
plot(X,Ythresh,pch=19,xlim=c(0,5),ylab="Y")
legend("topleft",bty="n",legend="Threshold /\nChange Point")
dev.off()

# Pearson's r plot:

set.seed(8092016)
Yr <- X + 2*rnorm(N)

pdf("PearsonsRPlotR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(X,Yr,pch=19,ylab="Y")
abline(h=mean(Yr),lty=2,lwd=2,col="red")
abline(v=mean(X),lty=2,lwd=2,col="red")
text(4,7,"I",col="red")
text(4,-1,"II",col="red")
text(1,-1,"III",col="red")
text(1,7,"IV",col="red")
dev.off()

# Four linear relationships:

set.seed(7222009)
XY9 <- rmvnorm(N, mean=c(0,0), sigma=matrix(c(1,0.9,0.9,1),ncol=2))
XY5 <- rmvnorm(N, mean=c(0,0), sigma=matrix(c(1,0.5,0.5,1),ncol=2))
XY0 <- rmvnorm(N, mean=c(0,0), sigma=matrix(c(1,0,0,1),ncol=2))
XYN5 <- rmvnorm(N, mean=c(0,0), sigma=matrix(c(1,-0.5,-0.5,1),ncol=2))

pdf("FourLinearsR.pdf",7,7)
par(mfrow=c(2,2))
par(mar=c(4,4,2,2))
plot(XY9,pch=19,xlab="X",ylab="Y")
legend("topleft",bty="n",legend="r = 0.9")
plot(XY5,pch=19,xlab="X",ylab="Y")
legend("topleft",bty="n",legend="r = 0.5")
plot(XY0,pch=19,xlab="X",ylab="Y")
legend("topright",bty="n",legend="r = 0")
plot(XYN5,pch=19,xlab="X",ylab="Y")
legend("topright",bty="n",legend="r = -0.5")
dev.off()

# Perfect Linearities:

YP1 <- X
YP2 <- 0.2*X - 3
YP3 <- 3*X - 5

pdf("PerfectRs.pdf",6,5)
par(mar=c(4,4,2,2))
plot(X,YP3,pch=19,ylab="Y")
points(X,YP1,pch=4,col="darkgreen")
points(X,YP2,pch=17,col="red")
legend("topleft",bty="n",legend="All have r = 1.0")
dev.off()

# Quadratic & other bad non-linearities

set.seed(8092016)

# Quadratic
YQuad <- 5 + 5*X - X^2 + 2*runif(N)
YQr <- cor(X,YQuad)
# "Steps"
Ysteps <- ifelse(X>1.5,runif(N),3+runif(N))
Ysteps <- ifelse(X>3.5,3+runif(N),Ysteps)
YSr <- cor(X,Ysteps)
# Outlier
Yout <- runif(N)
X <- X[order(X)]
Yout <- Yout[order(X)]
Yout[N-1] <- 10
YOr <- cor(X,Yout)

pdf("BadPearsonsR.pdf",7,5)
par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
plot(X,YQuad,pch=19,xlab="X",ylab="Y")
legend("topleft",bty="n",legend=paste0("r = ",round(YQr,2)))
plot(X,Ysteps,pch=19,xlab="X",ylab="Y")
legend("bottomright",bty="n",legend=paste0("r = ",round(YSr,2)))
plot(X,Yout,pch=19,xlab="X",ylab="Y")
legend("topleft",bty="n",legend=paste0("r = ",round(YOr,2)))
dev.off()

# Fisher's transformation plot:

r <- seq(-0.99,0.99,by=0.01)
z <- 0.5 * log((1+r)/(1-r))

pdf("RZPlotR.pdf",6,6)
par(mar=c(4,4,2,2))
plot(r,z,t="l",lwd=3)
dev.off()

# A little Pearson-Spearman comparison; first w/N=10:

Nreps <- 1000
N <- 10
Actual <- numeric(Nreps)
Rs <- numeric(Nreps)
Rhos <- numeric(Nreps)

set.seed(7222009)

for(i in 1:Nreps) {
  foo <- runif(1,-1,1)
  Actual[i] <- foo
  Xs<-rmvnorm(N,mean=c(0,0),sigma=matrix(c(1,foo,foo,1),ncol=2))
  Rs[i]<-cor(Xs[,1],Xs[,2])
  Rhos[i]<-SpearmanRho(Xs[,1],Xs[,2])
}

# Then with N=1000:

Nreps <- 1000
NK <- 1000
ActualK <- numeric(Nreps)
RsK <- numeric(Nreps)
RhosK <- numeric(Nreps)

set.seed(7222009)

for(i in 1:Nreps) {
  foo <- runif(1,-1,1)
  ActualK[i] <- foo
  Xs<-rmvnorm(NK,mean=c(0,0),sigma=matrix(c(1,foo,foo,1),ncol=2))
  RsK[i]<-cor(Xs[,1],Xs[,2])
  RhosK[i]<-SpearmanRho(Xs[,1],Xs[,2])
}

# Plots:

pdf("RvsRhoR.pdf",7,5)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
plot(Rs,Rhos,pch=20,xlab="Pearson's r",
     ylab=expression(rho))
abline(a=0,b=1,col="grey")
legend("topleft",bty="n",legend="N = 10")
plot(RsK,RhosK,pch=20,xlab="Pearson's r",
     ylab=expression(rho))
abline(a=0,b=1,col="grey")
legend("topleft",bty="n",legend="N = 1000")
dev.off()

# Africa examples:

AFURL<-"https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2016-git/master/Data/africa2001.csv"
temp<-getURL(AFURL)
Africa<-read.csv(textConnection(temp))
rm(temp,AFURL)

# POLITY and Health $:

pdf("POLITYHealthExp.pdf",5,5)
par(mfrow=c(1,1))
par(mar=c(4,4,2,2))
with(Africa, plot(polity,healthexp,pch=20,xlab="POLITY",
                  ylab="Health Expenditures"))
dev.off()

with(Africa, cor.test(polity,healthexp))
with(Africa, SpearmanRho(polity,healthexp))

# Literacy and HIV:

pdf("LiteracyHIV.pdf",5,5)
par(mfrow=c(1,1))
par(mar=c(4,4,2,2))
with(Africa, plot(literacy,adrate,pch=20,xlab="Literacy Rate",
                  ylab="HIV Prevalence Rate"))
dev.off()

with(Africa, cor.test(literacy,adrate))
with(Africa, SpearmanRho(literacy,adrate))

# Muslim % and HIV:

pdf("MuslimHIV.pdf",5,5)
par(mfrow=c(1,1))
par(mar=c(4,4,2,2))
with(Africa, plot(muslperc,adrate,pch=20,xlab="Muslim Percent",
                  ylab="HIV Prevalence Rate"))
dev.off()

with(Africa, cor.test(muslperc,adrate))
with(Africa, SpearmanRho(muslperc,adrate))
