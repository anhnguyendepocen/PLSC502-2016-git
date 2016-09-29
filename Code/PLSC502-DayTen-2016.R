#####################################
# PLSC 502 -- Fall 2016
#
# Day Ten materials
#####################################

# Uniform distributions:

x <- seq(-3,3,by=0.01)
U01 <- dunif(x)
U115 <- dunif(x,-1,1.5)
U252 <- dunif(x,-2.5,2)

pdf("UniformsR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(x,U01,t="l",lwd=2,col="black",xlim=c(-3,3),
     xlab="x value",ylab="Density")
lines(x,U115,lwd=2,lty=2, col="red")
lines(x,U252,lwd=2,lty=3, col="darkgreen")
legend("topleft",bty="n",lwd=2,lty=c(1,2,3),
       col=c("black","red","darkgreen"),
       legend=c("U(0,1)","U(-1,1.5)","U(-2.5,2)"))
dev.off()

# Uniform CDFs:

PU01 <- punif(x)
PU115 <- punif(x,-1,1.5)
PU252 <- punif(x,-2.5,2)
pdf("UniformFsR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(x,PU01,t="l",lwd=2,col="black",xlim=c(-3,3),
     xlab="x value",ylab="Cumulative Probability")
lines(x,PU115,lwd=2,lty=2, col="red")
lines(x,PU252,lwd=2,lty=3, col="darkgreen")
legend("topleft",bty="n",lwd=2,lty=c(1,2,3),
       col=c("black","red","darkgreen"),
       legend=c("U(0,1)","U(-1,1.5)","U(-2.5,2)"))
dev.off()

# Normals:

y <- seq(-10,10,by=0.1)
N01 <- dnorm(y)
N21 <- dnorm(y,2,1)
NN32 <- dnorm(y,-3,2)
N54 <- dnorm(y,5,4)
pdf("NormalsR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(y,N01,t="l",lwd=2,col="black",xlim=c(-10,10),
     xlab="x value",ylab="Density")
lines(y,N21,lwd=2,lty=2, col="red")
lines(y,NN32,lwd=2,lty=3, col="darkgreen")
lines(y,N54,lwd=2,lty=4, col="orange")
legend("topleft",bty="n",lwd=2,lty=c(1,2,3,4),
       col=c("black","red","darkgreen","orange"),
       legend=c("N(0,1)","N(2,1)","N(-3,2)","N(5,4)"))
dev.off()

# CDFs:
PN01 <- pnorm(y)
PN21 <- pnorm(y,2,1)
PNN32 <- pnorm(y,-3,2)
PN54 <- pnorm(y,5,4)
pdf("NormalFsR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(y,PN01,t="l",lwd=2,col="black",xlim=c(-10,10),
     xlab="x value",ylab="Cumulative Probability")
lines(y,PN21,lwd=2,lty=2, col="red")
lines(y,PNN32,lwd=2,lty=3, col="darkgreen")
lines(y,PN54,lwd=2,lty=4, col="orange")
legend("topleft",bty="n",lwd=2,lty=c(1,2,3,4),
       col=c("black","red","darkgreen","orange"),
       legend=c("N(0,1)","N(2,1)","N(-3,2)","N(5,4)"))
dev.off()

# Lognormals:

z<-seq(0,20,by=0.1)
LN01 <- dlnorm(z)
LN11 <- dlnorm(z,1,1)
LN105 <- dlnorm(z,1,0.5)
LN125 <- dlnorm(z,1,0.25)
LN225 <- dlnorm(z,2,0.25)
pdf("LogNormalsR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(z,LN01,t="l",lty=1,lwd=2,col="black",xlim=c(0,20),
     xlab="x value",ylab="Density")
lines(z,LN11,lwd=2,lty=2, col="red")
lines(z,LN105,lwd=2,lty=3, col="darkgreen")
lines(z,LN125,lwd=2,lty=4, col="orange")
lines(z,LN225,lwd=2,lty=5, col="navy")
legend("topright",bty="n",lwd=2,lty=c(1,2,3,4,5),
       col=c("black","red","darkgreen","orange","navy"),
       legend=c("lnN(0,1)","lnN(1,1)","lnN(1,0.5)",
                "lnN(1,0.25)","lnN(2,0.25)"))
dev.off()

# Chi-square:

w<-seq(0,4,by=0.01)
CS1 <- dchisq(w,1)
CS2 <- dchisq(w,2)
CS3 <- dchisq(w,3)
CS5 <- dchisq(w,5)
CS10 <- dchisq(w,10)
pdf("ChiSquaresR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(w,CS1,t="l",lty=1,lwd=2,col="black",xlim=c(0,4),
     xlab="x value",ylab="Density")
lines(w,CS2,lwd=2,lty=2, col="red")
lines(w,CS3,lwd=2,lty=3, col="darkgreen")
lines(w,CS5,lwd=2,lty=4, col="orange")
lines(w,CS10,lwd=2,lty=5, col="navy")
legend("topright",bty="n",lwd=2,lty=c(1,2,3,4,5),
       col=c("black","red","darkgreen","orange","navy"),
       legend=c("Chi-square(1)","Chi-square(2)",
                "Chi-square(3)","Chi-square(5)",
                "Chi-square(10)"))
dev.off()

# Student's t:

q<-seq(-4,4,by=0.01)
T1 <- dt(q,1)
T3 <- dt(q,3)
T8 <- dt(q,8)
T30 <- dt(q,30)
TN <- dnorm(q)
pdf("T-R.pdf",6,5)
par(mar=c(4,4,2,2))
plot(q,TN,t="l",lty=2,lwd=2,col="black",xlim=c(-4,4),
     xlab="x value",ylab="Density")
lines(q,T1,lwd=2,lty=1, col="red")
lines(q,T3,lwd=2,lty=1, col="darkgreen")
lines(q,T8,lwd=2,lty=1, col="orange")
lines(q,T30,lwd=2,lty=1, col="navy")
legend("topright",bty="n",lwd=2,lty=c(2,1,1,1,1),
       col=c("black","red","darkgreen","orange","navy"),
       legend=c("Standard Normal","t(1)","t(3)","t(8)","t(30)"))
dev.off()

# F distributions:

F11 <- df(w,1,1)
F21 <- df(w,2,1)
F51 <- df(w,5,1)
F1010 <- df(w,10,10)
F100100 <- df(w,100,100)
pdf("FsR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(w,F11,t="l",lty=1,lwd=2,col="black",xlim=c(0,4),
     ylim=c(0,2),xlab="x value",ylab="Density")
lines(w,F21,lwd=2,lty=2, col="red")
lines(w,F51,lwd=2,lty=3, col="darkgreen")
lines(w,F1010,lwd=2,lty=4, col="orange")
lines(w,F100100,lwd=2,lty=5, col="navy")
legend("topright",bty="n",lwd=2,lty=c(1,2,3,4,5),
       col=c("black","red","darkgreen","orange","navy"),
       legend=c("F(1,1)","F(2,1)","F(5,1)","F(10,10)",
                "F(100,100)"))
dev.off()

###########################################
# Taking random draws from distributions...

Xnorm<-rnorm(1000, mean=5,sd=sqrt(2))
Xbinom5point2<-rbinom(10000,5,0.2)

# Plot of binomials:

pdf("BinomialDrawsPlotR.pdf",6,5)
par(mar=c(4,4,2,2))
barplot(table(Xbinom5point2),xlab="Outcome",ylab="Frequency")
dev.off()

# Seed example:

seed<-3229 # calling "seed" some thing
set.seed(seed) # setting the system seed
rt(3,1) # three draws from a t distrib. w/1 d.f.
seed<-1077
set.seed(seed) # resetting the seed
rt(3,1) # different values for the draws
seed<-3229 # original seed
set.seed(seed)
rt(3,1) # identical values of the draws

