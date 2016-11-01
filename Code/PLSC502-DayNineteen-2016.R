#####################################
# PLSC 502 -- Fall 2016
#
# Day Nineteen materials
#####################################
# Packages, etc.:

require(RCurl)

options(digits=5)

# Fake data for the running example:

X <- c(0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1)
Y <- c(0,0,0,0,0,1,1,1,1,0,0,0,1,1,1,1,1,1,1,1)

T <- table(Y,X)
T

chisq.test(T,correct=FALSE)

p1<-4/9
p2<-8/11
p <- 12/20
se <- sqrt(((p*(1-p)*(1/9+1/11))))
Z <- (p1-p2) / se
Z
Z^2

# Chi-square is not a measure of association...

chisq.test(T, correct=FALSE)
X <- rep(X,times=10)
Y <- rep(Y,times=10)
T10 <- table(X,Y)
T10
chisq.test(T10,correct=FALSE)

# Odds Ratios...

T
OR <- (T[1,1])*T[2,2] / (T[1,2]*T[2,1])
OR

require(DescTools)
OddsRatio(T)

# Phi:
T
require(psych)
phi(T)
cor(X,Y)

Tpos<-as.table(rbind(c(10,0),c(0,10)))
phi(Tpos)
Tneg<-as.table(rbind(c(0,10),c(10,0)))
phi(Tneg)
T0<-as.table(rbind(c(5,5),c(5,5)))
phi(T0)

# Tetrachoric degression: Bivariate normals:

require(mvtnorm)
require(car)

set.seed(7222009)
N <- 10000
mu <- c(0,0)
var0 <- matrix(c(1,0,0,1),2,2)
var5<- matrix(c(1,0.5,0.5,1),2,2)
varn5 <- matrix(c(1,-0.5,-0.5,1),2,2)
var9<- matrix(c(1,0.9,0.9,1),2,2)
Z0 <- mvrnorm(N,mu,var0)
Z5 <- mvrnorm(N,mu,var5)
Zn5 <- mvrnorm(N,mu,varn5)
Z9 <- mvrnorm(N,mu,var9)

# Plot:
pdf("BivNormalsR.pdf",7,6)
par(mfrow=c(2,2))
par(mar=c(4,4,2,2))
ellipses(Z0[,1],Z0[,2],pch=".",xlab="X1",ylab="X2",
         smooth=FALSE)
legend("topright",bty="n",legend="r=0")
ellipses(Z5[,1],Z5[,2],pch=".",xlab="X1",ylab="X2",
         smooth=FALSE)
legend("topleft",bty="n",legend="r=0.5")
ellipses(Zn5[,1],Zn5[,2],pch=".",xlab="X1",ylab="X2",
         smooth=FALSE)
legend("topright",bty="n",legend="r=-0.5")
ellipses(Z9[,1],Z9[,2],pch=".",xlab="X1",ylab="X2",
         smooth=FALSE)
legend("topleft",bty="n",legend="r=0.9")
dev.off()

# Error correlations:

set.seed(7222009)
N <- 1000
mu <- c(0,0)
var4 <- matrix(c(1,0.4,0.4,1),2,2)
E4 <- mvrnorm(N,mu,var4)
var75 <- matrix(c(1,0.75,0.75,1),2,2)
E75 <- mvrnorm(N,mu,var75)

# Plot:

pdf("LatentTetrachoricR.pdf",6,5)
par(mfrow=c(1,1))
par(mar=c(4,4,2,2))
ellipses(E4[,1],E4[,2],pch=20,xaxt="n",yaxt="n",
         xlab=expression(e[1]),xlim=c(-4,4),
         ylab=expression(e[2]),ylim=c(-4,4),
         smooth=FALSE)
abline(v=0,lwd=2,lty=2,col="red")
abline(h=0,lwd=2,lty=2,col="red")
axis(1,at=0,labels=expression(tau[1]))
axis(2,at=0,labels=expression(tau[2]))
text(-3,-3,labels="{0,0}",col="red")
text(-3,3,labels="{0,1}",col="red")
text(3,-3,labels="{1,0}",col="red")
text(3,3,labels="{1,1}",col="red")
dev.off()

# Second plot:

pdf("LatentTetrachoricR2.pdf",6,5)
par(mar=c(4,4,2,2))
ellipses(E75[,1],E75[,2],pch=20,xaxt="n",yaxt="n",
         xlab=expression(e[1]),xlim=c(-4,4),
         ylab=expression(e[2]),ylim=c(-4,4),
         smooth=FALSE)
abline(v=-0.5,lwd=2,lty=2,col="red")
abline(h=1.5,lwd=2,lty=2,col="red")
axis(1,at=-0.5,labels=expression(tau[1]))
axis(2,at=1.5,labels=expression(tau[2]))
text(-3,-3,labels="{0,0}",col="red")
text(-3,3,labels="{0,1}",col="red")
text(3,-3,labels="{1,0}",col="red")
text(3,3,labels="{1,1}",col="red")
dev.off()

# Actual r_tet:

require(polycor)
polychor(T)

# Compare:

phi(T)

# Approximate formula:

alpha <- (OR)^(pi/4)
rtet <- (alpha - 1) / (alpha + 1)
rtet

# Phi and Tetrachoric Correlations:
#
# Symmetrical marginals:

NTs <- 101
PHI <- numeric(NTs)
RTET <- numeric(NTs)

for (i in 1:NTs) {
  ST <- as.table(rbind(c(NTs-i,i-1),c(i-1,NTs-i)))
  PHI[i] <- phi(ST)
  RTET[i] <- polychor(ST)
}

pdf("PhiVsRtetSymR.pdf",7,5)
par(mar=c(4,4,2,2))
plot(PHI,RTET,t="l",lwd=2,col="red",xlab=expression(phi),
     ylab=expression(r[tet]))
abline(a=0,b=1,lwd=1,lty=2)
dev.off()

# Asymmertrical marginals:

NTs <- 100
PHI <- numeric(NTs)
RTET <- numeric(NTs)

for (i in 0:NTs) {
  AT <- as.table(rbind(c(NTs-i,(NTs/2)+i),c(i,(NTs/2)+i)))
  PHI[i] <- phi(AT)
  RTET[i] <- polychor(AT)
}

pdf("PhiVsRtetAsymR.pdf",7,5)
par(mar=c(4,4,2,2))
plot(PHI,RTET,t="l",lwd=2,col="red",xlab=expression(phi),
     ylab=expression(r[tet]),ylim=c(-1,1),xlim=c(-1,1))
abline(a=0,b=1,lwd=1,lty=2)
dev.off()

# fin
