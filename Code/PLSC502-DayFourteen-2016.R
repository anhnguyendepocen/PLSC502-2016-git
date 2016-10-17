#####################################
# PLSC 502 -- Fall 2016
#
# Day Fourteen materials
#####################################
# Packages:

require(sampling)

# Picture of "pivotal" method:

z <- seq(-3,3,by=0.01)
zn <- dnorm(z)

pdf("CIIllustratedR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(z,zn,t="l",lwd=2,lty=2,xaxt="n",yaxt="n",ylab="",
     xlab=expression(paste("Possible Values of ",bar(X))))
abline(h=0)
segments(0,0,0,zn[301],lwd=3,col="black")
segments(z[90],0,z[90],zn[90],lwd=2,col="red")
segments(z[510],0,z[510],zn[510],lwd=2,col="red")
axis(1,at=c(z[90],0,z[510]),
     labels=c(expression(hat(theta)[L]),
              expression(hat(theta)),
              expression(hat(theta)[H])))
arrows(0,zn[90],z[90],zn[90],code=3,lwd=2,length=0.15,col="red")
arrows(0,zn[511],z[511],zn[511],code=3,lwd=2,length=0.15,col="red")
text(z[200],zn[115],cex=0.75,
     labels=expression(hat(theta) - z[alpha/2]*frac(sigma,sqrt(N))))
text(z[400],zn[115],cex=0.75,
     labels=expression(hat(theta) + z[alpha/2]*frac(sigma,sqrt(N))))
dev.off()

# CIs around proportions:

Pi<-seq(0.001,0.999,by=0.001)
PiHat<-seq(0.001,0.999,by=0.001)
ub10 <- Pi + 1.96*(sqrt((Pi*(1-Pi))/(10)))
lb10 <- Pi - 1.96*(sqrt((Pi*(1-Pi))/(10)))
ub100 <- Pi + 1.96*(sqrt((Pi*(1-Pi))/(100)))
lb100 <- Pi - 1.96*(sqrt((Pi*(1-Pi))/(100)))
ub400 <- Pi + 1.96*(sqrt((Pi*(1-Pi))/(400)))
lb400 <- Pi - 1.96*(sqrt((Pi*(1-Pi))/(400)))

pdf("ProportionCIsR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Pi,PiHat,t="l",lwd=3,xlab=expression(pi),
     ylab=expression(hat(pi)),xlim=c(-0.1,1.1),
     ylim=c(-0.1,1.1))
lines(Pi,ub10,lwd=2,lty=2,col="black")
lines(Pi,lb10,lwd=2,lty=2,col="black")
lines(Pi,ub100,lwd=2,lty=3,col="red")
lines(Pi,lb100,lwd=2,lty=3,col="red")
lines(Pi,ub400,lwd=2,lty=4,col="darkgreen")
lines(Pi,lb400,lwd=2,lty=4,col="darkgreen")
legend("topleft",bty="n",lty=c(2,3,4),lwd=2,
       col=c("black","red","darkgreen"),
       legend=c("N=10","N=100","N=400"))
dev.off()

####################################
# Court example...

WBURL<-"https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2016-git/master/Data/WarrenBurger.csv"
temp<-getURL(WBURL)
WB<-read.csv(textConnection(temp))
rm(temp,WBURL)

summary(WB)

# Single sample of 20 observations, calculate 
# pi-hat and its C.I. for CONSTIT:

set.seed(7222009)
WBsample <- with(WB, sample(constit,20,replace=F))
summary(WBsample)
LB <- mean(WBsample) - 1.96*sqrt((mean(WBsample)*(1-mean(WBsample)))/(20))
UB <- mean(WBsample) + 1.96*sqrt((mean(WBsample)*(1-mean(WBsample)))/(20))
print(c(LB,UB))

# prop.test(sum(WBsample),length(WBsample),correct=FALSE)

# Now do that 1000 times:

N <- 20
reps <- 1000
PI20 <- numeric(reps)
UB20<-numeric(reps)
LB20<-numeric(reps)
set.seed(7222009)
for (i in 1:reps) {
  foo <- with(WB, sample(constit,N,replace=F))
  bar <- prop.test(sum(foo),length(foo),correct=FALSE)
  PI20[i] <- bar$estimate
  LB20[i] <- PI20[i] - 1.96 * sqrt((PI20[i] * (1-PI20[i]))/(N))
  UB20[i] <- PI20[i] + 1.96 * sqrt((PI20[i] * (1-PI20[i]))/(N))
}

# Data for plotting: 

hats<-data.frame(P=as.numeric(unlist(dimnames(table(PI20)))),
                L=as.numeric(unlist(dimnames(table(LB20)))),
                U=as.numeric(unlist(dimnames(table(UB20)))))

# Plot...

pdf("PropSimN20-95-R.pdf",6,5)
par(mar=c(4,4,2,2))
with(hats, hist(PI20,xaxt="n",yaxt="n",main="",border="grey",
               xlab="",ylab=""))
par(new=TRUE)
with(hats, plot(PI20,PI20,t="p",pch=19,ylim=c(-0.2,0.8),
     xlab=expression(paste("Value of ",pi)),
     ylab=expression(hat(pi))))
with(hats, segments(P,L,P,U,lwd=4))
abline(h=mean(WB$constit),lwd=2,lty=2,col="red")
dev.off()

# How many of our confidence intervals contain the
# population mean of CONSTIT?

popmean<-mean(WB$constit)
prop.table(table(ifelse(UB20>popmean & LB20<popmean,1,0)))

# Same thing, increase to N=100:

N <- 100
reps <- 1000
PI100 <- numeric(reps)
UB100<-numeric(reps)
LB100<-numeric(reps)
set.seed(7222009)
for (i in 1:reps) {
  foo <- with(WB, sample(constit,N,replace=F))
  bar <- prop.test(sum(foo),length(foo),correct=FALSE)
  PI100[i] <- bar$estimate
  LB100[i] <- PI100[i] - 1.96 * sqrt((PI100[i] * (1-PI100[i]))/(N))
  UB100[i] <- PI100[i] + 1.96 * sqrt((PI100[i] * (1-PI100[i]))/(N))
}

# Data for plotting: 

hats<-data.frame(P=as.numeric(unlist(dimnames(table(PI100)))),
                 L=as.numeric(unlist(dimnames(table(LB100)))),
                 U=as.numeric(unlist(dimnames(table(UB100)))))

# Plot...

pdf("PropSimN100-95-R.pdf",6,5)
par(mar=c(4,4,2,2))
with(hats, hist(PI100,xaxt="n",yaxt="n",main="",border="grey",
                xlab="",ylab="",breaks=20))
par(new=TRUE)
with(hats, plot(PI100,PI100,t="p",pch=19,ylim=c(0,0.6),
                xlab=expression(paste("Value of ",pi)),
                ylab=expression(hat(pi))))
with(hats, segments(P,L,P,U,lwd=4))
abline(h=mean(WB$constit),lwd=2,lty=2,col="red")
dev.off()

# CI coverage:

popmean<-mean(WB$constit)
prop.table(table(ifelse(UB100>popmean & LB100<popmean,1,0)))

# Now N = 400...

N <- 400
reps <- 1000
PI400 <- numeric(reps)
UB400<-numeric(reps)
LB400<-numeric(reps)
set.seed(7222009)
for (i in 1:reps) {
  foo <- with(WB, sample(constit,N,replace=F))
  bar <- prop.test(sum(foo),length(foo),correct=FALSE)
  PI400[i] <- bar$estimate
  LB400[i] <- PI400[i] - 1.96 * sqrt((PI400[i] * (1-PI400[i]))/(N))
  UB400[i] <- PI400[i] + 1.96 * sqrt((PI400[i] * (1-PI400[i]))/(N))
}

# Data for plotting: 

hats<-data.frame(P=as.numeric(unlist(dimnames(table(PI400)))),
                 L=as.numeric(unlist(dimnames(table(LB400)))),
                 U=as.numeric(unlist(dimnames(table(UB400)))))

# Plot...

pdf("PropSimN400-95-R.pdf",6,5)
par(mar=c(4,4,2,2))
with(hats, hist(PI400,xaxt="n",yaxt="n",main="",border="grey",
                xlab="",ylab="",breaks=30))
par(new=TRUE)
with(hats, plot(PI400,PI400,t="p",pch=19,ylim=c(0.1,0.4),
                xlab=expression(paste("Value of ",pi)),
                ylab=expression(hat(pi))))
with(hats, segments(P,L,P,U,lwd=4))
abline(h=mean(WB$constit),lwd=2,lty=2,col="red")
dev.off()

# CI coverage:

popmean<-mean(WB$constit)
prop.table(table(ifelse(UB400>popmean & LB400<popmean,1,0)))



