#################################
# PLSC 502 Day Twenty Five code 
# (linear regression model fit)
#################################

seed <- 7222009
set.seed(seed)

X<-rnorm(250)
Y1<-5+2*X+rnorm(250,mean=0,sd=sqrt(0.2))
Y2<-5+2*X+rnorm(250,mean=0,sd=sqrt(20))
fit<-lm(Y1~X)
summary(fit)

pdf("TightLine-R.pdf",5,5)
plot(X,Y1,pch=20,xlab="X",ylab="Y",
     xlim=c(-2.5,2.5),ylim=c(-10,15))
abline(fit,lwd=3)
text(-1.5,12,labels="R-squared = 0.95")
dev.off()

fit2<-lm(Y2~X)
summary(fit2)

pdf("ScatteredLine-R.pdf",5,5)
plot(X,Y2,pch=20,xlab="X",ylab="Y",
     xlim=c(-2.5,2.5),ylim=c(-10,15))
abline(fit2,lwd=3)
text(-1.5,12,labels="R-squared = 0.20")
dev.off()

# Three-observation regression:

X<-c(1,2,3)
Y<-c(1,1,7)
WSR<-lm(Y~X)
summary(WSR)

# Plot:

# N = 3 plot...

df <- data.frame(X = c(1,2,3),
                 Y = c(1,1,7))
Xmean <- mean(df$X)
Ymean <- mean(df$Y)
fit3 <- with(df, lm(Y~X))
df$hats <- fit3$fitted.values
df$resid <- fit3$residuals

pdf("Nequals3-R.pdf",6,5)
par(mar=c(4,4,2,2))
with(df, plot(X,Y,pch=19,ylim=c(0,8),xlim=c(0.5,3.5)))
abline(h=Ymean)
with(df, segments(X,Y,X,hats,lwd=2,lty=2,col="red"))
segments(df$X,Ymean,df$X,df$hats,lwd=2,lty=3,col="green")
abline(fit3,lwd=3)
with(df, points(X,Y,pch=20))
legend("topleft",bty="n",lwd=c(3,2,2,1),lty=c(1,2,3,1),
       col=c("black","red","green","black"),
       legend=c("Regression line","RSS","MSS","Mean of Y"))
dev.off()

# R^2 = 0 plots:

seed<-7222009
set.seed(seed)
pdf("RSqZero.pdf",7,6)
par(mfrow=c(2,2))
par(mar=c(4,4,2,2))
X<-(runif(100))*10
Yzero<-(runif(100))*10
Yquad<-30-((X-5)^2)+(2*runif(100))
Ystep<-ifelse(abs(X-5)>2.5,5+runif(100),1+runif(100))
Ytype<-rep(0:1,50)
Yvar<-ifelse(Ytype==1,X+(2*runif(50)),10-X+2*runif(50))
plot(Yzero~X,xlab="X", ylab="Y",pch=20)
abline(lm(Yzero~X),lwd=3)
plot(Yquad~X,xlab="X", ylab="Y",pch=20)
abline(lm(Yquad~X),lwd=3)
plot(Ystep~X,xlab="X", ylab="Y",pch=20)
abline(lm(Ystep~X),lwd=3)
plot(Yvar[Ytype==0]~X[Ytype==0],xlab="X", ylab="Y",pch=20)
points(Yvar[Ytype==1]~X[Ytype==1],pch="o")
abline(lm(Yvar~X),lwd=3)
dev.off()


