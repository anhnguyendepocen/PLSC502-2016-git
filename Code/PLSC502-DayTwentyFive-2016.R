################################################
# PLSC 502 -- Fall 2016 
# Code for Day Twenty Five
################################################

# Data:

library(RCurl)
temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2016-git/master/Data/fordham.csv")
Data<-read.csv(text=temp, header=TRUE)
rm(temp)

with(Data, summary(milgdp))
with(Data, summary(gdp))

# Ladder Plots in R:

par(mfrow=c(3,3))
with(Data, plot(density(gdp^3,na.rm=TRUE),main="Cubic"))
with(Data, plot(density(gdp^2,na.rm=TRUE),main="Square"))
with(Data, plot(density(gdp,na.rm=TRUE),main="Identity"))
with(Data, plot(density(sqrt(gdp),na.rm=TRUE),main="Square Root"))
with(Data, plot(density(log(gdp),na.rm=TRUE),main="Log"))
with(Data, plot(density(1/sqrt(gdp),na.rm=TRUE),main="1 / Square Root"))
with(Data, plot(density(1/gdp,na.rm=TRUE),main="Inverse"))
with(Data, plot(density(1/gdp^2,na.rm=TRUE),main="1 / Square"))
with(Data, plot(density(1/gdp^3,na.rm=TRUE),main="1 / Cubic"))

with(Data, plot(density(milgdp^3,na.rm=TRUE),main="Cubic"))
with(Data, plot(density(milgdp^2,na.rm=TRUE),main="Square"))
with(Data, plot(density(milgdp,na.rm=TRUE),main="Identity"))
with(Data, plot(density(sqrt(milgdp),na.rm=TRUE),main="Square Root"))
with(Data, plot(density(log(milgdp),na.rm=TRUE),main="Log"))
with(Data, plot(density(1/sqrt(milgdp),na.rm=TRUE),main="1 / Square Root"))
with(Data, plot(density(1/milgdp,na.rm=TRUE),main="Inverse"))
with(Data, plot(density(1/milgdp^2,na.rm=TRUE),main="1 / Square"))
with(Data, plot(density(1/milgdp^3,na.rm=TRUE),main="1 / Cubic"))

# Other plots:

par(mfrow=c(2,2))
with(Data, plot(gdp,milgdp))
with(Data, plot(log(gdp),milgdp))
with(Data, plot(gdp,log(milgdp)))
with(Data, plot(log(gdp),log(milgdp)))

# Regressions:

linlin <- with(Data, lm(milgdp~gdp))
summary(linlin)
linlog <- with(Data, lm(milgdp~log(gdp)))
summary(linlog)
loglin <- with(Data, lm(log(milgdp+0.01)~gdp))
summary(loglin)
loglog <- with(Data, lm(log(milgdp+0.01)~log(gdp)))
summary(loglog)

# Residual plot:

pdf("MilSpendGDPResidsDensities-R.pdf",6,5)
par(mfrow=c(1,1))
par(mar=c(4,4,2,2))
plot(density(linlog$residuals),xlim=c(-5,10),lwd=2,lty=3,
     col="blue",main="",xlab="Residual Values")
lines(density(linlin$residuals),lwd=1,lty=1,col="black")
lines(density(loglin$residuals),lwd=2,lty=1,col="red")
lines(density(loglog$residuals),lwd=4,lty=1,col="darkgreen")
abline(v=0,lty=2,lwd=1)
legend("topright",bty="n",lwd=2,lty=c(1,2,3,4),
       col=c("black","red","blue","darkgreen"),
       legend=c("Untransformed","Logged Y","Logged X",
                "Both Logged"))
dev.off()

