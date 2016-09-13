#####################################
# PLSC 502 -- Fall 2016
#
# Day Five materials
#####################################

library(RCurl)

# Get NFL (2016) data:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2016-git/master/Data/NFL2016.csv")
NFL<-read.csv(text=temp, header=TRUE)
rm(temp)

# Histogram:

pdf("NFL-2016-Histogram.pdf",6,5)
par(mar=c(4,4,2,2))
with(NFL, hist(points,col="grey",main="",xlab="Points Scored"))
dev.off()

# Many modes:

x<-seq(-1,1,by=0.01)
nomode <- dunif(x,-0.9,0.9)
uni <- dnorm(x,0,0.2)
b1 <- dnorm(x,-0.5,0.15)
b2 <- dnorm(x,0.5,0.15)
bi <- b1+b2

pdf("ManyModes.pdf",7,4)
par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
plot(x,uni,t="l",lwd=3,xlab="X",yaxt="n",
     ylab="Frequency",main="Unimodal")
par(mar=c(4,2,2,2))
plot(x,bi,t="l",lwd=3,xlab="X",yaxt="n",
     ylab=" ",main="Bimodal")
plot(x,nomode,t="l",lwd=3,xlab="X",yaxt="n",
     ylab=" ",ylim=c(0,0.8),main="No Mode")
dev.off()

# Means/modes/medians and skewness:

p<-10
q<-1.5
m<-(p+q)/2
x<-seq(0,1,by=0.001)
left<-dbeta(x,p,q)
noskew<-dbeta(x,m,m)
right<-dbeta(x,q,p)

pdf("MeanMedianMode.pdf",6,5)
par(mfrow=c(3,1))
par(mar=c(4,4,2,2))
# right-skewed:
plot(x,right,t="l",lwd=2,main="Right-Skewed",
     xlab="X",ylab="Density")
abline(v=(q/(q+p)),lwd=2,col="red",lty=2) # mean
abline(v=((q-(1/3))/(p+q-(2/3))),lwd=2,col="darkgreen",lty=2) # median
abline(v=((q-1)/(q+p-2)),lwd=2,col="orange",lty=2) # mode
legend("topright",bty="n",legend=c("Mean","Median","Mode"),
       lwd=3,lty=c(2,2,2),col=c("red","darkgreen","orange"))
# symmetrical:
par(mar=c(4,4,2,2))
plot(x,noskew,t="l",lwd=2,main="Symmetrical",
     xlab="X",ylab="Density")
abline(v=(m/(m+m))+0.002,lwd=2,col="red",lty=2) # mean
abline(v=((m-(1/3))/(m+m-(2/3))),lwd=2,col="darkgreen",lty=2) # median
abline(v=((m-1)/(m+m-2))-0.002,lwd=2,col="orange",lty=2) # mode
legend("topright",bty="n",legend=c("Mean","Median","Mode"),
       lwd=3,lty=c(2,2,2),col=c("red","darkgreen","orange"))
# left-skewed:
plot(x,left,t="l",lwd=2,main="Right-Skewed",
     xlab="X",ylab="Density")
abline(v=(p/(p+q)),lwd=2,col="red",lty=2) # mean
abline(v=((p-(1/3))/(q+p-(2/3))),lwd=2,col="darkgreen",lty=2) # median
abline(v=((p-1)/(p+q-2)),lwd=2,col="orange",lty=2) # mode
legend("topleft",bty="n",legend=c("Mean","Median","Mode"),
       lwd=3,lty=c(2,2,2),col=c("red","darkgreen","orange"))
dev.off()

# Histogram with mean, median, and mode lines:

mean <- with(NFL, mean(points)) # mean
median <- with(NFL, median(points)) # median
mode <- function(x) {  # Function to find the mode of a
  ux <- unique(x)      # variable, of any kind
  ux[which.max(tabulate(match(x, ux)))]
}
mode <- with(NFL, mode(points)) # mode

lw<-3 # line width
pdf("NFL-2016-Histogram2.pdf",6,5)
par(mar=c(4,4,2,2))
with(NFL, hist(points,col="grey",main="",xlab="Points Scored"))
abline(v=mean, col="red",lty=2,lwd=lw)
abline(v=median,col="darkgreen",lty=2,lwd=lw)
abline(v=mode,col="orange",lty=2,lwd=lw)
legend("topright",bty="n",legend=c("Arithmetic Mean",
       "Median","Mode"),lwd=lw,lty=c(2,3,4,2,2),
       col=c("red","darkgreen","orange"))
dev.off()

