#####################################
# PLSC 502 -- Fall 2016
#
# Day Thirteen materials
#####################################
# Not much here today... 
#
# Efficiency plot example:

x <- seq(-4,4,by=0.01)
fat<-dnorm(x,0,2)
thin<-dnorm(x,0,1)

pdf("EfficiencyPicR.pdf",6,5)
par(mar=c(2,2,2,2))
plot(x,thin,t="l",lwd=2,lty=1,xaxt="n",yaxt="n",xlab="",
     ylab="")
lines(x,fat,lwd=2,lty=2)
arrows(x[450],thin[450],x[450]+2,thin[450],lwd=2,length=0.1,code=1)
text(x[450]+2.3,thin[450],labels=expression(paste(hat(theta)[1])))
arrows(x[600],fat[600],x[600]+1,fat[600]+0.1,lwd=2,length=0.1,code=1)
text(x[600]+1.3,fat[600]+0.1,labels=expression(paste(hat(theta)[2])))
axis(1,at=0,labels=expression(paste(theta)))
abline(v=0,lty=3,lwd=1)
dev.off()

# MSE / "6" plot:

x<-seq(4,8,by=0.01)
MSEsix <- 36-12*x+x^2
MSEMean20 <- 10/20
MSEMean100 <- 10/100
MSEMean1000 <- 10/1000

pdf("VariousMSEsR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(x,MSEsix,t="l",lwd=2,xlab=expression(mu),
     ylab="MSE")
abline(h=MSEMean20,col="red",lty=2,lwd=2)
abline(h=MSEMean100,col="darkgreen",lty=3,lwd=2)
abline(h=MSEMean1000,col="blue",lty=4,lwd=2)
legend(6,4,bty="n",lty=c(1,2,3,4),lwd=2,
       col=c("black","red","darkgreen","blue"),
       legend=c(expression(paste(lambda,"=6")),"Mean (N=20)",
                "Mean (N=100)","Mean (N=1000)"))
dev.off()

# Consistency:

x <- seq(-4,4,by=0.01)
fat<-dnorm(x,-1.5,2)
med<-dnorm(x,-0.75,1.3)
thin<-dnorm(x,0,1)

pdf("ConsistencyPicR.pdf",6,5)
par(mar=c(2,2,2,2))
plot(x,thin,t="l",lwd=2,lty=1,xaxt="n",yaxt="n",xlab="",
     ylab="")
lines(x,med,lwd=2,lty=2)
lines(x,fat,lwd=2,lty=3)
arrows(x[450],thin[450],x[450]+2,thin[450],lwd=2,length=0.1,code=1)
text(x[450]+2.8,thin[450],labels="N=1000")
arrows(x[250],med[250],x[250]-0.5,med[250]+0.1,lwd=2,length=0.1,code=1)
text(x[250]-0.5,med[250]+0.12,labels="N=100")
arrows(x[150],fat[150],x[150]-0.5,fat[150]+0.1,lwd=2,length=0.1,code=1)
text(x[150]-0.5,fat[150]+0.12,labels="N=10")
axis(1,at=0,labels=expression(paste(theta)))
abline(v=0,lty=3,lwd=1)
dev.off()
