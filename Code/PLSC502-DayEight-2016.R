#####################################
# PLSC 502 -- Fall 2016
#
# Day Eight materials
#####################################
# There isn't much here...
#
# Plot for the little hypothetical:

x <- seq(-1,1,by=0.01)
fx <- 0.5*(x+1)
xcoord<-c(-1,1)

pdf("ASimplePDF-R.pdf",6,5)
plot(x,fx,t="l",lwd=3,xlab="X",ylab="f(X)",xlim=c(-1.2,1.2))
polygon(c(-1,1,1,-1),c(0,1,0,0),col="grey",border=NA)
segments(-1.4,0,-1,0,lwd=3)
segments(1,0,1,4,0,lwd=3)
segments(1,0,1,1,lwd=3,lty=2)
segments(1,0,1.4,0,lwd=3)
dev.off()

# Another one:

pdf("ASimplePDF-2-R.pdf",6,5)
plot(x,fx,t="l",lwd=3,xlab="X",ylab="f(X)",xlim=c(-1.2,1.2))
polygon(c(-1,1,1,-1),c(0,1,0,0),col="grey",border=NA)
segments(-1.4,0,-1,0,lwd=3)
segments(1,0,1,4,0,lwd=3)
segments(1,0,1,1,lwd=3,lty=2)
segments(1,0,1.4,0,lwd=3)
segments(1/3,0,1/3,1,lwd=3,lty=3,col="darkgreen")
legend("topleft",bty="n",lwd=3,lty=3,col="darkgreen",
       legend=c("E(X)")) 
dev.off()

# CDF (calculated):

FX <- 0.5* (0.5 * (x)^2 + (x)) + 0.25

# CDF (numerically):

integrand <- function(x) {(x+1)/2}
altFX <- numeric(201)
for(i in 1:201) {
  altFX[i] <- integrate(integrand,-1,x[i])$value
}

# Fix for plotting:

FX<-c(0,FX)
FX<-c(FX,1)
x<-c(-1.2,x)
x<-c(x,1.2)

# Plot the CDF:

pdf("ExampleCDF-R.pdf",6,5)
par(mar=c(4,4,2,2))
plot(x,FX,t="l",lwd=3,xlab="F(x) [integral of f(x)]",
     ylab="Cumulative Probability")
dev.off()
