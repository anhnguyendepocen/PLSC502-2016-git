###########################################
# PLSC 502 -- Fall 2016
#
# Code for Day Twenty Seven
###########################################

# Collider plot:

seed <- 7222009
set.seed(seed)
N <- 200
SAT <- rnorm(N,1100,200)
Motivation <- runif(N,0,5)
Admitted <- rbinom(N,1,plogis(-13+1*Motivation+0.01*SAT))
table(Admitted)

pdf("ConditionalDependenceColliderPlotR.pdf",5,5)
par(mar=c(4,4,2,2))
plot(SAT[Admitted==0],Motivation[Admitted==0],pch=20,
     xlim=c(600,1600),ylim=c(0,5),xlab="SAT",ylab="Motivation")
points(SAT[Admitted==1],Motivation[Admitted==1],pch=4)
abline(lm(Motivation[Admitted==0]~SAT[Admitted==0]),lwd=2)
abline(lm(Motivation[Admitted==1]~SAT[Admitted==1]),lwd=2,lty=2)
legend("bottomleft",pch=c(20,4),bg="white",
       legend=c("Not Admitted","Admitted"))
dev.off()

# Simpson's paradox:

Y <- c(12,13,14,9,10,11,6,7,8,3,4,5)
X <- c(-5,-4,-3,-3,-2,-1,-1,0,1,1,2,3)
G <- c(rep("A",3),rep("B",3),rep("C",3),rep("D",3))

pdf("SimpsonsParadoxR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(X,Y,pch=19,xlim=c(-6,4),ylim=c(0,16))
text(X,Y,G,pos=1)
dev.off()
