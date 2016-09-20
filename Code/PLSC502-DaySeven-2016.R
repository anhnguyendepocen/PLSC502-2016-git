#####################################
# PLSC 502 -- Fall 2016
#
# Day Seven materials
#####################################
#
# There isn't much here...
#
# Odds plots:

Pr<-seq(0.01,0.99,by=0.01)
Odds<-Pr / (1-Pr)

spots<-c(20,50,80,90)
lc<-c("green","black","red","blue")
pdf("ProbabilityAndOddsR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Odds,Pr,t="l",lwd=2.5,xlim=c(0,20),ylim=c(0,1),
     ylab="Probability")
for (i in 1:length(spots)) {
  segments(Odds[spots[i]],0,Odds[spots[i]],Pr[spots[i]],lty=2,lwd=2,
           col=lc[i])
  segments(Odds[spots[i]],Pr[spots[i]],-1,Pr[spots[i]],lty=2,lwd=2,
           col=lc[i])
}
legend("bottomright",bty="n",lwd=2,lty=2,col=lc,cex=0.75,
       legend=c("Pr = 0.2 (Odds = 1:4)",
                "Pr = 0.5 (Odds = 1:1)",
                "Pr = 0.8 (Odds = 4:1)",
                "Pr = 0.9 (Odds = 9:1)"))
lines(Odds,Pr,lwd=2,col="black")
dev.off()

# Log-Odds

LogOdds<-log(Odds)

pdf("ProbAndLogOddsR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(LogOdds,Pr,t="l",lwd=2.5,ylim=c(0,1),xlab="Log-Odds",
     ylab="Probability")
for (i in 1:length(spots)) {
  segments(LogOdds[spots[i]],0,LogOdds[spots[i]],Pr[spots[i]],
           lty=2,lwd=2,col=lc[i])
  segments(LogOdds[spots[i]],Pr[spots[i]],-5,Pr[spots[i]],
           lty=2,lwd=2,col=lc[i])
}
dev.off()
