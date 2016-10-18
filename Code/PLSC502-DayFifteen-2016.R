#####################################
# PLSC 502 -- Fall 2016
#
# Day Fifteen materials
#####################################

options(digits=4)

# Plots of "tailedness":

OneTail <- qnorm(0.95)
sigma <- 1
mu <- 0
bounds <- c(mu-4*sigma, mu+4*sigma)
lower.x <- OneTail
upper.x <- max(bounds)
step <- (upper.x - lower.x) / 100
cord.x <- c(lower.x,seq(lower.x,upper.x,step),upper.x)
cord.y <- c(0,dnorm(seq(lower.x,upper.x,step),mu,sigma),0)

pdf("OneTailedDensityR.pdf",6,5)
par(mar=c(4,4,2,2))
curve(dnorm(x,mu,sigma),xlim=bounds,lwd=3,xlab="Z-score",
      ylab="Probability") 
polygon(cord.x,cord.y,col='red')
segments(0,0,0,z[500],lwd=2,lty=2)
arrows(3,0.2,2,0.06,length=0.12,lwd=2)
text(3,.22,label=c("Pr(Z > 1.65)"))
dev.off()

TwoTail <- qnorm(0.975)
LBU.x <- TwoTail
UBU.x <- max(bounds)
step <- (UBU.x - LBU.x) / 100
Top.x <- c(LBU.x,seq(LBU.x,UBU.x,step),UBU.x)
Top.y <- c(0,dnorm(seq(LBU.x,UBU.x,step),mu,sigma),0)
UBL.x <- -TwoTail
LBL.x <- min(bounds)
step <- (UBL.x - LBL.x) / 100
Bot.x <- c(LBL.x,seq(LBL.x,UBL.x,step),UBL.x)
Bot.y <- c(0,dnorm(seq(LBL.x,UBL.x,step),mu,sigma),0)

pdf("TwoTailedDensityR.pdf",6,5)
par(mar=c(4,4,2,2))
curve(dnorm(x,mu,sigma),xlim=bounds,lwd=3,xlab="Z-score",
      ylab="Probability") 
polygon(Top.x,Top.y,col='red')
polygon(Bot.x,Bot.y,col='red')
# segments(0,0,0,z[500],lwd=2,lty=2)
arrows(0.5,0.1,1.95,0.03,length=0.12,lwd=2)
arrows(-0.5,0.1,-1.95,0.03,length=0.12,lwd=2)
text(0,.12,label=c("Pr(|Z| > 1.96)"))
dev.off()

########
# The real-data example, from the Exercise Three data:

require(RCurl)

URL<-"https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2016-git/master/Exercises/PLSC502-2016-ExerciseFive.csv"
temp<-getURL(URL)
data<-read.csv(textConnection(temp))
rm(temp,URL)

# install.packages("DescTools")
require(DescTools)

data$DOB <- with(data, as.Date(DateOfBirth,
                               format = "%d%b%Y"))
data$Sign <- with(data, Zodiac(DOB))

popmean <- with(data, prop.table(table(1-Active)))[1]
popmean
with(data[data$Sign=="Scorpio",], 
     prop.test(table(1-Active),p=popmean,
               correct=FALSE))
