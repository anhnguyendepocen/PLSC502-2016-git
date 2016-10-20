#####################################
# PLSC 502 -- Fall 2016
#
# Day Sixteen materials
#####################################
# Packages:

require(RCurl)
require(pastecs)

options(digits=4)

# Assumptions plot:

x<-seq(-6,16,by=0.1)

pdf("DOMExampleR.pdf",6,5)
par(mar=c(4,4,2,2))
curve(dnorm(x,4,3),-6,16,lwd=2,ylab="Probability",ylim=c(0,0.4),
      xlab="Distribution of Y")
lines(x,dnorm(x,12,1),lwd=2,lty=2,col="red")
legend("topleft",bty="n",lty=c(1,2),lwd=2,col=c("black","red"),
       legend=c("Y|X=0","Y|X=1"))
text(0,0.12,col="black",labels=expression(paste(mu,"=4, ",sigma^2,"=9")))
text(8.5,0.25,col="red",labels=expression(paste(mu,"=12, ",sigma^2,"=1")))
segments(4,0,4,dnorm(4,4,3),lty=3)
segments(12,0,12,dnorm(12,12,1),lty=3,col="red")
dev.off()

#  Africa data example:

WBURL<-"https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2016-git/master/Data/africa2001.csv"
temp<-getURL(WBURL)
Africa<-read.csv(textConnection(temp))
rm(temp,WBURL)

# Summary:

stat.desc(Africa$adrate)

# HIV rates, by region:

with(Africa[Africa$subsaharan=="Not Sub-Saharan",], 
     stat.desc(adrate))

with(Africa[Africa$subsaharan=="Sub-Saharan",], 
     stat.desc(adrate))

# t-tests:

with(Africa, t.test(adrate~subsaharan))

with(Africa, t.test(literacy~subsaharan))


