#####################################
# PLSC 502 -- Fall 2016
#
# Day Twenty-Three materials
#####################################
# Packages, etc.:

options(digits=3)

require(RCurl) 
require(DescTools)
require(psych)
require(car)

#######################
# Infant mortality and DPT immunizations...

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2016-git/master/Data/CountryData2000.csv")
Data <- read.csv(text = url) # read the "votes" data
rm(url)

# Summarize:

IMdata<-na.omit(Data[c("infantmortalityperK","DPTpct","WBcode")])

with(IMdata, describe(infantmortalityperK))
with(IMdata, describe(DPTpct))

# Scatterplot:

pdf("IMDPT.pdf",7,6) # <- create PDF
par(mar=c(4,4,2,2))
with(IMdata, plot(DPTpct,infantmortalityperK,pch="",
                xlab="DPT Immunization Percentage",
                ylab="Infant Mortality (Deaths per 1000 Births)"))
with(IMdata, text(DPTpct,infantmortalityperK,labels=WBcode))
with(IMdata, abline(v=mean(DPTpct,na.rm=TRUE),lty=2))
with(IMdata, abline(h=mean(infantmortalityperK,na.rm=TRUE),lty=2))
dev.off()

IMDPT<-with(IMdata, lm(infantmortalityperK~DPTpct))
summary(IMDPT)   # regression
anova(IMDPT)     # ANOVA

# Other things:

vcov(IMDPT)
confint(IMDPT)
confint(IMDPT,level=0.99)
SEs<-predict(IMDPT,interval="confidence")
SEs

# Plot:

Sort<-order(IMdata$DPTpct)

pdf("IMDPT-CI.pdf",6,5)
plot(IMdata$DPTpct,IMdata$infantmortalityperK,pch=20,
     xlab="DPT Immunization Percentage",
     ylab="Infant Mortality Per 1000 Births")
abline(IMDPT,lwd=3)
lines(sort(IMdata$DPTpct),SEs[Sort,2],col="red",lwd=2,lty=2)
lines(sort(IMdata$DPTpct),SEs[Sort,3],col="red",lwd=2,lty=2)
dev.off()

