#####################################
# PLSC 502 -- Fall 2016
#
# Day Three materials
#####################################

library(RCurl)

# Get Africa (2001) data:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2016-git/master/Data/africa2001.csv")
Africa<-read.csv(text=temp, header=TRUE)
rm(temp)

# Summary statistics:

summary(Africa)

# Dotchart, population (in millions)

pdf("PopulationDotchartR.pdf",6,5)
with(Africa, dotchart(popthou/1000,pch=19,labels=country,
             cex=0.5,xlab="Population in Millions"))
dev.off()

# Dotchart redux (sorted):

Africa<-Africa[order(Africa$popthou),]

pdf("PopulationDotchartR2.pdf",6,5)
with(Africa, dotchart(popthou/1000,pch=19,labels=country,
                      cex=0.5,xlab="Population in Millions"))
abline(v=c(20,40,60,80,100,120),lty=2,lwd=1)
dev.off()

# Barchart (just like a dotchart...):

pdf("PopulationBarchartR.pdf",6,5)
with(Africa, barplot(popthou/1000,horiz=TRUE,names.arg=country,
             las=1,cex.names=0.5,xlab="Population in Millions"))
dev.off()

# Histogram, Muslim percentage:

pdf("MuslimPercentHistogramR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, hist(muslperc,breaks=10,col="grey",main=" ",
             xlab="Muslim Percentage Of The Population"))
dev.off()

# Histogram, Sub-saharan:

pdf("SubsaharanHistogramR.pdf",6,5)
par(mar=c(4,4,2,2))
xx<-with(Africa, barplot(table(subsaharan),col="grey",main=" ",
         xlab="Region",ylim=c(0,45),
         beside=TRUE,xpd=FALSE))
# Add Ns to top of bars:
with(Africa, text(xx, table(subsaharan),pos=3,
         labels=paste("N = ",c(table(subsaharan)),sep="")))
dev.off()

# Kernel density:

pdf("MuslimPctKDensityR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, plot(density(muslperc),t="l",lwd=2,main="",
             xlab="Muslim Percentage Of The Population",
             xlim=c(0,100)))
dev.off()

# Overlay histogram and density plot:

pdf("MuslimPctHistoDensityR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, hist(muslperc,breaks=10,col="grey",main=" ",
                  xlab="Muslim Percentage Of The Population",
                  freq=FALSE))
with(Africa, lines(density(muslperc),t="l",lwd=2))
dev.off()

# Density plot, Health Expenditures:

pdf("HealthExpKDensityR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, plot(density(healthexp),t="l",lwd=2,main="",
                  xlab="Health Expenditures, Percent of GDP"))
dev.off()

# Q-Q plot, Health Expenditures:

pdf("HealthExpQQR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, qqnorm(healthexp,main="",
                  ylab="Health Expenditures, Percent of GDP"))
with(Africa, qqline(healthexp,lwd=2))
dev.off()

# Q-Q plot, Muslim Percentage:

pdf("MuslimPctQQR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, qqnorm(muslperc,main="",
                    ylab="Muslim Percentage of the Population"))
with(Africa, qqline(muslperc,lwd=2))
dev.off()

# Boxplots, Muslim percentage:

pdf("MuslimPctBoxplotR.pdf",6,5)
par(mar=c(2,4,2,2))
with(Africa, boxplot(muslperc,main="",
                    ylab="Muslim Percentage of the Population"))
dev.off()

# Boxplots, three variables:

pdf("MultipleBoxplotR.pdf",6,5)
par(mar=c(2,4,2,2))
boxplot(Africa[,c("muslperc","literacy","adrate")],main="",
        ylab="Percentage",names=c("Muslim Pct.","Literacy","HIV Rate"))
dev.off()

# fin