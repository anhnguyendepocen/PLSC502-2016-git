#####################################
# PLSC 502 -- Fall 2016
#
# Day Four materials
#####################################

library(RCurl)

# Get Africa (2001) data:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2016-git/master/Data/africa2001.csv")
Africa<-read.csv(text=temp, header=TRUE)
rm(temp)
Africa<-Africa[complete.cases(Africa)==TRUE,]

#######################################
# Scatterplots
#
# Basic scatterplot:

pdf("MuslimLiteracyScatterplotR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, plot(muslperc,literacy))
dev.off()

# Nicer scatterplot:

pdf("AltMuslimLiteracyScatterplotR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, plot(muslperc,literacy,pch=19,ylab="Adult Literacy Rate",
                  xlab="Muslim Percentage of the Population",
                  ylim=c(10,100)))
with(Africa, text(muslperc,literacy,labels=cabbr,pos=3,cex=0.8))
abline(h=mean(Africa$literacy,na.rm=TRUE),lty=2)
abline(v=mean(Africa$muslperc,na.rm=TRUE),lty=2)
dev.off()

# Skewed data:

pdf("TradeGDPScatterplotR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, plot(gdppppd,tradegdp,pch=19,
     xlab="GDP Per Capita",ylab="Trade (% GDP)"))
dev.off()

# Skewed data, logged:

pdf("LoggedTradeGDPScatterplotR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, plot(gdppppd,tradegdp,pch=19,log="xy",
                  xlab="Logged GDP Per Capita",
                  ylab="LoggedTrade (% GDP)"))
dev.off()

# Binned data:

pdf("PolityWarScatterplotR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, plot(polity,intensity,pch=19,yaxp=c(0,3,3),
             xlab="POLITY Score",ylab="Conflict Intensity"))
dev.off()

# Avec jitter:

pdf("JitteredPolityWarScatterplotR.pdf",6,5)
set.seed(7222009)
par(mar=c(4,4,2,2))
with(Africa, plot(polity,jitter(intensity,1),pch=19,yaxp=c(0,3,3),
                  xlab="POLITY Score",ylab="Conflict Intensity"))
dev.off()

# How Not To Draw A Scatterplot:

pdf("SubsaharanCivilWarScatterplotR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, plot(as.numeric(subsaharan)-1,internalwar,pch=19,
                  xaxp=c(0,1,1),yaxp=c(0,1,1),xlab="Region",
                  ylab="Civil War"))
dev.off()

# Frequency tables are better:

with(Africa, xtabs(~subsaharan+internalwar))

# Binary-Continuous:

pdf("PolityCivilWarScatterplotR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, plot(polity,internalwar,pch=19,
                  yaxp=c(0,1,1),xlab="POLITY Score",
                  ylab="Civil War"))
dev.off()

# Add lowess:

pdf("LowessPolityCivilWarScatterplotR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, plot(lowess(polity,internalwar),xlab="POLITY Score",
                  ylab="Civil War",t="l",lwd=2,ylim=c(-0.1,1),
                  xlim=c(-10,10)))
with(Africa, points(polity,internalwar,pch=19))
dev.off()

# Bivariate boxplots:

pdf("SubsaharanHIVBoxplotsR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, boxplot(adrate~subsaharan,xlab="Region",
                  ylab="HIV Pravelence Rate"))
dev.off()

# Multiple conditioned boxplots:

pdf("SubsaharanMultipleBoxplotsR.pdf",9,4)
par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
with(Africa, boxplot(muslperc~subsaharan,xlab="Region",
             ylab="Muslim Percentage",cex=0.6,
             main="Muslim Percentage"))
with(Africa, boxplot(literacy~subsaharan,xlab="Region",
             ylab="Literacy Rate",cex=0.6,
             main="Literacy"))
with(Africa, boxplot(adrate~subsaharan,xlab="Region",
             ylab="HIV Prevalence Rate",cex=0.6,
             main="HIV Rate"))
dev.off()

# QQ-plot comparisons

library(lattice)

pdf("HIV-QQ-WarR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, qq(internalwar~adrate, col="black",pch=20,
             xlab="No Internal War", ylab="Internal War"))
dev.off()

# Multivariate plots...
#
# Scatterplot matrix:

install.packages("car")
library("car")

pdf("ScatterplotMatrixAfricaR.pdf",6,5)
par(mar=c(4,4,2,2))
dd <- Africa[,c("gdppppd","tradegdp","muslperc",
               "literacy","adrate")]
scatterplotMatrix(dd,reg.line=FALSE,smoother=FALSE,pch=19,
     var.labels=c("GDP","Trade","Muslim Percent","Literacy","HIV Rate"))
dev.off()

# Conditional scatterplots:

pdf("GDP-HIV-Region-R.pdf",7,5)
par(mfrow=c(1,2)) # <- Create a combined plot: 1 row, 2 columns
par(mar=c(4,4,4,2))
with(Africa[Africa$subsaharan=="Not Sub-Saharan",],
     plot(gdppppd,adrate,pch=19,main="Not Sub-Saharan",log="x",
     xlab="GDP Per Capita",ylab="HIV Prevalence"))
with(Africa[Africa$subsaharan=="Sub-Saharan",],
     plot(gdppppd,adrate,pch=19,main="Sub-Saharan",log="x",
          xlab="GDP Per Capita",ylab="HIV Prevalence"))
dev.off()

# Contour plot (requiresakima package):

library(akima)

cpdata <- with(Africa, interp(muslperc,literacy,adrate,
                       duplicate="mean"))

pdf("MuslimLiteracyHIVContourR.pdf",7,5)
par(mar=c(4,4,2,2))
filled.contour(cpdata,color.palette=topo.colors,
               xlab="Muslim Percentage",
               ylab="Literacy")
dev.off()


# "3-D" scatterplot:

pdf("AltMuslimHIVLiteracyScatterR.pdf",6,5)
par(mar=c(2,2,2,2))
cloud(adrate~literacy*muslperc,Africa,col="red",
      pch=20)
dev.off()

# Interactive "3D scatterplot"

library(rgl)
with(Africa, plot3d(muslperc,literacy,adrate,
             size=0.8, col="red",type="s",
             xlab="Muslim Percentage",
             ylab="Literacy",zlab="HIV Rate"))
with(Africa, plot3d(muslperc,literacy,adrate,
     size=1, type="h",xlab="Muslim Percentage",
     ylab="Literacy",zlab="HIV Rate",add=TRUE)) # (Add lines)
     
rgl.postscript("InteractiveMuslimLiteracyHIVScatter.pdf",
               fmt="pdf") # (Save the output)

# Multivariate Data display:

Africa$big<-factor(Africa$population>median(Africa$population),
                   labels=c("Big","Small")) # Splitting population at its median
Africa$civilwar<-factor(Africa$internalwar,
                 labels=c("No Civil War","Civil War")) # creating a "factor" variable for civil war

pdf("HIVLiteracySizeCivilWarScatterR.pdf",6,5)
with(Africa, xyplot(adrate~muslperc | civilwar * big,
             col="black",panel=function(x,y){panel.xyplot(x,y);
             panel.loess(x,y,span=1)},
             xlab="Muslim Percentage",ylab="HIV Rate"))
dev.off()
