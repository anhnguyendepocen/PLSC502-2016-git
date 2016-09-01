#####################################
# PLSC 502 -- Fall 2016
#
# Day Two materials
#####################################

library(RCurl)

# DH data:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2016-git/master/Data/DH.csv")
DH<-read.csv(text=temp, header=TRUE)
rm(temp)

select<-c("respon","age","female","followbaseball","DH_appr")
head(DH[select],8)

# Clerks / time-series:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2016-git/master/Data/annualclerks.csv")
Clerks<-read.csv(text=temp, header=TRUE)
rm(temp)

select<-c("Term","female","white","top5law","lcclerk")
head(Clerks[select],15)

# Panel/TSCS country data:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2016-git/master/Data/Assass.csv")
Panel<-read.csv(text=temp, header=TRUE)
rm(temp)

select<-c("country","ccode","year","gdppc","polity","region","coldwar")
Panel<-Panel[order(Panel$ccode,Panel$year),] # sort
Panel[1:200,select]

# Relational data: Country "dyads" (1968):

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2016-git/master/Data/dyads1968.csv")
Dyads<-read.csv(text=temp, header=TRUE)
rm(temp)

select<-c("ccode1","ccode2","dyadid","dem1","dem2","allies","distance")
Dyads[1:300,select]

# fin