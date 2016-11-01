#####################################
# PLSC 502 -- Fall 2016
#
# Day Eighteen materials
#####################################
# Packages:

require(RCurl)
require(gmodels)

options(digits=5)

DHURL<-"https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2016-git/master/Data/femins.csv"
temp<-getURL(DHURL)
Fem<-read.csv(textConnection(temp))
rm(temp,DHURL)

summary(Fem)

# Tables, etc.

oneway<-with(Fem, table(feminsult))
oneway

with(Fem, 
  chisq.test(table(feminsult)))

# Region

region<-with(Fem, table(feminsult,cenreg))
addmargins(region)
prop.table(region)
prop.table(region,1)
prop.table(region,2)
chisq.test(region)

# Alternative:

region2<-with(Fem,
            CrossTable(feminsult,cenreg,prop.chisq=FALSE,chisq=TRUE))

# Three-Way:

threeway<-with(Fem, table(feminsult,cenreg,intrace))
addmargins(threeway)
chisq.test(threeway)

# Small cell frequencies:

with(Fem, table(feminsult,race))
with(Fem, chisq.test(table(feminsult,race)))

with(Fem, fisher.test(table(feminsult,race), workspace=20000000))
