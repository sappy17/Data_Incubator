## Input Tables 

## Human development index (HDI)- 1990 to 2015
hdi<- read.csv("C:\\Users\\sapta\\Desktop\\Python\\Challenge\\New folder\\Happyness\\Human development index (HDI).csv", stringsAsFactors = F)
## Education Index
edui<- read.csv("C:\\Users\\sapta\\Desktop\\Python\\Challenge\\New folder\\Happyness\\Education Index.csv", stringsAsFactors = F)
## Life expectancy Index 
lei<- read.csv("C:\\Users\\sapta\\Desktop\\Python\\Challenge\\New folder\\Happyness\\Life expectancy Index.csv",stringsAsFactors = F)
## Life expectancy by birth
le_b<- read.csv("C:\\Users\\sapta\\Desktop\\Python\\Challenge\\New folder\\Happyness\\Life expectancy at birth (years).csv",stringsAsFactors = F)
## Government expenditure on education (% of GDP)
gee<- read.csv("C:\\Users\\sapta\\Desktop\\Python\\Challenge\\New folder\\Happyness\\Government expenditure on education (% of GDP).csv",stringsAsFactors = F)

## 2015 Data
## Happyness Index 2015
h_2015<- read.csv("C:\\Users\\sapta\\Desktop\\Python\\Challenge\\New folder\\Happyness\\world-happiness-report\\2015.csv", stringsAsFactors = F)
# hdi_2015<- subset(hdi, select= c(HDI.Rank..2015. , X2015))
# edui_2015<- subset(edui, select = X2015)
# lei_2015<- subset(lei, select=X2015)
# le_b_2015<- subset(le_b, select = X2015 )


par(mfrow=c(2,2))
plot(hdi$HDI.Rank..2015.,hdi$X2015, main = "HDI", xlab="", ylab="HDI")
plot(edui$HDI.Rank..2015.~edui$X2015, main = "Education Index", xlab="", ylab="Education Index")
plot(lei$HDI.Rank..2015.~lei$X2015, main= "Life expectancy Index" , xlab="HDI Rank", ylab= "Life expectancy Index")
plot(le_b$HDI.Rank..2015.~le_b$X2015, main= "Life expectancy by birth", xlab="HDI Rank", ylab="Life expectancy by birth")

l_h<-lm(hdi$HDI.Rank..2015.~hdi$X2015)
summary(l_h)
opar=par(mfrow=c(2,3))
plot(l_h,1:6,ask=F)


l_e<- lm (edui$HDI.Rank..2015.~edui$X2015)
summary(l_e)
opar=par(mfrow=c(2,3))
plot(l_e,1:6,ask=F)

l_lei<-lm(lei$HDI.Rank..2015.~lei$X2015)
summary(l_lei)
opar=par(mfrow=c(2,3))
plot(l_lei,1:6,ask=F)

l_le_b<- lm(le_b$HDI.Rank..2015.~le_b$X2015)
opar=par(mfrow=c(2,3))
plot(l_le_b,1:6,ask=F)

l_gee=lm(gee$HDI.Rank..2015.~gee$X2014)
opar=par(mfrow=c(2,3))
plot(l_gee,1:6,ask=F)