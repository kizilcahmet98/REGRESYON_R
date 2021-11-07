#veri1
veri1 <- read.csv("C:/Users/ahmet/Desktop/regresyon_odev/veri1.txt", sep="")
attach(veri1)
#Nitel veri ayarlaması
x4<-as.factor(x4)
#Normallik testi
qqnorm(y)
qqline(y)
pairs(veri1)
library(nortest)
ad.test(y)
#Regresyon Modeli
sonuc<-lm(y~x1+x2+x3+x4)
summary(sonuc)
predict(sonuc)
#Aykırı Değerlerin İncelenmesi
influence.measures(sonuc)
inf<-ls.diag(sonuc)
inf
library(zoo) 
cooksd<- cooks.distance(sonuc)
plot(cooksd, pch="*", cex=2, main="InfluentialObsbyCooksdistance") 
abline(h = if (length(veri1$y)>50) 4/length(veri1$y) else 4/(length(veri1$y)-(length(veri1)-1)-1) , col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value") 
abline(h = 2*length(veri1)/length(y) , col="blue")  
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*length(veri1)/length(y),index(hat),""), col="red")
studres<-inf$stud.res
plot(studres, pch="*", cex=2, main="Outlier by Studentized residuals")
abline(h = 3 , col="blue")
text(x=1:length(studres)+1, y=studres, labels=ifelse(studres>3,index(studres),""), col="red")
stdres<-inf$std.res
plot(stdres, pch="*", cex=2, main="Outlier by Standadized residuals") 
abline(h = 2 , col="blue")
text(x=1:length(stdres)+1, y=stdres, labels=ifelse(stdres>2,index(stdres),""), col="red")
rm(veri1,sonuc,inf,cooksd,hat,stdres,studres,x4)

#veri2
veri2 <- read.csv("C:/Users/ahmet/Desktop/regresyon_odev/veri2.txt", sep="")
attach(veri2)
#Nitel veri ayarlaması
x4<-as.factor(x4)
#Normallik testi
qqnorm(y)
qqline(y)
pairs(veri2)
library(nortest)
ad.test(y)
#Regresyon Modeli
sonuc<-lm(y~x1+x2+x3+x4)
summary(sonuc)
predict(sonuc)
#Aykırı Değerlerin İncelenmesi
influence.measures(sonuc)
inf<-ls.diag(sonuc)
inf
library(zoo) 
cooksd<- cooks.distance(sonuc)
plot(cooksd, pch="*", cex=2, main="InfluentialObsbyCooksdistance") 
abline(h = if (length(veri2$y)>50) 4/length(veri2$y) else 4/(length(veri2$y)-(length(veri2)-1)-1) , col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value") 
abline(h = 2*length(veri2)/length(y) , col="blue")  
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*length(veri2)/length(y),index(hat),""), col="red")
studres<-inf$stud.res
plot(studres, pch="*", cex=2, main="Outlier by Studentized residuals")
abline(h = 3 , col="blue")
text(x=1:length(studres)+1, y=studres, labels=ifelse(studres>3,index(studres),""), col="red")
stdres<-inf$std.res
plot(stdres, pch="*", cex=2, main="Outlier by Standadized residuals") 
abline(h = 2 , col="blue")
text(x=1:length(stdres)+1, y=stdres, labels=ifelse(stdres>2,index(stdres),""), col="red")
rm(veri2,sonuc,inf,cooksd,hat,stdres,studres,x4)

#veri3
veri3 <- read.csv("C:/Users/ahmet/Desktop/regresyon_odev/veri3.txt", sep="")
attach(veri3)
#Nitel veri ayarlaması
x4<-as.factor(x4)
#Normallik testi
qqnorm(y)
qqline(y)
pairs(veri3)
library(nortest)
ad.test(y)
#Regresyon Modeli
sonuc<-lm(y~x1+x2+x3+x4)
summary(sonuc)
predict(sonuc)
#Aykırı Değerlerin İncelenmesi
influence.measures(sonuc)
inf<-ls.diag(sonuc)
inf
library(zoo) 
cooksd<- cooks.distance(sonuc)
plot(cooksd, pch="*", cex=2, main="InfluentialObsbyCooksdistance") 
abline(h = if (length(veri3$y)>50) 4/length(veri3$y) else 4/(length(veri3$y)-(length(veri3)-1)-1) , col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value") 
abline(h = 2*length(veri3)/length(y) , col="blue")  
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*length(veri3)/length(y),index(hat),""), col="red")
studres<-inf$stud.res
plot(studres, pch="*", cex=2, main="Outlier by Studentized residuals")
abline(h = 3 , col="blue")
text(x=1:length(studres)+1, y=studres, labels=ifelse(studres>3,index(studres),""), col="red")
stdres<-inf$std.res
plot(stdres, pch="*", cex=2, main="Outlier by Standadized residuals") 
abline(h = 2 , col="blue")
text(x=1:length(stdres)+1, y=stdres, labels=ifelse(stdres>2,index(stdres),""), col="red")
rm(veri3,sonuc,inf,cooksd,hat,stdres,studres,x4)

#veri4
veri4 <- read.csv("C:/Users/ahmet/Desktop/regresyon_odev/veri4.txt", sep="")
attach(veri4)
#Nitel veri ayarlaması
x4<-as.factor(x4)
#Normallik testi
qqnorm(y)
qqline(y)
pairs(veri4)
library(nortest)
ad.test(y)
#Regresyon Modeli
sonuc<-lm(y~x1+x2+x3+x4)
summary(sonuc)
predict(sonuc)
#Aykırı Değerlerin İncelenmesi
influence.measures(sonuc)
inf<-ls.diag(sonuc)
inf
library(zoo) 
cooksd<- cooks.distance(sonuc)
plot(cooksd, pch="*", cex=2, main="InfluentialObsbyCooksdistance") 
abline(h = if (length(veri4$y)>50) 4/length(veri4$y) else 4/(length(veri4$y)-(length(veri4)-1)-1) , col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value") 
abline(h = 2*length(veri4)/length(y) , col="blue")  
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*length(veri4)/length(y),index(hat),""), col="red")
studres<-inf$stud.res
plot(studres, pch="*", cex=2, main="Outlier by Studentized residuals")
abline(h = 3 , col="blue")
text(x=1:length(studres)+1, y=studres, labels=ifelse(studres>3,index(studres),""), col="red")
stdres<-inf$std.res
plot(stdres, pch="*", cex=2, main="Outlier by Standadized residuals") 
abline(h = 2 , col="blue")
text(x=1:length(stdres)+1, y=stdres, labels=ifelse(stdres>2,index(stdres),""), col="red")
rm(veri4,sonuc,inf,cooksd,hat,stdres,studres,x4)

#veri5
veri5 <- read.csv("C:/Users/ahmet/Desktop/regresyon_odev/veri5.txt", sep="")
attach(veri5)
#Nitel veri ayarlaması
x4<-as.factor(x4)
#Normallik testi
qqnorm(y)
qqline(y)
pairs(veri5)
library(nortest)
ad.test(y)
#Regresyon Modeli
sonuc<-lm(y~x1+x2+x3+x4)
summary(sonuc)
predict(sonuc)
#Aykırı Değerlerin İncelenmesi
influence.measures(sonuc)
inf<-ls.diag(sonuc)
inf
library(zoo) 
cooksd<- cooks.distance(sonuc)
plot(cooksd, pch="*", cex=2, main="InfluentialObsbyCooksdistance") 
abline(h = if (length(veri5$y)>50) 4/length(veri5$y) else 4/(length(veri5$y)-(length(veri5)-1)-1) , col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value") 
abline(h = 2*length(veri5)/length(y) , col="blue")  
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*length(veri5)/length(y),index(hat),""), col="red")
studres<-inf$stud.res
plot(studres, pch="*", cex=2, main="Outlier by Studentized residuals")
abline(h = 3 , col="blue")
text(x=1:length(studres)+1, y=studres, labels=ifelse(studres>3,index(studres),""), col="red")
stdres<-inf$std.res
plot(stdres, pch="*", cex=2, main="Outlier by Standadized residuals") 
abline(h = 2 , col="blue")
text(x=1:length(stdres)+1, y=stdres, labels=ifelse(stdres>2,index(stdres),""), col="red")
rm(veri5,sonuc,inf,cooksd,hat,stdres,studres,x4)

#veri6
veri6 <- read.csv("C:/Users/ahmet/Desktop/regresyon_odev/veri6.txt", sep="")
attach(veri6)
#Nitel veri ayarlaması
x4<-as.factor(x4)
#Normallik testi
qqnorm(y)
qqline(y)
pairs(veri6)
library(nortest)
ad.test(y)
#Regresyon Modeli
sonuc<-lm(y~x1+x2+x3+x4)
summary(sonuc)
predict(sonuc)
#Aykırı Değerlerin İncelenmesi
influence.measures(sonuc)
inf<-ls.diag(sonuc)
inf
library(zoo) 
cooksd<- cooks.distance(sonuc)
plot(cooksd, pch="*", cex=2, main="InfluentialObsbyCooksdistance") 
abline(h = if (length(veri6$y)>50) 4/length(veri6$y) else 4/(length(veri6$y)-(length(veri6)-1)-1) , col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value") 
abline(h = 2*length(veri6)/length(y) , col="blue")  
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*length(veri6)/length(y),index(hat),""), col="red")
studres<-inf$stud.res
plot(studres, pch="*", cex=2, main="Outlier by Studentized residuals")
abline(h = 3 , col="blue")
text(x=1:length(studres)+1, y=studres, labels=ifelse(studres>3,index(studres),""), col="red")
stdres<-inf$std.res
plot(stdres, pch="*", cex=2, main="Outlier by Standadized residuals") 
abline(h = 2 , col="blue")
text(x=1:length(stdres)+1, y=stdres, labels=ifelse(stdres>2,index(stdres),""), col="red")
rm(veri6,sonuc,inf,cooksd,hat,stdres,studres,x4)

#veri7
veri7 <- read.csv("C:/Users/ahmet/Desktop/regresyon_odev/veri7.txt", sep="")
attach(veri7)
#Nitel veri ayarlaması
x4<-as.factor(x4)
#Normallik testi
qqnorm(y)
qqline(y)
pairs(veri7)
library(nortest)
ad.test(y)
#Regresyon Modeli
sonuc<-lm(y~x1+x2+x3+x4)
summary(sonuc)
predict(sonuc)
#Aykırı Değerlerin İncelenmesi
influence.measures(sonuc)
inf<-ls.diag(sonuc)
inf
library(zoo) 
cooksd<- cooks.distance(sonuc)
plot(cooksd, pch="*", cex=2, main="InfluentialObsbyCooksdistance") 
abline(h = if (length(veri7$y)>50) 4/length(veri7$y) else 4/(length(veri7$y)-(length(veri7)-1)-1) , col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value") 
abline(h = 2*length(veri7)/length(y) , col="blue")  
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*length(veri7)/length(y),index(hat),""), col="red")
studres<-inf$stud.res
plot(studres, pch="*", cex=2, main="Outlier by Studentized residuals")
abline(h = 3 , col="blue")
text(x=1:length(studres)+1, y=studres, labels=ifelse(studres>3,index(studres),""), col="red")
stdres<-inf$std.res
plot(stdres, pch="*", cex=2, main="Outlier by Standadized residuals") 
abline(h = 2 , col="blue")
text(x=1:length(stdres)+1, y=stdres, labels=ifelse(stdres>2,index(stdres),""), col="red")
rm(veri7,sonuc,inf,cooksd,hat,stdres,studres,x4)

#veri8
veri8 <- read.csv("C:/Users/ahmet/Desktop/regresyon_odev/veri8.txt", sep="")
attach(veri8)
#Nitel veri ayarlaması
x4<-as.factor(x4)
#Normallik testi
qqnorm(y)
qqline(y)
pairs(veri8)
library(nortest)
ad.test(y)
#Regresyon Modeli
sonuc<-lm(y~x1+x2+x3+x4)
summary(sonuc)
predict(sonuc)
#Aykırı Değerlerin İncelenmesi
influence.measures(sonuc)
inf<-ls.diag(sonuc)
inf
library(zoo) 
cooksd<- cooks.distance(sonuc)
plot(cooksd, pch="*", cex=2, main="InfluentialObsbyCooksdistance") 
abline(h = if (length(veri8$y)>50) 4/length(veri8$y) else 4/(length(veri8$y)-(length(veri8)-1)-1) , col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value") 
abline(h = 2*length(veri8)/length(y) , col="blue")  
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*length(veri8)/length(y),index(hat),""), col="red")
studres<-inf$stud.res
plot(studres, pch="*", cex=2, main="Outlier by Studentized residuals")
abline(h = 3 , col="blue")
text(x=1:length(studres)+1, y=studres, labels=ifelse(studres>3,index(studres),""), col="red")
stdres<-inf$std.res
plot(stdres, pch="*", cex=2, main="Outlier by Standadized residuals") 
abline(h = 2 , col="blue")
text(x=1:length(stdres)+1, y=stdres, labels=ifelse(stdres>2,index(stdres),""), col="red")
rm(veri8,sonuc,inf,cooksd,hat,stdres,studres,x4)

#veri9
veri9 <- read.csv("C:/Users/ahmet/Desktop/regresyon_odev/veri9.txt", sep="")
attach(veri9)
#Nitel veri ayarlaması
x4<-as.factor(x4)
#Normallik testi
qqnorm(y)
qqline(y)
pairs(veri9)
library(nortest)
ad.test(y)
#Regresyon Modeli
sonuc<-lm(y~x1+x2+x3+x4)
summary(sonuc)
predict(sonuc)
#Aykırı Değerlerin İncelenmesi
influence.measures(sonuc)
inf<-ls.diag(sonuc)
inf
library(zoo) 
cooksd<- cooks.distance(sonuc)
plot(cooksd, pch="*", cex=2, main="InfluentialObsbyCooksdistance") 
abline(h = if (length(veri9$y)>50) 4/length(veri9$y) else 4/(length(veri9$y)-(length(veri9)-1)-1) , col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value") 
abline(h = 2*length(veri9)/length(y) , col="blue")  
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*length(veri9)/length(y),index(hat),""), col="red")
studres<-inf$stud.res
plot(studres, pch="*", cex=2, main="Outlier by Studentized residuals")
abline(h = 3 , col="blue")
text(x=1:length(studres)+1, y=studres, labels=ifelse(studres>3,index(studres),""), col="red")
stdres<-inf$std.res
plot(stdres, pch="*", cex=2, main="Outlier by Standadized residuals") 
abline(h = 2 , col="blue")
text(x=1:length(stdres)+1, y=stdres, labels=ifelse(stdres>2,index(stdres),""), col="red")
rm(veri9,sonuc,inf,cooksd,hat,stdres,studres,x4)

#veri10
veri10 <- read.csv("C:/Users/ahmet/Desktop/regresyon_odev/veri10.txt", sep="")
attach(veri10)
#Nitel veri ayarlaması
x4<-as.factor(x4)
#Normallik testi
qqnorm(y)
qqline(y)
pairs(veri10)
library(nortest)
ad.test(y)
#Regresyon Modeli
sonuc<-lm(y~x1+x2+x3+x4)
summary(sonuc)
predict(sonuc)
#Aykırı Değerlerin İncelenmesi
influence.measures(sonuc)
inf<-ls.diag(sonuc)
inf
library(zoo) 
cooksd<- cooks.distance(sonuc)
plot(cooksd, pch="*", cex=2, main="InfluentialObsbyCooksdistance") 
abline(h = if (length(veri10$y)>50) 4/length(veri10$y) else 4/(length(veri10$y)-(length(veri10)-1)-1) , col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value") 
abline(h = 2*length(veri10)/length(y) , col="blue")  
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*length(veri10)/length(y),index(hat),""), col="red")
studres<-inf$stud.res
plot(studres, pch="*", cex=2, main="Outlier by Studentized residuals")
abline(h = 3 , col="blue")
text(x=1:length(studres)+1, y=studres, labels=ifelse(studres>3,index(studres),""), col="red")
stdres<-inf$std.res
plot(stdres, pch="*", cex=2, main="Outlier by Standadized residuals") 
abline(h = 2 , col="blue")
text(x=1:length(stdres)+1, y=stdres, labels=ifelse(stdres>2,index(stdres),""), col="red")
rm(veri10,sonuc,inf,cooksd,hat,stdres,studres,x4)

#veri11
veri11 <- read.csv("C:/Users/ahmet/Desktop/regresyon_odev/veri11.txt", sep="")
attach(veri11)
#Nitel veri ayarlaması
x4<-as.factor(x4)
#Normallik testi
qqnorm(y)
qqline(y)
pairs(veri11)
library(nortest)
ad.test(y)
#Regresyon Modeli
sonuc<-lm(y~x1+x2+x3+x4)
summary(sonuc)
predict(sonuc)
#Aykırı Değerlerin İncelenmesi
influence.measures(sonuc)
inf<-ls.diag(sonuc)
inf
library(zoo) 
cooksd<- cooks.distance(sonuc)
plot(cooksd, pch="*", cex=2, main="InfluentialObsbyCooksdistance") 
abline(h = if (length(veri11$y)>50) 4/length(veri10$y) else 4/(length(veri11$y)-(length(veri11)-1)-1) , col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value") 
abline(h = 2*length(veri11)/length(y) , col="blue")  
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*length(veri11)/length(y),index(hat),""), col="red")
studres<-inf$stud.res
plot(studres, pch="*", cex=2, main="Outlier by Studentized residuals")
abline(h = 3 , col="blue")
text(x=1:length(studres)+1, y=studres, labels=ifelse(studres>3,index(studres),""), col="red")
stdres<-inf$std.res
plot(stdres, pch="*", cex=2, main="Outlier by Standadized residuals") 
abline(h = 2 , col="blue")
text(x=1:length(stdres)+1, y=stdres, labels=ifelse(stdres>2,index(stdres),""), col="red")
rm(veri11,sonuc,inf,cooksd,hat,stdres,studres,x4)

#veri12
veri12 <- read.csv("C:/Users/ahmet/Desktop/regresyon_odev/veri12.txt", sep="")
attach(veri12)
#Nitel veri ayarlaması
x4<-as.factor(x4)
#Normallik testi
qqnorm(y)
qqline(y)
pairs(veri12)
library(nortest)
ad.test(y)
#Regresyon Modeli
sonuc<-lm(y~x1+x2+x3+x4)
summary(sonuc)
predict(sonuc)
#Aykırı Değerlerin İncelenmesi
influence.measures(sonuc)
inf<-ls.diag(sonuc)
inf
library(zoo) 
cooksd<- cooks.distance(sonuc)
plot(cooksd, pch="*", cex=2, main="InfluentialObsbyCooksdistance") 
abline(h = if (length(veri12$y)>50) 4/length(veri12$y) else 4/(length(veri12$y)-(length(veri12)-1)-1) , col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value") 
abline(h = 2*length(veri12)/length(y) , col="blue")  
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*length(veri12)/length(y),index(hat),""), col="red")
studres<-inf$stud.res
plot(studres, pch="*", cex=2, main="Outlier by Studentized residuals")
abline(h = 3 , col="blue")
text(x=1:length(studres)+1, y=studres, labels=ifelse(studres>3,index(studres),""), col="red")
stdres<-inf$std.res
plot(stdres, pch="*", cex=2, main="Outlier by Standadized residuals") 
abline(h = 2 , col="blue")
text(x=1:length(stdres)+1, y=stdres, labels=ifelse(stdres>2,index(stdres),""), col="red")
rm(veri12,sonuc,inf,cooksd,hat,stdres,studres,x4)

#veri13
veri13 <- read.csv("C:/Users/ahmet/Desktop/regresyon_odev/veri13.txt", sep="")
attach(veri13)
#Nitel veri ayarlaması
x4<-as.factor(x4)
#Normallik testi
qqnorm(y)
qqline(y)
pairs(veri13)
library(nortest)
ad.test(y)
#Regresyon Modeli
sonuc<-lm(y~x1+x2+x3+x4)
summary(sonuc)
predict(sonuc)
#Aykırı Değerlerin İncelenmesi
influence.measures(sonuc)
inf<-ls.diag(sonuc)
inf
library(zoo) 
cooksd<- cooks.distance(sonuc)
plot(cooksd, pch="*", cex=2, main="InfluentialObsbyCooksdistance") 
abline(h = if (length(veri13$y)>50) 4/length(veri13$y) else 4/(length(veri13$y)-(length(veri13)-1)-1) , col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value") 
abline(h = 2*length(veri13)/length(y) , col="blue")  
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*length(veri13)/length(y),index(hat),""), col="red")
studres<-inf$stud.res
plot(studres, pch="*", cex=2, main="Outlier by Studentized residuals")
abline(h = 3 , col="blue")
text(x=1:length(studres)+1, y=studres, labels=ifelse(studres>3,index(studres),""), col="red")
stdres<-inf$std.res
plot(stdres, pch="*", cex=2, main="Outlier by Standadized residuals") 
abline(h = 2 , col="blue")
text(x=1:length(stdres)+1, y=stdres, labels=ifelse(stdres>2,index(stdres),""), col="red")
rm(veri13,sonuc,inf,cooksd,hat,stdres,studres,x4)

#veri14
veri14 <- read.csv("C:/Users/ahmet/Desktop/regresyon_odev/veri14.txt", sep="")
attach(veri14)
#Nitel veri ayarlaması
x4<-as.factor(x4)
#Normallik testi
qqnorm(y)
qqline(y)
pairs(veri14)
library(nortest)
ad.test(y)
#Regresyon Modeli
sonuc<-lm(y~x1+x2+x3+x4)
summary(sonuc)
predict(sonuc)
#Aykırı Değerlerin İncelenmesi
influence.measures(sonuc)
inf<-ls.diag(sonuc)
inf
library(zoo) 
cooksd<- cooks.distance(sonuc)
plot(cooksd, pch="*", cex=2, main="InfluentialObsbyCooksdistance") 
abline(h = if (length(veri14$y)>50) 4/length(veri14$y) else 4/(length(veri14$y)-(length(veri14)-1)-1) , col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value") 
abline(h = 2*length(veri14)/length(y) , col="blue")  
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*length(veri14)/length(y),index(hat),""), col="red")
studres<-inf$stud.res
plot(studres, pch="*", cex=2, main="Outlier by Studentized residuals")
abline(h = 3 , col="blue")
text(x=1:length(studres)+1, y=studres, labels=ifelse(studres>3,index(studres),""), col="red")
stdres<-inf$std.res
plot(stdres, pch="*", cex=2, main="Outlier by Standadized residuals") 
abline(h = 2 , col="blue")
text(x=1:length(stdres)+1, y=stdres, labels=ifelse(stdres>2,index(stdres),""), col="red")
rm(veri14,sonuc,inf,cooksd,hat,stdres,studres,x4)

#veri15
veri15 <- read.csv("C:/Users/ahmet/Desktop/regresyon_odev/veri15.txt", sep="")
attach(veri15)
#Nitel veri ayarlaması
x4<-as.factor(x4)
#Normallik testi
qqnorm(y)
qqline(y)
pairs(veri15)
library(nortest)
ad.test(y)
#Regresyon Modeli
sonuc<-lm(y~x1+x2+x3+x4)
summary(sonuc)
predict(sonuc)
#Aykırı Değerlerin İncelenmesi
influence.measures(sonuc)
inf<-ls.diag(sonuc)
inf
library(zoo) 
cooksd<- cooks.distance(sonuc)
plot(cooksd, pch="*", cex=2, main="InfluentialObsbyCooksdistance") 
abline(h = if (length(veri15$y)>50) 4/length(veri15$y) else 4/(length(veri15$y)-(length(veri15)-1)-1) , col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value") 
abline(h = 2*length(veri15)/length(y) , col="blue")  
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*length(veri15)/length(y),index(hat),""), col="red")
studres<-inf$stud.res
plot(studres, pch="*", cex=2, main="Outlier by Studentized residuals")
abline(h = 3 , col="blue")
text(x=1:length(studres)+1, y=studres, labels=ifelse(studres>3,index(studres),""), col="red")
stdres<-inf$std.res
plot(stdres, pch="*", cex=2, main="Outlier by Standadized residuals") 
abline(h = 2 , col="blue")
text(x=1:length(stdres)+1, y=stdres, labels=ifelse(stdres>2,index(stdres),""), col="red")
rm(veri15,sonuc,inf,cooksd,hat,stdres,studres,x4)

#veri16
veri16 <- read.csv("C:/Users/ahmet/Desktop/regresyon_odev/veri16.txt", sep="")
attach(veri16)
#Nitel veri ayarlaması
x4<-as.factor(x4)
#Normallik testi
qqnorm(y)
qqline(y)
pairs(veri16)
library(nortest)
ad.test(y)
#Regresyon Modeli
sonuc<-lm(y~x1+x2+x3+x4)
summary(sonuc)
predict(sonuc)
confint(sonuc, level=.95)
#Aykırı Değerlerin İncelenmesi
influence.measures(sonuc)
inf<-ls.diag(sonuc)
inf
library(zoo) 
cooksd<- cooks.distance(sonuc)
plot(cooksd, pch="*", cex=2, main="InfluentialObsbyCooksdistance") 
abline(h = if (length(veri16$y)>50) 4/length(veri16$y) else 4/(length(veri16$y)-(length(veri16)-1)-1) , col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value") 
abline(h = 2*length(veri16)/length(y) , col="blue")  
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*length(veri16)/length(y),index(hat),""), col="red")
studres<-inf$stud.res
plot(studres, pch="*", cex=2, main="Outlier by Studentized residuals")
abline(h = 3 , col="blue")
text(x=1:length(studres)+1, y=studres, labels=ifelse(studres>3,index(studres),""), col="red")
stdres<-inf$std.res
plot(stdres, pch="*", cex=2, main="Outlier by Standadized residuals") 
abline(h = 2 , col="blue")
text(x=1:length(stdres)+1, y=stdres, labels=ifelse(stdres>2,index(stdres),""), col="red")
rm(veri16,sonuc,inf,cooksd,hat,stdres,studres,x4)

#veri17
veri17 <- read.csv("C:/Users/ahmet/Desktop/regresyon_odev/veri17.txt", sep="")
attach(veri17)
#Nitel veri ayarlaması
x4<-as.factor(x4)
#Normallik testi
qqnorm(y)
qqline(y)
pairs(veri17)
library(nortest)
ad.test(y)
#Regresyon Modeli
sonuc<-lm(y~x1+x2+x3+x4)
summary(sonuc)
predict(sonuc)
confint(sonuc, level=.95)
cor(veri17)
#Aykırı Değerlerin İncelenmesi
influence.measures(sonuc)
inf<-ls.diag(sonuc)
inf
library(zoo) 
cooksd<- cooks.distance(sonuc)
plot(cooksd, pch="*", cex=2, main="InfluentialObsbyCooksdistance") 
abline(h = if (length(veri17$y)>50) 4/length(veri17$y) else 4/(length(veri17$y)-(length(veri17)-1)-1) , col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value") 
abline(h = 2*length(veri17)/length(y) , col="blue")  
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*length(veri17)/length(y),index(hat),""), col="red")
studres<-inf$stud.res
plot(studres, pch="*", cex=2, main="Outlier by Studentized residuals")
abline(h = 3 , col="blue")
text(x=1:length(studres)+1, y=studres, labels=ifelse(studres>3,index(studres),""), col="red")
stdres<-inf$std.res
plot(stdres, pch="*", cex=2, main="Outlier by Standadized residuals") 
abline(h = 2 , col="blue")
text(x=1:length(stdres)+1, y=stdres, labels=ifelse(stdres>2,index(stdres),""), col="red")
#Değişken Varyanslılık Sorunu İncelenmesi
library(lmtest)
bptest(sonuc)
par(mfrow=c(2,2))
plot(predict(sonuc),inf$stud.res, ylab = "studentized residuals" , xlab = "predict value")
#Öz İlişki Sorunu İncelenmesi
dwtest(sonuc)
#çoklu Bağlantı Sorunu İncelenmesi
detach("package:car", unload=TRUE)
library(DAAG)
library(perturb)
colldiag(model.matrix(sonuc))
vif(sonuc)
ort1<-mean(x1)
kt1<-sum((x1-ort1)^2)
skx1<-(x1-ort1)/(kt1^0.5)
ort2<-mean(x2)
kt2<-sum((x2-ort2)^2)
skx2<-(x2-ort2)/(kt2^0.5)
ort3<-mean(x3)
kt3<-sum((x3-ort3)^2)
skx3<-(x3-ort3)/(kt3^0.5)
x<-cbind(skx1,skx2,skx3)
sm<- eigen (t(x)%*%x)
signif(sm$values,3)
signif(sm$vectors,3)
V<-sm$vectors
t(V)%*%V
V %*% diag(sm$values) %*% t(V)
#Uyum Kestirimi
a <- matrix(c(1,0,0,0,1,0),3,2)
a
uyum <- c(10.137,0.951586,5.650,1,0)
names(uyum) <- c("x1","x2","x3","x42","x43")
uyum
yi <- -9.630918+8.641663*uyum[1]+4.319856*uyum[2]-3.740896*uyum[3]-2.994661*uyum[4]-4.995958*uyum[5]
yi
#Ön Kestirimi
a <- matrix(c(1,0,0,0,1,0),3,2)
a
on <- c(11.32,0.278585,6.875,0,0)
names(on) <- c("x1","x2","x3","x42","x43")
on
ysapkai <- -9.630918+8.641663*on[1]+4.319856*on[2]-3.740896*on[3]-2.994661*on[4]-4.995958*on[5]
ysapkai
#Güven Aralıkları
confint(sonuc, level=.99)
#İleri Doğru Değişken Seçimi
library(stats)
lm.null <- lm(y ~ 1)
forward <- step(lm.null,lny~x1+x2+x3+x4, direction = "forward")
forward
summary(forward)
#Geriye Doğru Değişken Seçimi
backward<-step(sonuc,direction="backward")
summary(backward)
#Adımsal Değişken Seçimi
library(MASS)
step.model <- stepAIC(sonuc, direction = "both", trace = FALSE)
step.model
summary(step.model)


