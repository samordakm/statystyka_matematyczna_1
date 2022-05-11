# lab - 10.05.2021

##### Zadanie 6 #####
dane=read.csv2("http://www.ibspan.waw.pl/~pgrzeg/stat_lab/samochody.csv")
head(dane)

b <- boxplot(dane$cena)
out <- b$out

which(dane$cena == out[1])
which(dane$cena == out[2])
which(dane$cena == out[3])
which(dane$cena == out[4])

#### Uzupe³nienie
#Q_3+1.5*IQR
#Q_1-1.5*IQR


#a)
dane$zp=378.5/(1.609*dane$mpg)
head(dane)

x <- dane$zp
mean(x) 
is.na(x)

which(is.na(x))

mean(x, na.rm=T)

# lub
#xx=na.omit(x)
#mean(xx)

#d)
h <- hist(dane$zp, prob=T)
lines(h$mids, h$density, type="l", col="maroon")
h$mids

nowe_dane <- dane$zp[is.na(dane$zp)==F]


#?denisty
lines(density(nowe_dane), col="blue")

?density

#wybor jadra
g1 <- density(nowe_dane, kernel="gaussian")
g2 <- density(nowe_dane, kernel="epanechnikov")
g3 <- density(nowe_dane, kernel="triangular")
g4 <- density(nowe_dane, kernel="cosine")

par(mfrow=c(2,2))
plot(g1)
plot(g2)
plot(g3)
plot(g4)

#wybor bandwidth
#szerokosc okna

?density
g5 <- density(nowe_dane, kernel="epanechnikov", bw=0.2)
g6 <- density(nowe_dane, kernel="epanechnikov")
g7 <- density(nowe_dane, kernel="epanechnikov", bw=1.2)
par(mfrow=c(1,3))
plot(g5)
plot(g6)
plot(g7)

g8 <- density(nowe_dane, kernel="epanechnikov", bw="ucv")
g9 <- density(nowe_dane, kernel="epanechnikov", bw="SJ")
g10 <- density(nowe_dane, kernel="epanechnikov")
par(mfrow=c(1,3))
plot(g8)
plot(g9)
plot(g10)


##### Zadanie 8 #####
#filtrowanie zmiennych
par(mfrow=c(1,1))
boxplot(dane$zp ~ dane$producent, horzontal=F, names=c("A", "E", "J")) #anova

zpA=dane$zp[dane$producent==1]
zpE=dane$zp[dane$producent==2]
zpJ=dane$zp[dane$producent==3]

boxplot(zpA, zpE, zpJ, horizontal = F, names=c("A", "E", "J"))

tapply(dane$zp, dane$producent, length)
tapply(dane$zp, dane$producent, sd)
tapply(dane$zp, dane$producent, summary)


##### Zadanie 9 #####
#table()
#boxplot()

boxplot(dane$zp ~ dane$cylindry)
table(dane$cylindry) 

tapply(dane$zp, dane$cylindry, length)
tapply(dane$zp, dane$cylindry, summary)
tapply(dane$zp, dane$cylindry, sd)

################## zadanie 10 ###################


#sp.1 
dane$waga2 <- cut(dane$waga, c(0,2500, Inf), labels = c("<=2500", ">2500"))

par(mfrow=c(1,2))
boxplot(dane$zp ~ dane$waga2)
barplot(table(dane$waga2))

#sp.2
zpwag <- dane$zp[dane$waga<2500]
zpwag2 <- dane$zp[dane$waga>=2500]
par(mfrow=c(1,2))
hist(zpwag, prob=T, xlim=c(4,17), ylim=c(0,0.65))
hist(zpwag2, prob=T, xlim=c(4,17), ylim=c(0,0.65))

################## zadanie 14 ###################

tapply(dane$przysp, dane$producent, summary)

przyspA=dane$przysp[dane$producent==1]
przyspJ=dane$przysp[dane$producent==3]
summary(przyspA)
summary(przyspJ)

par(mfrow=c(1,2))
boxplot(przyspA, przyspJ, horizontal=F, names=c("Ameryka","Japonia"))
boxplot(zpA, zpJ, horizontal = F, names=c("Ameryka", "Japonia"))

##### Uzupe³nienie #####
#install.packages("e1071")
library(e1071)
skewness(dane$zp) #nie radzi sobie z brakami danych

skewness(nowe_dane)
kurtosis(nowe_dane)

# kurtoza - mierzy co sie dzieje w ogonach rozkaldu
#dla normalnego = 0
#===> K = 0 – intensywnosc wartosci skrajnych jest podobna do intensywnosci wartoœci skrajnych rozk³adu normalnego 
#(dla którego kurtoza wynosi dok³adnie 0)
#===> K > 0 – intensywnosc wartosci skrajnych jest wieksza ni¿ dla rozk³adu normalnego ("ogony" rozk³adu s¹ "grubsze")
#===> K < 0 – intensywnosc wartosci skrajnych jest mniejsza ni¿ w przypadku rozk³adu normalnego ("wê¿sze ogony" rozk³adu).


#skosnosc
#=0 rozkald idealnie symetryczny
#<0 wskazuja na rozklad lewostronnie skosny (wydluzone jest lewe ramie rozkladu)
#>0 prawostronnie skosny


##### Uzupe³nienie #####

#boxploty

#identyfikacja outliersow
#Q_1-1.5*IQR
#Q_3+1.5*IQR

?boxplot