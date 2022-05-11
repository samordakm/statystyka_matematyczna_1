############        zestaw 2         ############
a <- c(23.3, 24.5, 25.3, 25.3, 24.3, 24.8, 25.2, 24.5, 24.6, 24.1, 24.3, 26.1)
mean(a)
median(a)
min(a)
max(a)
IQR(a)



boxplot(a)


b <- c(19, 23.3, 24.5, 25.3, 25.3, 24.3, 24.8, 25.2, 33, 24.5, 24.6, 24.1, 24.3, 26.1)
boxplot(b)











################## zadanie 1 ###################
#licznosci klas ni - wektor liczbowy
ni=c(17364,56128,11239,8170)
ni
class(ni) #sprawdza typ - numeryczny

#dlugosc wektora ni = liczna klas
length(ni)

#wybranie 1-szej wspolrzednej
ni[1]
#wybranie 2 i 3 wspolrzednej
ni[c(2,3)]

#suma el-tow ni = licznosc proby
sum(ni) #czyli suma wszystkich badanych

#etykiety - wektor napis??w
klasa=c("panny", "mezatki", "wdowy", "rozwodki")
klasa
class(klasa)

# wykres kolowy

?pie

pie(ni,labels=klasa,main= "Stan cywilny kobiet w USA")
#main - tytul ca??osci

#label - dolaczanie etykiet
#procent obserwacji w kazdej klasie
pi=100*ni/sum(ni)
pi
round(pi,2)

# etykiety bardziej ladne

etykiety=paste(klasa,"-",round(pi,2),"%")
#za pomoc?? paste mozna mieszac ze soba elementy wektora i znaki
#czyli dziala jak wektor c() tylko dla mieszanych rodzajow etykiet

pie(ni,labels=etykiety)

#mozna podczepic wektor kolorow
kolory=c("red","yellow","blue","green")
pie(ni,labels=etykiety, col=kolory)

#palety, zestawy kolorystyczne
pie(ni,labels=etykiety, col=rainbow(4))
pie(ni,labels=etykiety, col=rainbow(10))
pie(ni,labels=etykiety, col=heat.colors(4))
pie(ni,labels=etykiety, col=topo.colors(4))

#tym podgladam jakie kolory mam do wyboru
colors()

pie(ni,labels=etykiety, col=kolory, radius=0.5)
?pie

# wykres slupkowy
?barplot

#tu names zamiast labels
barplot(ni,names=klasa)
barplot(pi,names=klasa, col=kolory)

opis=paste(round(pi,2),"%")
barplot(pi,names=klasa, col=kolory, legend=opis)

################## zadanie 2 ###################

dane=read.csv("http://www.ibspan.waw.pl/~pgrzeg/stat_lab/stacje.csv")
#mozna te?? u??y?? read.csv2, on jest "polski" tzn przecinek oddziela cz????ci dziesietne
#a kropki przedzielaj?? dane, read.csv odwrotnie

head(dane)
tail(dane)
names(dane)

class(dane) #m??wi jaki to jest typ obiektu dla R
#cz??sto jak jakie?? funkcje nie dzia??aj??, to dlatego, ??e jest z??y format danych
head(dane) #wy??wietla 6 pierwszych wierszy tych danych
names(dane) #wektor z nazwami kolumn z tej ramki danych
dim(dane) #wymiary ramki - najpierw liczba wierszy, potem liczba kolumn


t <- table(dane)
t <- table(dane[,1])
t <- table(dane$Answers)

p <- prop.table(t)
p
pie(t)
pie(p)

barplot(t)
barplot(p)

################### zadanie 3 ###################

y <- scan(nlines=2)
23.30 24.50 25.30 25.30 24.30 24.80 25.20 24.50 24.60 24.10
24.30 26.10 23.10 25.50 22.60 24.60 24.30 25.40 25.20 26.80 
y

seq(1,10,20)

plot(1:20, y)
plot(y)
plot(y, type="l")
plot(y, type="b", xlab="dzien", ylab="cena")

?plot


################## zadanie 4 ###################
# read.csv(".../butelki.csv")
# http://www.ibspan.waw.pl/~pgrzeg/stat_lab/butelki.csv

dane <- read.csv("http://www.ibspan.waw.pl/~pgrzeg/stat_lab/butelki.csv")
head(dane)

dane$cisnienie <- dane$strength * 0.0068947
head(dane)

dim(dane)

#b)
x <- dane$cisnienie
n <- length(x)
hist(x)
hist(x, prob=T, ylim=c(0,2))
?hist
h <- hist(x)
h

?hist
hist(x, breaks=8)
hist(x, breaks=14)
hist(x, breaks=5)

#breaks = "Sturges"
#k = ceiling(1+ log_2(n))
#"Scott"
#"FD"

hist(x, breaks="Scott")
hist(x, breaks="FD", prob=T, ylim = c(0,2))


#c)
plot(h)
h
plot(h$mids, h$counts, type="l", col="red")
lines(h$mids, h$counts, type="l", col="red")

#d)

#e)
boxplot(x ~ category)
?boxplot

b <- boxplot(x)
?boxplot
b$out

b$stats
summary(x)

var(x)
sd(x)

#f)*
#====== DODATEK =======
#momenty zwykle i centralne
#k-ty moment zwyk??y
#mean to jest pr??bkowy odpowiednik warto??ci oczekiwanej
mk=function(x,k) {mean(x^k)}
mk(x,1)
mean(x)

#k-ty moment centralny
Mk=function(x,k) {mean((x-mean(x))^k)}
Mk(x,2)*n/(n-1) # tu sprawdzam czy sie dobrze liczy, to powinno mi da?? var
var(x)

#wspolczynnik asymetrii (skosnosc)
# a= M3/sqrt(M2^3)
a=Mk(x,3)/(sqrt(Mk(x,2)^3))

#wspolczynnik sp??aszczenia (kurtoza)
# k= M4/(M2^2)-3

k=(Mk(x,4)/(Mk(x,2)^2)) - 3
k

# wspolczynnik zmienno??ci
# wzgl??dna miara rozproszenia
cv = sd(x)/abs(mean(x))
cv
round(cv*100,2)

#g)
#50-ty percentyl
median(x)
quantile(x, 0.5)

quantile(x, 0.25)
quantile(x, 0.75)
quantile(x, 0.9)
summary(x)

#h)
mean(x)

mean(x, trim=0.1)

#po 10% obserwacji skrajnych: najmnijeszych i najwiekszych
# w sumie 2*0.1*100%= 20% obserwacji

#posortowanej niemalej??co pr??bie

n <- length(x)
k=10
a <- sort(x)[(k+1):(n-k)]
mean(a)

mean(x, trim=0.05)
mean(x, trim=0.1)
mean(x, trim=0.15)


################## zadanie 6 ###################
#UWAGA: uzywamy funkcji read.csv2 zamiast read.csv
#kwestia kodowania separatora dziesi??tnego - polski przecinek

# read.csv2(".../samochody.csv")
# http://www.ibspan.waw.pl/~pgrzeg/stat_lab/samochody.csv

dane=read.csv2("http://www.ibspan.waw.pl/~pgrzeg/stat_lab/samochody.csv")
head(dane)

#a)
dane$zp=378.5/(1.609*dane$mpg)
head(dane)

x <- dane$zp
mean(x) 
is.na(x)
sum(is.na(x))

mean(x, na.rm=T)

xx=na.omit(x)
mean(xx)


################## zadanie 7 ###################
sam <- dane
sam$category = cut(sam$zp, c(0, 7, 10, 16))
#x.kod=cut(x,breaks=c(-Inf,7,10,Inf))
levels(sam$category) = c("malo","srednio","duzo")

