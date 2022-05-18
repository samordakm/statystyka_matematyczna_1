#lab 24.05
#lab nr 5
########### ESTYMACJA PRZEDZIA£OWA ###########

#(L,P)
#(L-----parametr-----P)
#(sr -   ,sr +    )



#zadane pstwo - poziom ufnosci
#95% przedial ufnosci
#poziom ufnosci - frakcja pokryc przez przedzialy liczbowe, ktore sa realizacjami tych losowych przedzialow

### Zad 5 ###
#rozklad normalny
#nie znamy parametrow tego rozkaldu

m <- 10000
n <- 10 #licznosc probki, 10 eltow z rozkladu normalnego
alfa <- 0.05
kw <- qt(1-alfa/2, n-1)

#1-alfa to jest nasz poziom ufnsoci

czy.wpada = replicate(m,
                      {
                        x <- rnorm(n) #N(0,1) - czyli WO to mi=0, i dla niej kostruujemy te przedzialy ufnosci
                        sr <- mean(x)
                        odch <- sd(x)
                        #mozemy wyliczyc lewy i prawy kraniec przedzialu ufnosci
                        (sr - kw*odch/sqrt(n) <0 ) & (sr + kw*odch/sqrt(n) > 0) #to sprawdza czy mi=0 wpadlo czy nie wpadlo do przedzialu ufnosci 
                      }
  
)
czy.wpada[1:10]
sum(czy.wpada)/m

m <- 100

#ilustracja graficzna
przedzial = replicate(m,
                      {
                        x <- rnorm(n) #N(0,1) - czyli WO to mi=0, i dla niej kostruujemy te przedzialy ufnosci
                        sr <- mean(x)
                        odch <- sd(x)
                        #mozemy wyliczyc lewy i prawy kraniec przedzialu ufnosci
                        c(sr - kw*odch/sqrt(n), sr + kw*odch/sqrt(n))
                      }
                      
)
lewy <- przedzial[1,]
prawy <- przedzial[2,]

matplot(rbind(lewy, prawy), rbind(1:m, 1:m), type="l", lty=1, xlab="", ylab="")
abline(v=0)


### Zad 6 ###
#50 losowo wybranych podrêcznikóW
#srednia cena = 28.40
#odchylenie std = 4.75
#95%
#dla sredniej ceny podecznika

#rozklad jest normalny

#Model I

n <- 50
sr <- 28.4
sigma <- 4.75
alfa <- 0.05
kw <- qnorm(1-alfa/2)

sr + c(-1,1)*(kw*sigma/sqrt(n))
#znane odchylenie - to to jest najprostszy z mo¿liwych wariantow

### Zad 7 ###

x = scan(nlines=3)
330.0 322.0 345.0 328.6 331.0 342.0
342.4 340.4 329.7 334.0 326.5 325.8
337.5 327.3 322.6 341.0 340.0 333.0 
x
#zakladamy rozkald normalny
#dla wart oczekiwanej i dla odch std

#ten model jest juz zaimplementowany w R
#t.test()

?t.test

t.test(x)

mean(x)
t.test(x)$conf.int


#przedzial ufnosci dla wariancji
alfa <- 0.05
n <- length(x)
#lewy kraniec
l <- (n-1)*var(x)/qchisq(1-alfa/2, n-1)
#prawy kraniec
p <- (n-1)*var(x)/qchisq(alfa/2, n-1)

sd(x)
c(sqrt(l), sqrt(p)) #przedzial ufnosci dla och std


#jest do tego funkcja
#sigma.test()
#install.packages("TeachingDemos")
library(TeachingDemos)
sigma.test(x)
sqrt(sigma.test(x)$conf.int) #przedzial ufnosci dla odchylenai standardowego


### Zadanie 10 ###

data(iris)
head(iris)

x <- iris$Petal.Length[iris$Species=="virginica"]
#a)
t.test(x, conf.level = 0.99)$conf.int
mean(x)

5.761169 - 5.552
5.552 - 5.342831

#b)
sqrt(sigma.test(x)$conf.int)

#co sie stanie z dlugoscia przedzialu, kiedy zwiekszymy poziom ufnosci?
sqrt(sigma.test(x, conf.level=0.99)$conf.int)

### zad 12 ### to samo co wyzej

data(chickwts)
head(chickwts)
table(chickwts$feed)


#wzraz ze wzrostem liczby st swobody rozklad t studenta przypominac rozkald normalny

### zadanie 13 ###

data(faithful)
dim(faithful)

x <- faithful$waiting
mean(x) #estymator punktowy nieznanej wartosci oczekiwanej
t.test(x, conf.level = 0.99)$conf.int #to jest przybli¿ony asymptotyczny przedzial ufnosci

### Zadanie 14 ###
data(Orange)


### Zadanie 8 ###
#nie mamy dostepu do pelnych danych tzn kazdego z 1014 doroslych co oni dokaldnie powiedzieli
##tylko mamy to zsumowane
# 1 - podziela poglad
# 0 - nie podziela pogladu

k <- 578 # liczba sukcesow w probie
n <- 1014

p <- k/n
p #punktowy, probkowy estymator p 

#prop.test() zwracac asyptotyczne przedzialy ufnosci
#tej funkcji mozna uzywac tylko w przypadku duzych prob !!!!

prop.test(k, n, conf.level = 0.95)
?prop.test
prop.test(k, n, conf.level = 0.95, correct=F) #recznie

#binom.test()
binom.test(k,n)
#zwraca postac dokladnego przedzialu ufnosci, ktory nie wykorzystuje przyblizenia rozkaldem normalnym


### zadanie 9 ###

#mala licznosc proby!!!
#w ogole ten nasz model z kartki odpada

binom.test(3,12)
#binom daje dok³adny przedzial ufnosci, a nie asymptotycznie normalny


### zadanie 11 ###
#150 to jest juz wystarczajaco duza probka
binom.test(19, 150,  conf.level=0.96)$conf.int
prop.test(19, 150,  conf.level=0.96)$conf.int


### zadanie 15 ###
library(MASS)
data(Pima.te)
dim(Pima.te) #332 to juz jest duzo