### kartkowka ###
data(Orange)
head(Orange)
sqrt(sigma.test(Orange$circumference, conf.level=0.97)$conf.int)

### lab 6 ###

### zadanie 1 ###

# Model I
#rozklad cisnienia jest normalny z odch std 0.07

sigma <- 0.07
alfa <- 0.04 # poziom istotnosci
n <- 20

#test dla wartosci oczekiwanej mu

#ustalenie pary hipotez
#=======================
#H0: mu=1.2
#H1: mu > 1.2
#=======================

x=c(1.36, 1.14, 1.27, 1.15, 1.20, 1.29, 1.27, 1.18, 1.23, 1.36, 1.38, 1.37, 1.30, 1.21, 1.33, 1.28, 1.32, 1.29, 1.33, 1.25) 
mean(x)

mu <- 1.2

#statystyka testowa
Tstat <- (mean(x)-mu)/sigma*sqrt(n)

#zbior krytyczny [z(1-alfa): +Inf)
kw <- qnorm(1-alfa)

#====
# jesli wartosc statystyki testowej T zawiera sie w zbiorze krytycznym, to odrzucamy H_0 i przyjmujemy H_1
# jesli sie nie zawiera w obszarze krytycznym, to pozostajemy przy H_0
#====


#TESTOWANIE
Tstat>kw

#ODPOWIEDZ: Dana partia butelek spe³nia postawione wymaganie jakosciowe.

library(TeachingDemos)
#z.test()

zt <- z.test(x, mu=1.2, stdev = 0.07, alternative = "greater", conf.level = 0.96)
#alternative hypothesis: true mean is greater than 1.2
#p-value = 7.052e-07
p<alfa

?z.test

zt$statistic
#wyznaczanie samemu tej p-wartosci
#p=P(T>t)=1-P(T=t)
#gdzie T to jest zmienna losowa o rozkaldzie N(0,1)

1-pnorm(Tstat)


### Zadanie 2 ###
n <- 7
#normalnosc rozkaldu - zalozona

#Model???
#testy dla wartosci oczekiwanej
#MODEL II - bo nieznane mu, i nieznane sigma

#ustalenie pary hipotez
#=======================
#H0: mu = 150
#H1: mu != 150
#=======================

x = c(142,151,148,151,145,150,141)

mu0=150
alfa <- 0.05

T <- (mean(x)-mu0)*sqrt(n)/sd(x)
kw <- qt(1-alfa/2, n-1)

T < -kw | T > kw
# FALSE
# nie mamy podstaw do odrzucenia H

#Odpowiedz: Na zadanym poziomie istotnosci, nie mamy podstaw aby twierdzic, ze waga netto tej marki 
#kawy nie wynosi faktycznie 150g.

#skorzystamy z gotowego testu:
t.test(x, mu=150)
#p-value = 0.0963 > 0.05


### Zadanie 3 ###

#to NIE JEST duza proba ===> model III odpada
#nie mamy zalozenia o normalnosci rozkladu

#przyjmijmy, ze jest normalnosc!

x= c(2852, 3060, 2631, 2819, 2805, 2835, 2955, 2595, 2690, 2723, 2815, 2914)
qqnorm(x)
qqline(x, col="red")

#test Shapiro - Wilka
shapiro.test(x)
#p-value = 0.9532


#mamy normalnosc ===> MODEL II

#ustalenie pary hipotez
#=======================
#H0: mu = 2900
#H1: mu < 2900
#=======================

testowanie <- t.test(x, mu=2900, alt="less")
#p-value = 0.01807

alfa <- 0.03
testowanie$p.value < alfa
#TRUE
#czyli odrzucmay H0

#ODPOWIEDZ: Na poziomie istotnosci 0.03 mozemy stwierdzic, ze czas sweicenia zarowek jest istotnie krotszy od 2900 godzin.

### Zadanie 12 ###
### Zadanie 15 ###















