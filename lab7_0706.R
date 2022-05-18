#lab 7 

############ testy parametryczne dla dwoch prob ###########

### zadanie 4 / kartka 4 ####

#metoda 1
# X ~ N(mu1, sigma1)
# Y ~ N(mu2, sigma2)

#sigma1 = sigma2


# Model 2
# proby niezalezne
# -------------
# H0: mu1 = mu2
# H1: mu1 < mu2
# -------------

x=c(145,150,153,148,141,152,146,154,139,148)
y=c(152,150,147,155,140,146,158,152,151,143,153)

?t.test
t.test(x,y, alternative = "less", var.equal = TRUE)
#p-value = 0.1779 > 0.05

#DECYZJA: nie ma podstaw do odrzucenia H0

#takie same: zadanie 14, 17

### ZADANIE 16 / kartka 4 ###

# glu - normalny
# X ~ N(mu1 ,sigma1)
# Y ~ N(mu2 ,sigma2)

# -------------
# H0: sigma1 = sigma 2
# H1: sigma1 != sigma2
# -------------

library(MASS)
data(Pima.te)
head(Pima.te)

x <- Pima.te$glu[Pima.te$type=='No']
y <- Pima.te$glu[Pima.te$type=='Yes']

?var.test

var.test(x,y)
# p-value = 1.522e-05

# DECYZJA: odrzucamy H0

# takie same: zadanie 19

### ZADANIE 5 / kartka 4 ###

#staz pracy - ma rozklad normalny

# X - staz pracy pracownikow umyslowych
# Y - staz pracy pracownikow fizycznych

# X ~ N(mu1, simga1)
# Y ~ N(mu2, simga2)

# proby niezalezne

# -------------
# H0: mu1 = mu2
# H1: mu1 > mu2
# -------------


### KROK 1
#testujemy hipoteze o rownosci wariancji

# H0: sigma1 = sigma2
# H1: sigma1 != sigma2

x=c(14,17,7,33,2,24,26,22,12) 
y=c(13,15,3,2,25,4,1,18,6,9,20,11,5,1,7)

var.test(x,y)
# p-value = 0.3557

# p-value > 0.05
# nie mamy podstaw zeby odrzucic H0

### KROK 2
# testowanie testem t

t.test(x,y, alternative = "greater", var.equal = TRUE)


### ZADANIE 20 / kartka 4 ###

#rozklad ocen jest normalny
alfa <- 0.1

# X ~ N(mu1, sigma1)
# Y ~ N(mu2, sigma2)

#MODEL: 4

# H0: mu1 = mu2
# H1: mu1 < mu2

#obserwacje zalezne w parach!!!

?t.test
x = c(3.5, 4.0, 3.7, 4.6, 3.0, 3.9)
y = c(4.2, 3.9, 3.8, 4.5, 3.4, 4.2)

t.test(x,y, paired = TRUE, alternative = "less", conf.level = 0.9)

# p-value = 0.07511 
# 0.1

# ----------------------------------------------------

# testowanie zgodnosci
# majac probke X=(X1, ..., Xn) - rozkaldu F naszej probki nie znamy

# H0 : F = F_0
# H1 : ~ H0

# tylko dla rozkaldow ciaglych
# test Ko³mogorowa-Smirnowa
# probka nie musi byc jakos super liczna
# to nie jest zbyt swietny test, bo ma mala MOC
# i czesto nie odrzuca hipotezy falszywej

?ks.test

x <- runif(100)

#normalny 2,1
ks.test(x, "pnorm", 2,1)
ks.test(x, "punif")

# test chi-kwadrat 
# potrzebuje naprawde duzej probki
# bo jest to test asymptotyczny
# on dziala dla rozkaldow ciaglych i dyskretnych

?chisq.test

#Czy moja moenta jest sumetryczna?
pstwa = c(0.5, 0.5)

#eksperyment jest taki ze rzucam monet¹ tysiac razy

wyniki <- c(522, 478)
chisq.test(wyniki, p=pstwa)

#test kruskala-wallisa
?kruskal.test


################# testy normalnosci ######################

#shapiro.test()

library(goftest)
#cvm.test()
#ad.test()

#########################################################
par(mfrow=c(1,1))

x <- rnorm(100)
qqnorm(x)
qqline(x)

x <- runif(100)
qqnorm(x)
qqline(x)
#light tailed

x <- rcauchy(100)
qqnorm(x)
qqline(x)
#heavy tailed

x <- rexp(100)
qqnorm(x)
qqline(x)
#right skew

x <- rgamma(100,2)
qqnorm(x)
qqline(x)


### ZADANIE 3 / kartka 5###
# test zgodnosci 

# Badania grupy krwi 200 osób da³y nastêpuj¹ce wyniki:
# grupa 0 - 73 osoby
# grupa A - 74 osoby
# gruba B - 34 osoby
# grupa AB - 19 osob

# a) Czy na podstawie tych wynikow mozna przyjac hipoteze o 
# roznomiernym rozkladzie wszystkich grup krwi?

# H0: rownomierny rozklad cechy
# H1: ~H0

krew <- c(73,74,34, 19)
pstwa_prawdziwe <- rep(0.25, 4)

chisq.test(krew, p=pstwa_prawdziwe)

# b) Zweryfikuj hipoteze o tym, ze grupa krwi 0 wystepuje u 36,7% ludzi,
# grupa A - u 37%, grupa B - u 18,7%, a gruba AB u 7,6% ogolu. 

pstwa_wgrupach <- c(0.367, 0.37, 0.187, 0.076)
chisq.test(krew, p=pstwa_wgrupach)


### ZADANIE 10 / kartka 5 ###
# test niezaleznosci chi-kwadrat

ww <- c(80, 115, 55)
bw <- c(95, 70, 35)
ct <- rbind(ww, bw)
chisq.test(ct)

#ODP: wyksztalcenie i dochod nie sa niezalezne

### testy jednorodnosci chi-kwadrat

#do weryfikacji hipotez o jednakowym rozkladzie
#ale dla wielu populacji w przypadku dyskretnym

# prop.test(x, y)
# x <- czestosc pojawien sie
# y <- licznosci grup

#### ZADANIE 5 / kartka 5 ###

x <- c(61, 34, 38, 35)
y <- c(206, 164, 98, 102)
prop.test(x, y)