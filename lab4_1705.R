dane=read.csv2("http://www.ibspan.waw.pl/~pgrzeg/stat_lab/samochody.csv")
head(dane)

hist(dane$cena, prob=T)
lines(density(dane$cena, kernel="epanechnikov", bw="SJ"))


### Zbiór zadañ 2 ###
#zad 2 - uzupelniajace

n <- 400 #max wielkosc proby
L <- 200 #ilosc probek

M <- matrix(runif(n*L), ncol=L)
#kolumna to jest pojedyncza proba

k <- 20
#wybieram probke k elementowa
#wybieram k wierszy z macierzy M

#M[1:k, ]

#colSums()
sumaU <- colSums(M[1:k, ])

#U(0,1)
#wartosc oczekiwana 1/2 ===> k*1/2
#wariancja 1/12 ====> k*1/12

z <- (sumaU - k/2)/(sqrt(k/12))

hist(z, prob=T, breaks=40)
x <- seq(-4,4,0.01) 
curve(dnorm(x),add=T)

k<-200


### Zestaw 3 ###

#zad 1
#lemat Glivienki-Cantelego

#X_1,...,X_n probka prosta, z rozkladu o dytrybuancie F, wtedy:
#P(lim n-> \infty sup t \in R |\hat{F}(t)-F(t)|=0)=1

#ecdf()

x=c(2,2,4,3,1,2,1,5,2,3)
table(x)
prop.table(table(x))
plot(ecdf(x))
Fn <- ecdf(x)
Fn(3.5)


x20=rnorm(20)
x20
table(x20)
plot(ecdf(x20))

#porownanie dystrybuanty empirycznej z dystrybuanta teoretyczna
#pnorm

curve(pnorm(x), -4,4, lty=2, main="n=20")
plot(ecdf(x20), add=T)

x50=rnorm(50)
curve(pnorm(x), -4,4, lty=2, main="n=20")
plot(ecdf(x50), add=T)

x100=rnorm(100)
curve(pnorm(x), -4,4, lty=2, main="n=20")
plot(ecdf(x100), add=T)

x200=rnorm(200)
curve(pnorm(x), -4,4, lty=2, main="n=20")
plot(ecdf(x200), add=T)

x300=rnorm(300)
curve(pnorm(x), -4,4, lty=2, main="n=20")
plot(ecdf(x300), add=T)

Fn <-ecdf(x300)
Fn(0)
pnorm(0)

Fn(c(-2,0,2))
pnorm(c(-2,0,2))


######################## Estymacja punktowa ########################

# zad 2.
?rcauchy

n <- 500
x <- rcauchy(n)

#a) estymacja parametru polozenia a=0
#dla kazdej naszej probki zawieracaej n poczatkowych elementow wysciowej 
#x_i = (y_1, ..., Y_n)
#chcemy wyznaczyc srednia oraz mediane

#rozklad cauchyego nie ma momentow

sr <- numeric()
med <- numeric()
for(i in 1:n){
  sr[i]=mean(x[1:i])
  med[i]=median(x[1:i])
}

plot(sr, ylim=c(-1,1))
points(med, col="red", add=T)
abline(h=0, col="green")

#b) estymacja parametry rozproszenia b=1

s <- numeric()
r <- numeric()

for(i in 2:n){
  s[i-1] <- sd(x[1:i])
  r[i-1] <- IQR(x[1:i])/2
}
plot(s, ylim=c(0,2))
points(r, col="red", add=T)
abline(h=1, col="green")

#zadanie 3
#a)
x <- rnorm(n)
sr <- numeric()
med <- numeric()
for(i in 1:n){
  sr[i]=mean(x[1:i])
  med[i]=median(x[1:i])
}

plot(sr, ylim=c(-0.5,0.5))
points(med, col="red", add=T)
abline(h=0, col="green")

#b) estymacja parametry rozproszenia b=1

s <- numeric()
r <- numeric()

for(i in 2:n){
  s[i-1] <- sd(x[1:i])
  r[i-1] <- IQR(x[1:i])/2
}
plot(s, ylim=c(0,2))
points(r, col="red", add=T)
abline(h=1, col="green")

# zadanie 4
#U(0, theta)

#EMM=2*srednia
#theta/2

#ENW=Xn:n
#max()

#przypominajka
#MSE = b^2 + Var
#mean((ENW-teta)^2)

#E3
#E3 = (n+1)*max()/n

#----0--x----x------x----------x-----x---x--max(z probki)-----theta-------(n+1)/n*ENW---------


#wygenerowac 10000 probek 20-elementowych

m <- 10000
n <- 20

?replicate
#replicate(ile razy, {.. co zrobic ...})

#losujemy 3 niezalezne proby z N(0,1)
#o licznosciach 5
# dla kazdej wyznaczyc srendia

odp <- replicate(3,{
          x=rnorm(5)
          c(mean(x), sd(x))})
odp[1,] #srednie
odp[2,] #odchylenia

estymatory <- replicate(m,
                        {
                          x=runif(n)
                          c(2*mean(x), max(x), (n+1)*max(x)/n)
                        }
)

EMM=estymatory[1,]
ENW=estymatory[2,]
E3=estymatory[3,]

#obciazneia
#teoretyczne obciazenia
#b(EMM)=0

theta=1

b1 <- mean(EMM-theta)
b2 <- mean(ENW-theta)
b3 <- mean(E3-theta)

#bledy sredniokwadratowe
mse1 <- mean((EMM-theta)^2)
mse2 <- mean((ENW-theta)^2)
mse3 <- mean((E3-theta)^2) #ma najmniejszy MSE


MSE11= theta^2/(3*n)
MSE22=2*(theta^2)/((n+1)*(n+2))


#MSE=b^2+Var
v1 <- var(EMM)
v2 <- var(ENW)
v3 <- var(E3)

b1^2+v1
mse1

b2^2+v2
mse2

b3^3+v3
mse3
