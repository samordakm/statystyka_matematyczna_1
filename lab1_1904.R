#zadanie 1
#gestosci, srednia przesuwa wykres prawo lewo
?dnorm

dnorm(c(0,1,4))

curve(dnorm(x),-10,10, col=1)
curve(dnorm(x,1,1),-10,10,col=2, add=T)
curve(dnorm(x,2,1),-10,10,col3=3,add=T)
curve(dnorm(x,-1,1),-10,10,col=4,add=T)
curve(dnorm(x,-2,1),-10,10,col=5,add=T)

#im wieksza wariancja tym sie bardziej rozplaszcza wykres
curve(dnorm(x,0,1),-10,10,col=1)
curve(dnorm(x,0,0.5),-10,10,col=2, add=T)
curve(dnorm(x,0,2),-10,10,col=3, add=T)
legend("topleft", c("N(0,1)","N(0,0.5)","N(0.2)"),fill=c(1,2,3))

#dystrybuanty
#p
curve(pnorm(x,0,1),-10,10,col=1)
curve(pnorm(x,0,0.5),-10,10,col=2, add=T)
curve(pnorm(x,0,2),-10,10,col=5, add=T)
legend("topleft", c("N(0,1)","N(0,0.5)","N(0.2)"),fill=c(1,2,5))
curve(pnorm(x,1,1),-10,10,col=2, add=T)
curve(pnorm(x,2,1),-10,10,col3=3,add=T)
curve(pnorm(x,-1,1),-10,10,col=4,add=T)

?pnorm

#funckje przezycia, dopisanie tego lower,tail=F
#1-F(x)
curve(pnorm(x,0,1, lower.tail=F),-10,10,col=1)
curve(pnorm(x,0,0.5, lower.tail=F),-10,10,col=2, add=T)
curve(pnorm(x,0,2, lower.tail=F),-10,10,col=5, add=T)

#zadanie 2
#sprawdzic regule 3-sigmowa dla r.normalnego N(0,1)
#P(-3*1<=X<=3*1)=b duze 0.99
pnorm(3)-pnorm(-3) #0.9973002

x<-seq(-5,5,0.01)
y<-dnorm(x)
#xlab=NA usuwa opis osi X lub Y
#axes=FALSE
plot(x,y,type="l", main="Regula 3-sigmowa", xlab=NA, ylab=NA)
wx<-c(-3,x[x>=-3 & x<=3],3)
wy<-c(0,y[x>=-3 & x<=3],0)
polygon(wx,wy,col="maroon1")

#zadanie 3
#dla rozkladu normalnego R przez drugi parametr rozumie odchylenie standardowe
dnorm(150,173,6)
curve(dnorm(x,173,6),140,205)
#a) dystrybuanta
pnorm(179,173,6)
#b) 
pnorm(180,173,6)-pnorm(167,173,6)
#c)
1-pnorm(181,173,6)
pnorm(181,173,6,lower.tail = FALSE)

#d) kwantyl 0.6 dla rozkladu N(173,6)
qnorm(0.6,173,6)

#zadanie 4
qnorm(0.9,0,1)
qnorm(0.975,0,1)
#a za jednym zamachem tak sie liczy rozne wartosci kwartyli
qnorm(c(0.9,0.975),0,1)

qnorm(0.9,0,1)
qnorm(0.975,0,2)
qnorm(c(0.9,0.975),0,c(1,2))

?qt

#t-student bierze kwantyl, a drugi parametr to liczba stopni swobody
qt(c(0.95,0.99,0.97),c(10,20,40))
qchisq(c(0.9,0.95),c(4,10))
qf(c(0.95,0.99),c(2,3),c(10,18))

qf(0.025, 4,4)

#zadanie 5
#rozklad gamma
curve(dgamma(x,1,1),0,10,col=3,lty=2)
curve(dexp(x,1),0,10,add=T) #to jest dokladnie ten sam rozklad

curve(dgamma(x,1,1),0,10,lty=2)
curve(dgamma(x,0.5,1),0,10, add=T)
curve(dgamma(x,0.25,1),0,10, add=T)
curve(dgamma(x,2,1),0,10, add=T,col=5)
curve(dgamma(x,1.5,1),0,10, add=T)
curve(dgamma(x,1.25,1),0,10, add=T)
curve(dgamma(x,5,1),0,10, add=T)
curve(dgamma(x,10,1),0,10, add=T)

#drugi parametr dla stalego pierwszego parametru odpowiada za wysokosc gorki
#drugi parametr nazywany jest parametrem skali
curve(dgamma(x,2,2),0,10, add=T,col=2)
curve(dgamma(x,2,3),0,10, add=T,col=2)

#zadanie 6
#rozklad chi-kwdrat
#chi-kwadrat(n)~Gamma(n/2,1/2)
curve(dchisq(x,5),0,50)
curve(dchisq(x,10),0,50,add=T,col=2)
curve(dchisq(x,40),0,50,add=T,col=4)

#x_i N(0,1)
#chi^2 = sum i=1 ^n x_i^2

#dla duzych n: chi-kwadrat(n) ~to z grubsza~ N(n,sqrt(2n))
curve(dnorm(x,100,sqrt(2*100)),0,200,ylim=c(0,0.05))
curve(dchisq(x,100),add=T,col=2)
curve(dnorm(x,120,sqrt(2*120)),0,200,ylim=c(0,0.05))
curve(dchisq(x,120),add=T,col=2)


curve(dnorm(x,40,sqrt(2*40)), 0,200)
curve(dchisq(x,40),0,50,add=T,col=4)


#zadanie 7
#rozkald t-studenta
curve(dnorm(x,0,1),-6,6)
curve(dt(x,1),-6,6,add=T, col=6) #t(1)
curve(dt(x,5),-6,6,add=T,col=2) #t(5)
curve(dt(x,30),-6,6,add=T, col="green") #t(30)
#dla t-studenta duzo szybciej zachodzi ta zbieznosc, "maja te "ciezsze ogony"

#zadanie 8
#f-snedecora - tu zadnej zbieznosci do normalnego nie ma
curve(df(x,10,20),0,6, col=6) 
curve(df(x,10,10),0,6, col=4, add=T) 
curve(df(x,10,5),0,6, col=10, add=T) 
#jezeli pierwszy parametr bedzie wiekszy od dwojki, to te rozklady beda mialy
#taki ksztalt jednej gorki, te maksimum i im wiekszy ten drugi parametr tym wieksza gorka


curve(df(x,2,2),0,6, col=6) 
curve(df(x,3,2),0,6, col=4, add=T) 
curve(df(x,5,2),0,6, col=10, add=T) 

curve(dexp(x),0,4)
curve(df(x,2,1),0,6, col=6, add=T) 
curve(df(x,2,5),0,6, col=4, add=T) 
curve(df(x,2,10),0,6, col=10, add=T) 
curve(df(x,2,20),0,6, col=3, add=T) 

#zadanie 9
#gestosci r. beta: beta(5,2),beta(2,2),beta(1,1),beta(2,5)

curve(dbeta(x,5,2),from=-3,to=4)
curve(dbeta(x,2,2),from=-3,to=4,col="red",add=T)
curve(dbeta(x,1,1),from=-3,to=4,col="blue",add=T)
curve(dbeta(x,2,5),from=-3,to=4,col="green",add=T)


#zadanie 10
0:10
seq(0,10,1)

barplot(dbinom(0:10,10,0.5),names.arg = 0:10,col="green")
?barplot

#P(x=k) dla X o rozkl b(n,p): 
#dbinom(k,n,p) - w k-podajemy nasz nośnik
#n- to normalnie ile razy i p-ostatnie to pstwo
#P(X=k), k=0,...,10 dla X o rozkl b(10,0.5)
n=10
p=0.5
k=0:n
pk =dbinom(k,n,p)

plot(k,pk,type="h") #brzydko
barplot(pk,names=k) #to rysuje takie słupeczki, histogramy jakby

# P(Z=k), k=0,...,50 dla Z o rozkl b(50,0.25):

n=50
p=0.25
k=0:n
pk =dbinom(k,n,p)

barplot(pk,names=k)

### 

plot(k,pk,type="h")

x=seq(0,50,0.01)
m=n*p
sig=sqrt(n*p*(1-p))
y=dnorm(x,m,sig) #wartości f-cji gestosci N(0,1)

lines(x,y,type="l",col="red")

#15:27

#zadanie 11

?dgeom

# dgeom ozn. rozkald geometryczny zdefiniowany tak: P(Y=k)=p*(1-p)^k, k=0,1,...
#X - z tego zadania ma rozklad geometryczny

p=0.1 #bo jedna na 10 osob wchodzi
k=0:40
pk=dgeom(k,prob=p)
plot(k,pk,type="h")

# a) jesli X=nr osoby ktora jako pierwsza weszla do sklepu to
# P(X=k)=P(Y=k-1), dla k=1,2,...

k=0:3
dgeom(k,prob=p)

# b) P(X>11)=1-P(X<=11)=1-F_X(11)=1-F_Y(10)
1-pgeom(10,prob=p)

#zadanie 12
 dhyper
# w populacji o N eltach jest M eltow wyroznionych
# P(X=k) = P(w probce n????-elementowej jest k e.wyroznionych)
# dhyper(k,M,N-M,n)

N=200
M=5
n=10
k=0
dhyper(k,M,N-M,n)

#sprawdzamy choose - to beczka(n po k) - reczne sprawdzenie takie
choose(M,k)*choose(N-M,n-k)/choose(N,n)

#zadanie 13

#rozklad wykladniczy

curve(dexp(x),0,10)
a=0.0001
curve(dexp(x,a),0,50000)

#a
abline(v=1000, col="red")
1-pexp(1000,a) #funkcja przezycia

abline(v=10000, col="blue")
1-pexp(10000,a)

abline(v=30000, col="green")
1-pexp(30000,a)

#b
alpha=0.1
qexp(alpha,a) #kwantyl rzedu alfa

curve(dexp(x,a),0,10000)
abline(v=1000, col="red")

abline(v=qexp(alpha,a), col="yellow")


#zadanie 14
# rozklad poissona 
#P(X=k) dla X o rozkladzie Poissona z parametrem a:
#dpois(k,a)
a=4
k=0:20
pk=dpois(k,a)
plot(k,pk,type="h")




#a) X~exp(a), gdzie a=4
curve(dexp(x,a),0,10)

#b) EX=Sigma_X=1/a
1/a

#c) 30min=1/2 godz
pexp(0.5,a)

#d)
1-pexp(1,a)
dpois(0,a)



#zadanie 15
#Niech X1,Y1,X2,Y3,... beda niezaleznymi zmiennymi losowymi 
#o rozkladzie jednostajnym U([0,1]). Dla funkcji borelowskiej 
#f:[0,1] --> [0,1] definiujemy

#Z_{i}=I_{{f(X_{i})>Y_{i}}}.

#Wowczas, z MPWL, mamy:

#(1/n)suma_i^n (Zi)} -> calka_0^1 f(x)dx  p.n.


#a)  Pole obszaru A to

#calka_{A}dxdy=calka_0^1(calak_0^{x2}dy)dx=calka_0^1 (x^2)dx=1/3. 

# Generujemy probke losowa U1,V1,...,U_{n},V_{n} z rozkladu jednostajnego:
n=10000
u=runif(n)
v=runif(n)

#Zaznaczmy te punkty na obrazku i nalozymy na niego wykres funkcji 
#y=x^2 dla 0<x<1. 

plot(u,v,xlim=c(0,1),ylim=c(0,1),pch='.')
curve(x*x, col="red", type="l", xlim=c(0,1), add=T, lwd=3)

#Zliczamy teraz punkty z naszej probki, ktore znalazly sie pod 
#wykresem funkcji y=x^2

ile.pod=sum(v<u*u)
ile.pod
pole=ile.pod/n
pole

#b)

u=runif(n,min=-1/sqrt(2),max=1/sqrt(2))
v=runif(n)

ile.pod=sum(v>u^2 & v<1-u^2)
ile.pod
pole=sqrt(2)*ile.pod/n
pole


#W R mozliwe jest calkowanie numeryczne, np.
integrate(dnorm, -1.96, 1.96)
integrate(dnorm, -Inf, Inf)

#Calki wystepujace z naszym zadaniu mozna obliczyc nastepujaco:
g =function(x) {x^2}
integrate(g, lower = 0, upper =1)

h =function(x) {1-2*x^2}
integrate(h, lower = -1/sqrt(2), upper =1/sqrt(2))



### zwiazki miedzy rozkladami ###
x<-seq(-5,5,0.01)
curve(dgamma(x,1,3),0,10,lty=2)
curve(dexp(x,3),0,10, add=T, col='red')

curve(dchisq(x,3),0,10,lty=2)
curve(dgamma(x,3/2,1/2),0,10, add=T, col='red')

curve(dbeta(x,1,1),0,10,lty=2)
curve(dunif(x),0,10, add=T, col='red')

curve(dcauchy(x,0,1),0,10,lty=2)
curve(dt(x,1),0,10, add=T, col='red')

curve(dweibull(x,scale =1, shape = 1),0,10,lty=2)
curve(dexp(x,1),0,10, add=T, col='red')
?dweibull
?dexp




