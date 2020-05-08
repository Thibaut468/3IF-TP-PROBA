library(randtoolbox)
library(microbenchmark)
source('lois.R')

############################################################
##  Partie 2
############################################################

#Question Bonus 1
#Loi Binomiale B(n,p)
N <- 1000
n <- 50
p <- 0.5
x <- 0

for(i in 1:N)
{
  x[i] <- LoiBinomiale(n,p)
}

#Loi Normale(np,np(1-p))
y <- 0
plot(table(x),xlim=c(0,n),main='Densité d\'une loi Binomiale(n,p)')
y <- dnorm(0:n,n*p,n*p*(1-p))
plot(y,main='Densité d\'une loi Normale N(np,np(1-p))')

#Question Bonus 2

# Benchmark sur 100 appels
k <- 3000
microbenchmark(times=100,SimulationInversion(k),SimulationRejet(k))

# Vérification de la distribution des valeurs simulés
inversion <- SimulationInversion(k)
rejet <- SimulationRejet(k)
x <- seq(0,1,0.01)

par(mfrow=c(1,3))
hist(inversion,xlab='',main='Simulation par Inversion d\'une densité f')
hist(rejet,xlab='',main='Simulation par Rejet d\'une densité f')
plot(x,(2/(log(2)^2))*(log(1+x)/(1+x)),xlab='',main='Distribution théorique')
