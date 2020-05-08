library(randtoolbox)
source('generateurs.R')

############################################################
##  Partie 1
############################################################

sVN <- 9721
sMT <- 2504
sRA <- 4074
sSM <- 1092

k <- 1000

#QUESTION 2.1
vn <- VonNeumann(k,1,sVN)
mt <- MersenneTwister(k,1,sMT)
ra <- RANDU(k,sRA)
sm <- StandardMinimal(k,sSM)

par(mfrow=c(2,2))
hist(mt[,1],xlab='',main='Mersenne Twister')
hist(vn[,1],xlab='',main='Von Neumann')
hist(ra[,1],xlab='',main='RANDU')
hist(sm[,1],xlab='',main='Standard Minimal')

#QUESTION 2.2
plot(vn[1:(k-1),1],vn[2:k,1],xlab='VN(i)', ylab='VN(i+1)', main='Von Neumann')
plot(mt[1:(k-1),1],mt[2:k,1],xlab='MT(i)', ylab='MT(i+1)', main='Mersenne Twister')
plot(ra[1:(k-1),1],ra[2:k,1],xlab='RA(i)', ylab='RA(i+1)', main='RANDU')
plot(sm[1:(k-1),1],sm[2:k,1],xlab='SM(i)', ylab='SM(i+1)', main='Standard Minimal')

#QUESTION 3
#TESTS
x<-0
x[1]=22
x[2]=21
test <- Frequency(x,5)

#CR
MAX <- 10000
nbVal <- 100
seeds <- sample.int(MAX,nbVal)
pVN <- vector("numeric",nbVal)
pMT <- vector("numeric",nbVal)
pRA <- vector("numeric",nbVal)
pSM <- vector("numeric",nbVal)

#Test sur 100 séquences de 1000 valeurs (avec 100 graines différentes donc) pour chaque générateur.
for(i in 1:nbVal)
{
  pVN[i] <- Frequency(VonNeumann(k,1,seeds[i]),14)  #2^14-1 = 16383. On va jusqu'à 9999.
  pMT[i] <- Frequency(MersenneTwister(k,1,seeds[i]),32) #On va jusqu'à 2^32-1
  pRA[i] <- Frequency(RANDU(k,seeds[i]),31) #On va jusqu'à 2^31-1
  pSM[i] <- Frequency(StandardMinimal(k,seeds[i]),31) #On va jusqu'à 2^31-2
}

par(mfrow=c(2,2))
hist(pMT,xlab='',main='Mersenne Twister')
hist(pVN,xlab='',main='Von Neumann')
hist(pRA,xlab='',main='RANDU')
hist(pSM,xlab='',main='Standard Minimal')

nbVN <- 0
nbMT <- 0
nbRA <- 0
nbSM <- 0
for(i in 1:nbVal)
{
  if(pMT[i]<=0.01){nbMT<-nbMT+1}
  if(pVN[i]<=0.01){nbVN<-nbVN+1}
  if(pRA[i]<=0.01){nbRA<-nbRA+1}
  if(pSM<=0.01){nbSM<-nbSM+1}
}
nbVN<-nbVN/nbVal
nbMT<-nbMT/nbVal
nbRA<-nbRA/nbVal
nbSM<-nbSM/nbVal

nbVN*100
nbMT*100
nbRA*100
nbSM*100

#QUESTION 4
#TESTS
x <- 0
x[1]=19
x[2]=11
test <- Runs(x,5)

#CR
#En utilisant les mêmes séquences que tout à l'heure
for(i in 1:nbVal)
{
  pVN[i] <- Runs(VonNeumann(k,1,seeds[i]),14)  #2^14-1 = 16383. On va jusqu'à 9999.
  pMT[i] <- Runs(MersenneTwister(k,1,seeds[i]),32) #On va jusqu'à 2^32-1
  pRA[i] <- Runs(RANDU(k,seeds[i]),31) #On va jusqu'à 2^31-1
  pSM[i] <- Runs(StandardMinimal(k,seeds[i]),31) #On va jusqu'à 2^31-2
}
par()
par(mfrow=c(2,2))
hist(pMT,xlab='',main='Mersenne Twister')
hist(pVN,breaks=seq(0,1,0.01),xlim=c(0,1),xlab='',main='Von Neumann')
hist(pRA,xlab='',main='RANDU')
hist(pSM,xlab='',main='Standard Minimal')

nbVN <- 0
nbMT <- 0
nbRA <- 0
nbSM <- 0
for(i in 1:nbVal)
{
  if(pMT[i]<=0.01){nbMT<-nbMT+1}
  if(pVN[i]<=0.01){nbVN<-nbVN+1}
  if(pRA[i]<=0.01){nbRA<-nbRA+1}
  if(pSM<=0.01){nbSM<-nbSM+1}
}
nbVN<-nbVN/nbVal
nbMT<-nbMT/nbVal
nbRA<-nbRA/nbVal
nbSM<-nbSM/nbVal

nbVN*100
nbMT*100
nbRA*100
nbSM*100

#QUESTION 5
#En utilisant les mêmes séquences que tout à l'heure
d <- 4
for(i in 1:nbVal)
{
  pVN[i] <- order.test(VonNeumann(k,1,seeds[i])[,1],d,FALSE)$p.value 
  pMT[i] <- order.test(MersenneTwister(k,1,seeds[i])[,1],d,FALSE)$p.value
  pRA[i] <- order.test(RANDU(k,seeds[i])[,1],d,FALSE)$p.value
  pSM[i] <- order.test(StandardMinimal(k,seeds[i])[,1],d,FALSE)$p.value
}

par(mfrow=c(2,2))
hist(pMT,xlab='',main='Mersenne Twister')
hist(pVN,xlab='',main='Von Neumann')
hist(pRA,xlab='',main='RANDU')
hist(pSM,xlab='',main='Standard Minimal')
