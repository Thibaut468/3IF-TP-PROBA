library(randtoolbox)
source('file.R')

############################################################
##  Partie 3
############################################################
# T le temps écoulé entre deux arrivées de clients -> loi exp(lambda)
# D durée de réponse du serveur --> loi exp(mu)
# Sont deux durée indépendantes l'une de l'autre
# Notons M (Markovien) la loi exp, on a un modèle M/M/1 (avec un serveur)
# Ainsi, temps moyen 1/lambda et 1/mu (espérance d'une loi exponentielle)

#Question 6
#8 clients par heure en moyenne
lambda <- 8/60
#15 clients par heure partent en moyenne
mu <- 15/60
#12 heures de fonctionnement 
D <- 720
  
list_fileMM1 <- FileMM1(lambda,mu,D)
arr <- list_fileMM1[[1]]
dep <- list_fileMM1[[2]]

#Question 7
#Première fonction avec un pas de temps régulier
evolSimple <- EvolutionPasDeTempsConstant(arr,dep,D)
#Génération du pas de temps
x<-0
for(i in 1:720){x[i]=i}
#Affichage avec une fonction en escalier
par(mfrow=c(1,1))
plot(x,evolSimple,type='s')

#Question 8

