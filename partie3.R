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

#Seconde fonction avec un pas dynamique
#8 clients par heure en moyenne
lambda <- 8/60
#15 clients par heure partent en moyenne
mu <- 15/60
#12 heures de fonctionnement 
D <- 720

list_fileMM1 <- FileMM1(lambda,mu,D)
list_evolDyna <- EvolutionDynamique(list_fileMM1[[1]],list_fileMM1[[2]],D)

#Affichage avec une fonction en escalier
par(mfrow=c(1,1))
plot(list_evolDyna[[1]],xlab='Temps',ylab='Nombre de clients dans le système - App1',list_evolDyna[[2]],type='s')

#14 clients par heure en moyenne
lambda <- 14/60
#15 clients par heure partent en moyenne
mu <- 15/60
#12 heures de fonctionnement 
D <- 720

list_fileMM1 <- FileMM1(lambda,mu,D)
list_evolDyna <- EvolutionDynamique(list_fileMM1[[1]],list_fileMM1[[2]],D)

#Affichage avec une fonction en escalier
par(mfrow=c(1,1))
plot(list_evolDyna[[1]],xlab='Temps',ylab='Nombre de clients dans le système - App2',list_evolDyna[[2]],type='s')

#15 clients par heure en moyenne
lambda <- 15/60
#15 clients par heure partent en moyenne
mu <- 15/60
#12 heures de fonctionnement 
D <- 720

list_fileMM1 <- FileMM1(lambda,mu,D)
list_evolDyna <- EvolutionDynamique(list_fileMM1[[1]],list_fileMM1[[2]],D)

#Affichage avec une fonction en escalier
par(mfrow=c(1,1))
plot(list_evolDyna[[1]],xlab='Temps',ylab='Nombre de clients dans le système - App3',list_evolDyna[[2]],type='s')

#20 clients par heure en moyenne
lambda <- 20/60
#15 clients par heure partent en moyenne
mu <- 15/60
#12 heures de fonctionnement 
D <- 720

list_fileMM1 <- FileMM1(lambda,mu,D)
list_evolDyna <- EvolutionDynamique(list_fileMM1[[1]],list_fileMM1[[2]],D)

#Affichage avec une fonction en escalier
par(mfrow=c(1,1))
plot(list_evolDyna[[1]],xlab='Temps',ylab='Nombre de clients dans le système - App4',list_evolDyna[[2]],type='s')

#Question 8
#Nombre moyen d'arrivé dans un intervalle de temps t : lambda*t
#Nombre moyen de départ dans un intervalle de temps t : mu*t
#On a donc le rapport alpha = lambda/mu == intensité du trafic sur un serveur (en Erlang).
#Alpha > 1 : plus d'arrivée que de départ --> régime divergent en probabilité 
#Alpha < 1 : plus de départ que d'arrivée --> régiment convergent en probabilité
#Alpha = 1 : Pdv mathématique : instable car non convergent ni divergent, parfois l'un parfois l'autre, on ne peut pas réellement conclure
# Chaine de Markov en continue dans le parcours de la file d'attente

#8 clients par heure en moyenne
lambda <- 8/60
#15 clients par heure partent en moyenne
mu <- 15/60
#12 heures de fonctionnement 
D <- 720

list_fileMM1 <- FileMM1(lambda,mu,D)
list_evolDyna <- EvolutionDynamique(list_fileMM1[[1]],list_fileMM1[[2]],D)

#Affichage avec une fonction en escalier
par(mfrow=c(1,1))
plot(list_evolDyna[[1]],xlab='Temps',ylab='Nombre de clients dans le système - App4',list_evolDyna[[2]],type='s')

nbMoyen <- NombreClientsMoyens(list_evolDyna[[1]],list_evolDyna[[2]],D)

tpsMoyen <- TempsAttenteMoyen(list_fileMM1[[1]],list_fileMM1[[2]])

#E(N)=espérance du nbre de client = alpha/(1-alpha)
alpha <- lambda/mu
theorieNbMoyen <- alpha/(1-alpha)

#E(W)=temps d'attente moyen = E[N]/lambda
theorieTpsMoyen <- theorieNbMoyen/lambda
