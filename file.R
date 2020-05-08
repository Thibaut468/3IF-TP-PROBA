# Implémentation.s question 6

FileMM1 <- function(lambda, mu, D) 
{
  #lambda --> param de la loi des arrivées
  #mu --> param de la loi des départ
  #D --> temps d'observation
  arr <- 0
  index_arr <- 1
  dep <- 0
  index_dep <- 1
  
  #Les durées sont en minutes (car les paramètres en min-1)
  temps_depuis_debut <- 0
  
  #Arrivées
  while(D > temps_depuis_debut)
  {
    temps_depuis_debut <- temps_depuis_debut + rexp(1,lambda)
    if(temps_depuis_debut<=D)
    {
      arr[index_arr] <- temps_depuis_debut
      index_arr <- index_arr+1
    }
  }
  
  #Départs
  #Génération du départ de la première personne qui est traité directement
  dep[1] <- arr[1] + rexp(1,mu)
 
  #Génération des temps de départ suivants
  for(i in 2:length(arr))
  {
    #Personne précédente déjà partie, on traite de suite
    if(arr[i]>dep[i-1])
    {
      tmp <- arr[i] + rexp(1,mu)
      if(tmp>D){break}
      dep[i] <- tmp
    }
    else #Personne précédente n'est pas encore partie
    {
      tmp <- dep[i-1] + rexp(1,mu)
      if(tmp>D){break}
      dep[i] <- tmp
    }
  }
  
  #Création de la liste et retour
  ret_list <- list(arr,dep)
  return(ret_list)
}

# Implémentation.s question 7
EvolutionPasDeTempsConstant <- function(arr,dep,D) #nombre de personne dans la file en fonction du temps
{
  nbPersonne=0
  for(temps in 1:D)
  {
    nbArrive=0
    nbParti=0
    for(a in 1:length(arr)) #on parcours pour chaque minute le tableau d'arrivé, on s'arrete si
      # l'heure d'arrivé est supérieure à la minute actuellement regardée
    {
      if(arr[a]>temps){break}
      nbArrive=nbArrive+1
    }
    
    for(a in 1:length(dep)) #idem pour les départs
    {
      if(dep[a]>temps){break}
      nbParti=nbParti+1
    }
    nbPersonne[temps]=nbArrive-nbParti
  }
  return(nbPersonne)
}