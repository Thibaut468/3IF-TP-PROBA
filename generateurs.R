# Implémentation.s question 1
RANDU <- function(k, graine)
{
  a <- 65539
  b <- 0
  m <- 2^31
  s <- graine
  x <- matrix(NA,k,1)
  for(i in 1:k)
  {
    s <- (a*s+b)%%m
    x[i,1] <- s
  }
  return(x)
}

StandardMinimal <- function(k, graine)
{
  a <- 16807
  b <- 0
  m <- 2^31-1
  s <- graine
  x <- matrix(NA,k,1)
  for(i in 1:k)
  {
    s <- (a*s+b)%%m
    x[i,1] <- s
  }
  return(x)
}

# Implémentation.s question 3
Frequency <- function(x, nb)
{
  S <- 0
  tmp <- 0
  for(i in 1:length(x))
  {
    tmp <- binary(x[i])
    for(j in (32-nb+1):32)
    {
      S <- S+(2*tmp[j]-1)
    }
  }
  S <- abs(S)/sqrt(nb*length(x))
  p <- 2*(1-pnorm(S))
  return(p)
}

# Implémentation.s question 4
Runs <- function(x, nb)
{
  ##PRE-TEST
  PI <- 0
  tmp <- 0
  p <- 0
  for(i in 1:length(x))
  {
    tmp <- binary(x[i])
    for(j in (32-nb+1):32)
    {
      PI <- PI+tmp[j]
    }
  }
  PI <- PI/(nb*length(x))
  
  tau <- 2/sqrt(nb*length(x))
  if(abs(PI-(1/2)) >= tau)
  {
    p <- 0
  } 
  else
  {
    #TEST DES RUNS
    V <- 0
    for(i in 1:length(x))
    {
      tmp <- binary(x[i])
      for(j in (32-nb+1):31) 
      {
        if(tmp[j]!=tmp[j+1]) #Si le bit j est différent du bit j+1
        {
          V <- V+1
        }
      }
      
      #Gestion du dernier bit du nombre en cours avec le premier bit du nombre suivant
      if(i!=length(x)) #On ne traitera pas le dernier bit de la séquence
      {
        tmpNext <- binary(x[i+1])
        if(tmp[32]!=tmpNext[32-nb+1]) #Bit j est différent du bit j+1 situé sur le nombre suivant
        {
          V <- V+1
        }
      }
    }
    
    V <- V+1
    p <- 2*(1-pnorm(abs(V-2*nb*length(x)*PI*(1-PI))/(2*PI*(1-PI)*sqrt(nb*length(x)))))
  }
  return(p)
}

# Implémentation.s déjà donnée.s 
VonNeumann <- function(n, p=1, graine)
{
  x <-  rep(graine,n*p+1)
  for(i in 2:(n*p+1))
  {
    numbers <- strsplit(format(x[i-1]^2,scientific=FALSE),'')[[1]]
    while(length(numbers)>4){ 
        numbers <- numbers[2:(length(numbers)-1)] 
    }
    x[i] <- as.numeric(numbers)%*%(10^seq(length(numbers)-1,0,-1))
  }
  x <- matrix(x[2:(n*p+1)],nrow=n,ncol=p)
  return(x)
}


MersenneTwister <- function(n, p=1, graine)
{
  set.seed(graine,kind='Mersenne-Twister')
  x <- sample.int(2^32-1,n*p)
  x <- matrix(x,nrow=n,ncol=p)
  return(x)
}


binary <- function(x)
{
  if((x<2^31)&(x>=0))
    return( as.integer(rev(intToBits(as.integer(x)))) )
  else{
    if((x<2^32)&(x>0))
      return( c(1,binary(x-2^31)[2:32]) )
    else{
      cat('Erreur dans binary : le nombre etudie n est pas un entier positif en 32 bits.\n')
      return(c())
    }
  }
}