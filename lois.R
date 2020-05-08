# Implémentation.s question bonus 1
LoiBinomiale <- function(n,p)
{
  X <- 0
  for(i in 1:n)
  {
    if(runif(1)<p)
      X <- X+1
  }
  return(X)
}

# Implémentation.s question bonus 2
SimulationInversion <- function(k)
{
  U <- runif(k)
  X <- 0
  for(i in 1:k)
  {
    X[i] <- exp(sqrt(U[i])*log(2,exp(1)))-1
  }
  
  return(X);
}

SimulationRejet <-function(k)
{
  # On a U de loi Uniforme [0,1] --> runif()
  # On a Y de loi Uniforme [0,1] --> runif()
  # On a f/cg = ln(1+x)/(1+x)
  
  c <- 2/(log(2,exp(1))^2)
  X <- 0
  
  for(i in 1:k)
  {
    U <- runif(1)
    Y <- runif(1)
    
    while(!(U <= (log(1+Y,exp(1))/(1+Y))))
    {
      U <- runif(1)
      Y <- runif(1)
    }
    
    X[i] <- Y
  }
  
  return(X);
}