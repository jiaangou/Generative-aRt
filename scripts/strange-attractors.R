#Strange attractors
library(dplyr)
library(ggplot2)

strange_attractor <- function(initial, iterations, seed){
  
  #Number of dimensions
  dimensions <- length(initial)
  
  #Initialize matrix
  mat <- matrix(0, ncol = dimensions, nrow = iterations + 1)
  
  #Fill in initial values
  mat[1,] <- initial
  
  #Create a random parameter values
  set.seed(seed)
  par <- runif(10, min = -0.5, max = 1.3)
  
  #print(mat)
  
  #Define function given those parameter values
  for (i in 2:(iterations+1)){
    
    xt <- mat[i-1, 1]
    yt <- mat[i-1, 2]
    #print(xt*par[1])
    
    #update x
    mat[i, 1] <- par[1]*xt + par[2]*yt + par[3]*xt + par[4]*yt^(par[5])
    #update y
    mat[i, 2] <- par[6]*yt + par[7]*xt + -par[8]*xt^par[9] + par[10]*yt
    
    #print(mat[i,])
    #print(xt * par[1])
  }
return(mat)   
}

quartz()

strange_attractor(initial = c(0.2, 0.4), iterations = 10000, seed = 43)


