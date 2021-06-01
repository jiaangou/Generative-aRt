####################################
#1D CA ------
####################################
library(dplyr)
library(ggplot2)
library(PalCreatoR)
library(here)

#Initiation -------
N <- 2^10 + 1
n_iteration <- 300

random_active <- rbinom(N*n_iteration, 1, 0.01) #randomly activate 1% of cells
random_mat <- matrix(random_active, nrow = n_iteration, ncol = N)

#General 1-D CA function -----
one_d_ca <- function(N, iterations, mat, error = 0){
  
  #If matrix is no supplied, create one
  if(missing(mat)){
    #matrix
    mat <- matrix(0, nrow = iterations, ncol = N)
    
    #set initial condition
    mat[1,N%/%2] <- 1
    
  }
  if(missing(N) == missing(iterations)){
    iterations <- nrow(mat)
    N <- ncol(mat)
  }
  
  
  #Iterate
  for (i in 2:iterations){
    
    for (j in 2:(N-1)){ #dont include the boundary cases
      
      #Incorporate error
      step <- rbinom(1,1,1-error)
      
      #implement rule
      after <- mat[i-step, j + step]
      before <- mat[i-step, j - step]
      
      total <- after + before
      
      if(total == 1){
        mat[i, j] <- 1
      }
    }
  }
  return(mat)
}



#Run function -----
out <- one_d_ca(mat = random_mat, error = 0.1)


#General ggplot function ----
ggplot_matrix <- function(mat){
  p <- reshape::melt(mat)%>%
    ggplot(aes(x = X2, y = X1))+
    geom_tile(aes(fill = value))+
    theme_void()
  return(p)
}


#Plot results ----
ca_p <- ggplot_matrix(out)+
  guides(fill = FALSE)


#Coloring -------
van_pal <- here('vancouver.jpg')%>%
  create_pal(image = ., n = 6)

#Final product
ca_p+
  scale_fill_gradient2(low = van_pal[1], mid = van_pal[4], high = van_pal[3])
ggsave(file = '1D_CA.jpg', width = 6, height = 4, units = 'in', dpi = 360)
