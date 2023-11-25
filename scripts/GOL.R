#Game of life
library(dplyr)
library(ggplot2)
library(PalCreatoR)
library(here)


#Initialiaztion
row <- 20
col <- 20
grid <- expand.grid(x = 1:row, y = 1:col)

#Neighborhood
neighborhood <- function(x, y, df, neighborhood_size = 1, torus = FALSE){

require(dplyr)

x_range <- seq(from = x - neighborhood_size, to = x + neighborhood_size)
y_range <- seq(from = y - neighborhood_size, to = y + neighborhood_size)

#Boundary conditions:
x_max <- max(df$x)
y_max <- max(df$y)

#Toroidal function - correct indices that are out of bounds 
toroidal <- function(max_i, coords){
  
  #indices of values that are out of bounds
  smaller_i <- which(coords<1)  # < 1
  larger_i <- which(coords>max_i)  # > max
  
  #correct those indices
  coords[smaller_i] <- max_i + coords[smaller_i]
  coords[larger_i] <-  coords[larger_i] - max_i
  
  return(coords)
  
}

#If torus, then correct indices that are out of bounds
if(torus == TRUE){
  
  #Correct out of bound indices --------------
  if(any(x_range< 1| x_range>x_max)){
    x_range <- toroidal(max_i = x_max, coords = x_range)
  }
  
  if(any(y_range < 1 | y_range>y_max)){
    y_range <- toroidal(max_i = y_max, coords = y_range)
  }
  
}

#Get neighborhood data
nh <- df%>%
  filter(x %in% x_range)%>%
  filter(y %in%y_range)

return(nh)

}


grid%>%
  filter(x %in% neighborhood(3, 10, df = grid, neighborhood_size = 1)$x)%>%
  filter(y %in% neighborhood(3, 10, df = grid, neighborhood_size = 1)$y )

