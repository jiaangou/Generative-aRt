#Turing pattern
library(dplyr)
library(ggplot2)
#remotes::install_github("ZenBrayn/r-turing-patterns", dependencies = TRUE)
#install.packages("BiocManager")
#BiocManager::install("EBImage")
library(turingpatterns)


# Size of the grid
grid_x <- 500
grid_y <- 500
# Number of processing iterations
n_itr <- 100

# This might take a while...
tp_grid <- turing_pattern(grid_x = 500, grid_y = 500, n_itr = n_itr, display_intr_imgs = TRUE)

TP <- tp_grid%>%
  reshape2::melt()%>%
  rename(`x` = Var1)%>%
  rename(`y` = Var2)%>%
  ggplot(aes(x = x, y = y, col = value))+
  geom_point()+
  theme_void()


# Save out your work
save_image(tp_grid, "ex1.png")

#DIY turing pattern
#I. Create 2 molecules that diffuse at different rates -----
diffusion_fun <- function(rate = 1, initial_coordinate, steps){
  #Store iterations in a matrix
  
  time_series <- matrix(0, nrow = steps+1, ncol = length(initial_coordinate))
  time_series[1,] <- initial_coordinate #first element is the initial coordinate
  
  for(i in 2:steps+1){
  
  #move or stay depending on rate
  move <- rbinom(n = 1, size = 1, prob = rate)
  
  #If move, then choose new position
  if(move == 1){
    new_coordinate <- sapply(time_series[i-1,], FUN = function(x) x + sample(c(-1,1), size = 1))
  
    time_series[i,] <- new_coordinate}
  else{
    time_series[i,] = time_series[i-1,] #new position = previous position
    } 
  
  }
  return(time_series)
}
  
fast <- lapply(rep(300,300), function(x) diffusion_fun(rate = 1, initial_coordinate = c(0,0), steps = x)%>%
  as.data.frame()%>%
  tibble::rowid_to_column(var = 'time'))%>%
  bind_rows(.id = 'molecule')%>%
  mutate(speed = 'fast')

slow <- lapply(rep(300,300), function(x) diffusion_fun(rate = 0.6, initial_coordinate = c(0,0), steps = x)%>%
                 as.data.frame()%>%
                 tibble::rowid_to_column(var = 'time'))%>%
  bind_rows(.id = 'molecule')%>%
  mutate(speed = 'slow')


ggplot(aes(x = V1, y = V2, col = speed))+
  geom_point()



