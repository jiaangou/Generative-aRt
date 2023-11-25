#Diffusions
library(ggplot2)
library(dplyr)

#set up grid (40x40)
xygrid <- expand.grid(x = 1:40, y = 1:40)

grid_p <- xygrid%>%
  ggplot(aes(x = x, y = y))+
  geom_tile(fill = 'white', col = 'black', size = 1)+
  #geom_point(aes(x = 10, y = 10), size = 5)+
  theme_void()

xygrid%>%
  ggplot(aes(x = x, y = y))+
  geom_tile(fill = 'white', col = 'black')
  theme_void()


#matrix form
matrix(0, nrow = 40, ncol = 40)


#random walk function
random_walk <- function(start, no.steps, bias_walk = FALSE){

  #Choice of walk
  walk_choices <- c(-1, 0, 1)
  prob <- rep(1/3, 3) #equal probability
  
  #Bias walk
  if(bias_walk == TRUE){
    prob <- c(0.2, 0.2, 0.4) #biased prob towards +1
  }
  
  #Random walks  ------
  #for each dimension (equal to the length of coordinate vector, 'start'), a number of steps ('no.steps') are taken 
  #the cumulative sum of these steps + initial position are then taken to get the coordinates after each step
  walks <- sapply(start, FUN = function(x)sample(walk_choices,
                                                 size = no.steps,
                                                 replace = TRUE,
                                                 prob = prob)%>%
                    c(x, .)%>%
                    cumsum)
  

  return(walks)
}


#simulate random walks for multiple individuals all starting from the same position
start <- c(20, 20) #middle of 40x40 grid

#300 steps 300 individuals
multiple_walks <- lapply(rep(300, 300), FUN = function(x)random_walk(start = start, no.steps = x)%>%
         as.data.frame()%>%
         tibble::rowid_to_column(var = 'time'))%>%
  bind_rows(.id = 'individual')


#animate walk on grid --------
#put points on grid
grid_walks_p <- grid_p + 
  geom_point(data = multiple_walks,
             aes(x = V1, y = V2, group = individual),
             position = position_dodge(width = 0.1),
             alpha = 0.7,
             size = 2) +
  ylim(0,40) + xlim(0,40)

#render animation
library(gganimate)
diffusion_anim <- grid_walks_p + transition_time(time) + ggtitle('Diffusion', subtitle = 'Time: {frame}')

#save
anim_save(file = 'diffusion_animation.gif')


#######################
#Biased walk -----------
########################
#Walk
biased_walks <- lapply(rep(300, 300), FUN = function(x)random_walk(start = start,
                                                           no.steps = x,
                                                           bias_walk = TRUE)%>%
                           as.data.frame()%>%
                           tibble::rowid_to_column(var = 'time'))%>%
  bind_rows(.id = 'individual')

#plot
bias_walk_p <- grid_p + 
  geom_point(data = biased_walks,
             aes(x = V1, y = V2, group = individual),
             position = position_dodge(width = 0.1),
             alpha = 0.7,
             size = 2) +
  ylim(0,40) + xlim(0,40)


#render animation
bias_anim <- bias_walk_p + transition_time(time) + ggtitle('Bias walk', subtitle = 'Time: {frame}')

anim_save(bias_anim, file = 'bias_walk.gif')



