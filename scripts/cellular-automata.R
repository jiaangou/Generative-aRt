#Cellular automaton
library(dplyr)
library(ggplot2)

#Set up 10x10 grid
df <- expand.grid(x = 1:10, y = 1:10)%>%
  mutate(state = rbinom(100, prob = 0.1, size = 1))
  

ggplot(df, aes(x = x, y = y))+
  geom_tile(aes(fill = factor(state)), col = 'black', size = 1)+
  scale_fill_manual(values = c('white','black'))+
  guides(fill = FALSE)+
  theme_void()


#2D CA 
#Rule 1 - cells next to active cells are activated 
#get active states of t+1
activate_tplus1 <- function(current_grid){
  
  #get boundaries 
  boundary <- sqrt(nrow(current_grid))
  
  #get currently activated cells
  active_cells <- current_grid%>%
    filter(state == 1)
 
  active_x <- active_cells$x
  active_y <- active_cells$y
  
  #activate adjacent cells
  new_active_x <- c(active_x + 1, active_x - 1)
  new_active_y <- c(active_y + 1, active_y - 1)
   
  #put newly activated cells in a dataframe
  new_active_cells <- data.frame(x = new_active_x,
                                 y = new_active_y,
                                 state = 1)%>%
    filter(x <= boundary)%>% #trim out edges
    filter(x >= 1)%>%
    filter(y <= boundary)%>%
    filter(y >= 1)
  
  #create new grid of activated and non-activated cells
  new_grid <- expand.grid(x = 1:boundary, y = 1:boundary)%>%
    left_join(new_active_cells, by = c('x','y'))%>%
    tidyr::replace_na(list(state = 0))%>%
    distinct(x, y, .keep_all = TRUE)
  
  return(new_grid)
}

new <- df

########for loops ########
for (i in 1:5){
  new <- new%>%
    activate_tplus1()
  
  print(
    new%>%
    ggplot(aes(x = x, y = y))+
    geom_tile(aes(fill = factor(state)), col = 'black', size = 1)+
    #geom_raster(aes(fill = factor(state)))+
    scale_fill_manual(values = c('white','black'))+
    guides(fill = FALSE)+
    theme_void()
    )
  i <- i + 1
  
}
########################
########################


new%>%
  activate_tplus1()%>%
  ggplot(aes(x = x, y = y))+
  geom_tile(aes(fill = factor(state)), col = 'black', size = 1)+
  #geom_raster(aes(fill = factor(state)))+
  scale_fill_manual(values = c('white','black'))+
  guides(fill = FALSE)+
  theme_void()




#simulate over time
sim_list <- list()
t <- 1
state_tplus1 <-  expand.grid(x = 1:10, y = 1:10)%>%
  mutate(state = rbinom(100, prob = 0.1, size = 1))
sum_active <- sum(state_tplus1$state)

while(sum_active < 0 | t < 50){
  #activate next step
  state_tplus1 <- state_tplus1%>%
    activate_tplus1()
  
  sim_list[[t]] <- state_tplus1
  
  #update iteration or sum_active
  t <- t + 1
  sum_active <- sum(state_tplus1$state)
  
  #progress message
  message(paste0("time: ", t))
  
}




########
sim_all <- sim_list%>%
  bind_rows(.id = 'time')%>%
  mutate(time = as.numeric(time))


#Animate

library(gganimate)
anim_sim <- sim_all%>%
  filter(time < 20)%>%
  ggplot(aes(x = x, y = y))+
  geom_tile(aes(fill = factor(state)), col = 'black', size = 1)+
  #geom_raster(aes(fill = factor(state)))+
  scale_fill_manual(values = c('white','black'))+
  guides(fill = FALSE)+
  xlim(c(1,10))+ylim(1,10)+
  theme_void()+
  transition_states(time,
  transition_length = 2,
  state_length = 1)  

anim_sim
anim_save(anim_sim, file = 'accidental_ca.gif')

####################################
#1D CA ------
####################################

#Initiation
N <- 2^10 + 1
n_iteration <- 500


random_active <- rbinom((2^8+1)*200, 1, 0.01) #randomly activate 1% of cells
random_mat <- matrix(random_active, nrow = 200, ncol = 2^8+1)

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


out <- one_d_ca(mat = random_mat, error = 0.1)


ggplot_matrix <- function(mat){
  p <- reshape::melt(mat)%>%
    ggplot(aes(x = X2, y = X1))+
    geom_tile(aes(fill = value))+
    theme_void()
  return(p)
}

ggplot_matrix(out)+
  scale_fill_gradient(low = 'white', high = 'black')+
  guides(fill = FALSE)





#Add color variation ----------------------------------
#Multiply each activated cell by a random factor from 1 to 2
length(mat)
new_mat <- mat*(runif(length(mat), min = 1, max = 10))
dim(new_mat)
#quantile(1:1025)

#subset figure for testing
ca_1d <- reshape::melt(new_mat[1:86,482:560])%>%
  ggplot(aes(x = X2, y = X1))+
  geom_tile(aes(fill = value))+
  theme_bw()+
  scale_fill_viridis_c(option = "A")
  #guides(fill = FALSE)


library(rayshader)
plot_gg(ca_1d, width = 5, height = 4, scale = 400, multicore = TRUE,
        zoom = 0.7, theta = 10, phi = 30, windowsize = c(800, 800))

#render_camera(fov = 70, zoom = 0.5, theta = 130, phi = 35)




#Rules
#if both neighbors are present
#cells <- ()

  
