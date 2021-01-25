#load libraries
library(dplyr)
library(ggplot2)
#devtools::install_github("GenChangHSU/PalCreatoR")
library(PalCreatoR)
library(gganimate)


#Create a color palette 
color_cappa <- create_pal(image = 'cappa.jpg', n = 5)
color_sunrise <- create_pal(image = 'sunrise.jpg', n = 4)

#Random walk function
random_walk_art <- function(range = 100, no.walks = 10, no.steps = 20, step_size_distribution = 'uniform', colors){
  #function to generate random n steps by step size 
  rand_n_steps <- function(start, n, step_size = 1){
    
    step_options <- c(-step_size, 0, step_size)
    
    steps <- sapply(start,
                    FUN = function(x)sample(x = step_options, #3 possible steps that can be taken
                                            size = n, #Number of steps to take
                                            replace = TRUE)) 
    
    #Take the cumulative sum to get positions after every step
    if(is.null(dim(start))){ #if dimension = 1
      position <- c(start, steps)%>%
        cumsum
    }
    else{ #if dimension > 1
      position <- rbind(start, steps)%>%
        cumsum
      
    }
    
    return(position)
    
  }

    
  walks <- list() #list to store each walk
  for(i in 1:no.walks){
    walks[[i]] <- data.frame(x = rnorm(1, mean = 0, sd = range),
                             y = rnorm(1, mean = 0, sd = range),
                             z = rnorm(1, mean = 0, sd = range))
  }
  
  
  if(step_size_distribution == 'uniform'){
   
    positions <- lapply(walks, FUN = function(x)rand_n_steps(start = x,
                                                             n = no.steps,
                                                             step_size = runif(1, min = 0.5, max = 1)))%>%
      lapply(FUN = function(x)tibble::rowid_to_column(x,var = 'step_ID'))
    
  }else if(step_size_distribution == 'exponential'){
    positions <- lapply(walks, FUN = function(x)rand_n_steps(start = x,
                                                             n = no.steps,
                                                             step_size = rexp(1, rate = 0.5)))%>%
      lapply(FUN = function(x)tibble::rowid_to_column(x,var = 'step_ID'))
    
  }else{
    stop("Incorrect distribution")
  }

  #randomly assign a color from palette to each walk
  walk_color <- sample(colors, size = no.walks, replace = TRUE)%>%
    rep(each = no.steps + 1) #include starting position
  
  
  #combine walks into a single dataframe
  walks_positions <- positions%>%
    bind_rows(.id = 'walk')%>%
    mutate(color = walk_color)

  
  return(walks_positions)

}


# Static Cappadocia plot  -------------------------------------------------------------
set.seed(123)
p1 <- random_walk_art(range = 200,
                no.walks = 800, no.steps = 100,
                step_size_distribution = 'exponential',
                colors = color_cappa)%>%
  ggplot(aes(x = x , y = y, group = walk, col = color)) +
  scale_color_identity(guide = "legend") +
  geom_path(alpha = 0.7, size = 1) +
  guides(color = FALSE)+
  theme_void()+
  ggtitle('800 walks & 100 steps')+
  theme(text = element_text(family="Gill Sans", size = 20))


# Animated Cappadocia plot  -----------------------------------------------------------
anim1 <- p1 + transition_reveal(along = step_ID) 


# Static sunrise plot  -------------------------------------------------------------
set.seed(13)
p2 <- random_walk_art(range = 200,
                     no.walks = 100, no.steps = 800,
                     step_size_distribution = 'exponential',
                     colors = color_sunrise)%>%
  ggplot(aes(x = x , y = y, group = walk, col = color)) +
  scale_color_identity(guide = "legend") +
  geom_path(alpha = 0.7, size = 1) +
  guides(color = FALSE)+
  theme_void()+
  ggtitle('100 walks & 800 steps')+
  theme(text = element_text(family="Gill Sans", size = 20))


# Animated sunrise plot  -----------------------------------------------------------
anim2 <- p2 + transition_reveal(along = step_ID) 


# Compose images ----------------------------------------------------------
library(patchwork)
library(magick)

#static
static <- p1+p2
ggsave(static, filename = "random-walk-static.jpg", width = 480, height = 240, units = 'mm')

#animations: save gifs then combine with magik
a1_gif <- animate(anim1, width = 240, height = 240)
a2_gif <- animate(anim2, width = 240, height = 240)

new_gif <- image_append(c(a1_gif[1], a2_gif[1]))
for(i in 2:100){
  combined <- image_append(c(a1_gif[i], a2_gif[i]))
  new_gif <- c(new_gif, combined)
}

#save
#anim_save(new_gif, file='random-walks.gif')



#
#magick::image_read('random-walk-static.jpg')
