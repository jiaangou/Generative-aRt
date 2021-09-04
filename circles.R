#Circle filling --------------
library(dplyr)
library(ggplot2)
library(ggforce) #for plotting circles
library(here)
#================================================
#Setup color palette 
#================================================
library(PalCreatoR)
van_pal <- here('vancouver.jpg')%>%
  create_pal(image = ., n = 7)%>%
  .[-4] #remove the white
  

#================================================
#Circle filling function 
#================================================
circle_filling <- function(xy_range, decay_rate  = 10, max_iter){

  
  #Functions -------
  #1. random_xy: generates random xy coordinates from a uniform distribution between the range specified by argument (returns a a vector w/ 2 values)
  random_xy <- function(xy_range){
    xy <- runif(2, min = min(xy_range), max = max(xy_range))
    return(xy)
  }
  #2. Check overlap
  check_overlap <- function(circle1, circle2){
    #X1 - X2
    x_square_dist <- (circle1[1] - circle2[1])^2
    #Y1 - Y2
    y_square_dist <- (circle1[2] - circle2[2])^2
    #sqrt(d^2) = d
    distance <- sqrt(x_square_dist+y_square_dist)
    
    #Check if distance larger than sum of radii
    radii_sum <- sum(circle1[3], circle2[3])
    out <- distance > radii_sum
    
    return(out)
  }
  
  
  #Initiate first circle
  first_circle <- c(random_xy(xy_range), rexp(1, rate = decay_rate)) #first ricle
  circles <- matrix(data = first_circle, nrow = 1, dimnames = list(NULL, c('x', 'y', 'radius')))

  #Iteration: Add new circles until max_iter reached
  iteration <- 1
  while(iteration < max_iter){ #loop until # of circles reached or until max_iterations
    
    #Generate a new circle
    new_circle <- c(random_xy(xy_range),  rexp(1, rate = decay_rate))
    
    #Check if circle overlaps with previous ones
    checks <- apply(circles, 1, function(x)check_overlap(x, new_circle))
    if(sum(checks) == length(checks)){
      circles <- rbind(circles, new_circle) #append new circle into circles
    }
    
    iteration <- iteration + 1
  } 
  
  return(circles)
}



#================================================
#Generate circles 
#================================================
nonoverlap_circle <- circle_filling(xy_range = c(0, 10), decay_rate = 5, max_iter = 3000)

#Total # of circles a
no.circles <- nrow(nonoverlap_circle)

#Assign colors randomly to each circle
colors <- sample(c(van_pal, '#FFFFFF'), size = no.circles, replace = TRUE) # FFFFF for white

#Plot
circle_data <- nonoverlap_circle%>%
  as_tibble()%>%
  mutate(col = factor(colors))

bubbly <- circle_data%>%
  ggplot()+
  geom_circle(aes(x0 = x, y0 = y, r = radius, fill = colors))+
  scale_fill_manual(values = levels(circle_data$col))+
  theme_void()+
  guides(fill = "none")+
  coord_fixed()

ggsave(bubbly, filename = 'bubbly_universe.png', dpi = 360)
