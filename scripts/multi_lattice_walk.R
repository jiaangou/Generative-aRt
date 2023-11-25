##################################################
##################################################
step_function <- function(pos, prev_x = NULL, prev_y = NULL){
  
   if(is.null(walked_x)&is.null(walked_y)){
     walked_x <- pos[1]
     walked_y <- pos[2]
   }else{
     walked_x <- c(prev_x, pos[1])
     walked_y <- c(prev_y, pos[2])
   }
  

  #Loop until no more positions can be walked
  while(is.numeric(pos)){
    new_locs <- lapply(direction, function(x)pos+x)%>%
      bind_rows()%>%
      mutate(repeated = paste0(x, y) %in% paste0(walked_x, walked_y))%>%
      filter(repeated == FALSE)
    
  #Sample 1 position 
    if(nrow(new_locs) > 1){
      new_locs <- new_locs%>%
        sample_n(1)
    }
    #Update position
    if(nrow(new_locs) == 1){
      pos <- c(x = new_locs$x, y = new_locs$y)
      
      walked_x <- c(walked_x, pos[1])
      walked_y <- c(walked_y, pos[2])
      
    }else{
      pos <- NA
    }
  }
  
  #Remove walks from previous walkers
  if(!is.null(prev_x)&!is.null(prev_y)){
    walked_x <- walked_x[-1:-length(prev_x)] 
    walked_y <- walked_y[-1:-length(prev_y)]
  }

  return(data.frame(x = walked_x, y = walked_y))
  
}

#Initial conditions
lattice_walk <- function(pos = c(x = 5, y = 5), no.walks){
  
  #no.walks <- 60
  #pos <- c(x = 5, y = 5)
  
  walk_list <- vector(mode = 'list', length = no.walks)
  
  #First walk
  walk_list[[1]] <- step_function(pos = pos)
  
  #Loop the rest
  for(i in 2:no.walks){
    
    #Combine all walked positions ---
    combined_walks <- walk_list%>%
      bind_rows()
    
    #New initial position ---
    pos <- expand.grid(x = min(combined_walks$x):max(combined_walks$x),
                       y = min(combined_walks$y):max(combined_walks$y))%>%
      bind_rows(combined_walks)%>%
      group_by(x, y) %>%
      filter(n() == 1)%>%
      ungroup()%>%
      sample_n(1)
    
    #Take steps with new position
    pos <- c(x = pos$x, y = pos$y)
    
    #Steps: BUG! Need to remove repeated steps
    walk_list[[i]] <- step_function(pos = pos, prev_x = combined_walks$x, prev_y = combined_walks$y) ######
    
  }
  
  
  
  
  
}


####################################################
#PLOTS ============================================
####################################################
col_pal <- wesanderson::wes_palette(name = "BottleRocket1", n = 7)

col_pal2 <- c('#003f5c','#2f4b7c','#665191', '#a05195', '#d45087', '#f95d6a', '#ff7c43', '#ffa600')

lattice_walk(no.walks = 100)%>%
  bind_rows(.id = 'walk_i')%>%
  ggplot(aes(x = x, y= y, group = walk_i))+
  geom_path(size = 1)+
  #scale_color_manual(values = c(col_pal2))+
  theme_void()+
  theme(panel.background = element_rect(fill = salmon_pal[1]))+
  guides(col = "none", size = "none")  




#Plot
  lapply(function(x)x%>%tibble::rowid_to_column(var = 'step_i'))%>%
  lapply(function(x)x%>%mutate(size = rexp(n(), rate = 3)%>%
                                 scales::rescale(to = c(0.5, 5)))%>%
           mutate(fill = rbinom(n(), 1, 0.5)))%>%
  bind_rows(.id = 'walk_i')%>%
  ggplot(aes(x = x, y = y, group = walk_i))+
  #geom_path()+
  #geom_point(aes(size = size, col = walk_i), stroke = 1.5, shape = 22)+
  geom_point(aes(size = size), col = salmon_pal[4], stroke = 1, shape = 22)+
  #scale_color_manual(values = c(salmon_pal[2:4]))+
  #scale_fill_discrete(values = c(salmon_pal[3:4]))+
  theme_void()+
  theme(panel.background = element_rect(fill = salmon_pal[2]))+
  guides(col = "none", size = "none")  
  geom_point(data = pos, aes(x = x, y =y))







