#Lattice walk --------
lattice <- expand.grid(x = 1:10, y = 1:10)

lattice%>%
  ggplot(aes(x = x, y =y))+
  geom_point(size = 5)+
  theme_void()


p1 <- c(1, 3)
pos

lapply(direction, function(x)pos+x)%>%
  bind_rows()


#Initiaion
pos <- c(x = 5, y = 5)

#Function: lattice walk function for a single walker
lattice_walk <- function(pos, no.walks = 1){
  
  #pos <- c(x = pos[1], y = pos[2])
  
  #Create a list to store walks
  walk_list <- vector(mode = 'list', length = no.walks)

  #Walker can walk to the 4 adjacent cells but only if it hasn't been walked
  direction <- list(right = c(0, 1),
                    left = c(0, -1),
                    top = c(1, 0),
                    bottom = c(-1, 0))
  
  #Initiate list
  for(i in 1:no.walks){
    
    #Iterate until no more walks
    while(is.numeric(pos)){ #while there are positions 
      
      #Compute all possible new locations and subset those that have no been walked before
      new_locs <- lapply(direction, function(x)pos+x)%>%
        bind_rows()%>%
        mutate(repeated = paste0(x,y) %in%paste0(walked_x, walked_y))%>%
        filter(repeated == FALSE)
      
      
      #Sample 1 of possible locations (if there are more than 1)  
      if(nrow(new_locs) > 1){
        new_locs <- new_locs%>%
          sample_n(1)
      }
      
      #Update position
      if(nrow(new_locs) == 1){
        pos <- c(x = new_locs$x, y = new_locs$y)
      }else{
        pos <- NA
      }
      
      #Update walked positions
      walked_x <- c(walked_x, pos[1])
      walked_y <- c(walked_y, pos[2])
      
      
      #Update list
      walk_list[[i]] <- data.frame(x = walked_x, y = walked_y)
      
      
      
    }  
    
    
    
  }

  return(walk_list)
}



sampled_grid <- expand.grid(x = min(walk_1$x):max(walk_1$x),
            y = min(walk_1$y):max(walk_1$y))

new_initial <- sampled_grid%>%
  bind_rows(walk_1%>%select(x,y))%>%
  distinct()%>%
  sample_n(1)




#

walk_1 <- lattice_walk(pos = c(x=5,y=5))%>%
  tidyr::drop_na()%>%
  tibble::rowid_to_column(var = 'time')


walk_1%>%
  ggplot(aes(x = x, y = y))+
  geom_path()


walk_2 <- lattice_walk(pos = c(x = 8, y = -5), walked = walk_1)%>%
  tidyr::drop_na()%>%
  tibble::rowid_to_column(var = 'time')


list(walk1 = walk_1, walk2 = walk_2)%>%
  lapply(function(x)x%>%mutate(size = rexp(n(), rate = 3)%>%
                                 scales::rescale(to = c(0.5, 5)))%>%
           mutate(fill = rbinom(n(), 1, 0.5)))%>%
  bind_rows(.id = 'walk')%>%
  ggplot(aes(x = x, y = y, group = walk))+
  #geom_path(aes(col = walk), size = 2)+
  geom_point(aes(size = size, col = walk), stroke = 1.5, shape = 22)+
  scale_color_manual(values = c(salmon_pal[3:4]))+
  #scale_fill_discrete(values = c(salmon_pal[3:4]))+
  theme_void()+
  theme(panel.background = element_rect(fill = salmon_pal[2]))+
  guides(col = "none", size = "none")


#lat_walks <- lattice_walk(pos = c(x=5,y=5))

#Plot
salmon_pal <- c("#E7F2F8", "#74BDCB", "#FFA384", "#EFE7BC")
#
#
lattice_walk(pos = c(x=5,y=5))%>%
  tidyr::drop_na()%>%
  tibble::rowid_to_column(var = 'time')%>%
  mutate(size = rexp(n(), rate = 3)%>%scales::rescale(to = c(0.5, 5)))%>%
  ggplot(aes(x = x, y =y))+
  geom_point(aes(size = size), shape = 22)+
  scale_size(range = c(1, 10))+
  #geom_path()+
  #geom_tile(fill = 'transparent', col = 'black', size = 1)+
  #geom_point(shape = 22, size = 3, fill = salmon_pal[3])+
  theme_void()+
  theme(panel.background = element_rect(fill = salmon_pal[4]))


