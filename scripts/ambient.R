library(ambient)
library(dplyr)
library(ggplot2)

grid <- long_grid(x = seq(0, 1, length.out = 1000),
                  y = seq(0, 1, length.out = 1000))%>% 
  mutate(noise = gen_worley(x, y, frequency = 5, value = 'distance'))%>%
  mutate(noise2 = noise + gen_worley(x, y, frequency = 10, value = 'distance'))%>%
  mutate(cheker = gen_checkerboard(x, y, frequency = 50))%>%
  mutate(simplex = gen_simplex(x, y, frequency = 30))%>%
  mutate(sphere = gen_spheres(x, y, frequency = 30))
  
  

grid

grid%>%
  plot(noise)

gen_checkerboard()
gen_simplex()
gen_spheres


#Vector fields
grid500 <- long_grid(x = seq(0, 1, length.out = 500),
                  y = seq(0, 1, length.out = 500))


grid500%>%
  mutate(x_angle = cos(x)*pi)%>%
  mutate(y_angle = sin(y)*pi)%>%
  mutate(angle = x_angle + y_angle)%>%
  plot(x_angle)


for(column in grid$x){
  for(row in grid$y){
    scaled_x = column * 0.005
    scaled_y = row * 0.005
  #get our noise value, between 0.0 and 1.0
    noise_val = noise(scaled_x, scaled_y)
  #translate the noise value to an angle (betwen 0 and 2 * PI)
    angle = map(noise_val, 0.0, 1.0, 0.0, PI * 2.0)
    grid[column][row] = angle
  }
}



  geom_spoke(aes(angle = value), arrow = arrow(ends = 'last', length = unit(0.1, 'cm')),
             radius = 0.5)
  

?geom_spoke
