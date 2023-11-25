library(dplyr)
library(ggplot2)

#Chaos----------------
library(deSolve)

Lorenz <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dX <- s * (Y - X)
    dY <- X * (r - Z) - Y
    dZ <- X * Y - b * Z
    list(c(dX, dY, dZ))
  })
}

#initialize
parameters <- c(s = 10, b = 8/3, r = 1.5)
#r <- seq(0, 30, by = 0.05)
state <- c(X = 0, Y = 1, Z = 1)
times <- seq(0, 50, by = 0.01)
#solve

out <- ode(y = state, times = times, func = Lorenz, parms = parameters)

attractor <- out%>%
  as.data.frame()%>%
  ggplot(., aes(x = Y, y = Z))+
  geom_line(alpha = 0.8)+
  theme_classic()

library(gganimate)
attractor + transition_reveal(times, range = c(0,50))


#Fractals--------------------------
library(ggplot2)

max_iter=25
cl=colours()
step=seq(-2,0.8,by=0.005)
points=array(0,dim=c(length(step)^2,3))
t=0

for(a in step)
{
  for(b in step+0.6)
  {
    x=0;y=0;n=0;dist=0
    while(n<max_iter & dist<4)
    {
      n=n+1
      newx=a+x^2-y^2
      newy=b+2*x*y
      dist=newx^2+newy^2
      x=newx;y=newy
    }
    
    if(dist<4)
    { 
      color=24 # black
    }
    else
    {
      color=n*floor(length(cl)/max_iter)
    }
    
    t=t+1
    points[t,]=c(a,b,color)
  }
}

df=as.data.frame(points)    

df
quartz()
ggplot(data=df, aes(V1, V2, col = V3))+ 
  geom_point() 



#Strange attractors -------------
library(purrr)
n <- 5

# Initialization of our data frame
df <- tibble(x = numeric(n+1),
             y = numeric(n+1))

# Convert our data frame into a list
#df <- by_row(df, function(v) list(v)[[1L]], .collate = "list")$.out

# This function computes current location depending of previous one
f <- function(j, k, a, b, c, d) {
  tibble(
    x = sin(a*j$y)+c*cos(a*j$x),
    y = sin(b*j$x)+d*cos(b*j$y)
  )
}

#
> Teasel <- function (t, y, p) {
  + yNew <- A %*% y
  + list (yNew / sum(yNew))
  + }
The model is solved using method “iteration”:
  > out <- ode(func = Teasel, y = c(1, rep(0, 5) ), times = 0:50,
               + parms = 0, method = "iteration")






