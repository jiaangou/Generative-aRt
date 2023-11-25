library(ggplot2)
library(ggquiver)
library(dplyr)

# Set up a grid of points
x <- seq(-10, 10, by = 1)
y <- seq(-10, 10, by = 1)
grid <- expand.grid(x = x, y = y)

# Define a function to calculate vector values at each point
calculate_flow <- function(x, y) {
  u <- -y  # Example flow in x-direction
  v <- x   # Example flow in y-direction
  return(data.frame(x = x, y = y, u = u, v = v))
}

# Calculate flow at each point in the grid
flow_data <- calculate_flow(grid$x, grid$y)

# Create a flow field plot using ggplot2
ggplot(flow_data, aes(x, y, u = atan2(v, u), v = sqrt(u^2 + v^2))) +
  geom_quiver() +
  xlim(c(-10, 10)) +
  ylim(c(-10, 10)) +
  theme_minimal() +
  ggtitle("Flow Field")


#Fibonacci
fib <- function(n){
  phi <- (1 + sqrt(5))/2
  out <- (1 / sqrt(5))*phi^n
  return(out)
}

data.frame(n = 0:5)%>%
  mutate(y = fib(n))%>%
  ggplot(aes(x = y, y = 0))+
  geom_point()




#Fibonacci curve
fib_curve <- function(x, n){
  fn <- fib(n)
  out <- prod(fn-x)
  return(out)
}

#Sequence
s <- seq(from = 0, to = 10, by = 5)

#variable
x <- seq(from = 0, to = 5, by = 0.001)

data.frame(x = x)%>%
  mutate()

polynomial <- function(x, roots){
  
  sums <- sapply(roots, function(r)r-x)
  out  <- apply(sums, 1, prod)
  
  return(out)
  
}

r <- seq(from = 0, to =3, by = 0.)
power_a <- 3

roots <- data.frame(roots = 2^(r*power_a))%>%
  mutate(y = 0)

plot(roots$roots ~ r)

data.frame(x = seq(from = 0, to = 5, by = 0.001))%>%
  mutate(y = polynomial(x = x, roots = roots$roots))%>%
  ggplot(aes(x = x, y = y))+
  geom_line()
  #geom_point(data = roots, aes(x = roots, y = y))



#Matrix eigenvalues -------

# complex function 
complex_function <- function(x){
  x*(5+8i) - 3 - 5i
}



#i. random samples (x) or size N
N <- 100
rand_x <- matrix(runif(2*N, min = -5, max = 5), ncol = 2)
complex <- apply(rand_x, 2, function(x)complex_function(x))


#ii. matrix generation
D <- 4
gen_matrix <- function(d = 4, A, B, p = 0.8){
  
  #Set up matrix
  vals <- rbinom(d^2, size = 1, prob = p)
  mat <- matrix(vals, nrow = d, ncol = d)
  
  #Sample 2 random locations on the matrix
  rand_i <- sample(1:d^2, 2)
  
  #Insert complex function into matrix element 
  mat[rand_i[1]] <- A
  mat[rand_i[2]] <- B
  
  return(mat)
}
M <- lapply(1:nrow(complex), function(x)gen_matrix(d = D, A = complex[x, 1], B = complex[x, 2], p = 0.8))

#iii. computing eigenvalues 
eigenvals <- function(matrix){

  eig <- eigen(matrix, only.values = TRUE)$values
  mat <- sapply(eig, function(x)c(Re(x), Im(x)))%>%
  t()
  colnames(mat) <- c('Real','Imaginary')
  return(mat)
}

x_rang <- 1000
D <- 100
rand_x <- matrix(data = runif(x_rang*2, min = -100, max = 100), ncol = 2)
eigen_list <- lapply(1:x_rang, function(x)gen_matrix(d = D,
                                       A = complex_function(rand_x[x, 1]),
                                       B = complex_function(rand_x[x,2])))%>%
  lapply(function(x)eigenvals(x))

#Random matrices
rand_matrices <- function(d, Xs){
  
  eigenlist <- lapply(1:nrow(Xs), function(x)gen_matrix(d = d,
                                         A = complex_function(Xs[x, 1]),
                                         B = complex_function(Xs[x,2])))%>%
    lapply(function(x)eigenvals(x))
  
  eig <- lapply(1:nrow(Xs), function(x)eigenlist[[x]]%>%
           cbind(x1 = Xs[x,1], x2 = Xs[x,2]))%>%
    do.call(rbind, .)%>%
    as.data.frame()
  
  return(eig)
}

rand_matrices(d = 20, Xs = rand_x)%>%
  ggplot(aes(x = Real, y = Imaginary))+
  #geom_point(size = 0.1)+
  geom_bin2d(bins = 200)+
  scale_fill_gradient(low = '#ebd3e6', high = '#d5b4e0')+
  lims(x = c(-10, 10), y = c(-10, 10))+
  theme_void()+
  theme(panel.background = element_rect(fill = 'black'), legend.position = "none")





#Mobius transformation ----------
mobius <- function(Z, n, theta){
  k <- 0:n
  a <- (Z*(theta[1] + theta[2]*1i) + (theta[3] + theta[4]*1i)) /  (Z*(theta[5] + theta[6]*1i) + (theta[7] + theta[8]*1i)) 
  b <- exp((2*pi*1i*k)/n) 
  c <- sapply(b, function(x)a*x)
  
  mat <- reshape2::melt(c)%>%
    setNames(c('z_i', 'n', 'complex'))%>%
    mutate(z = Z[z_i])%>%
    mutate(real = Re(complex), imag = Im(complex))
  
  return(mat)
}


theta <- runif(8, min = -20, max = 10)
z <- seq(from = -10, to = 10, by = .05)
n <- 60
mobius(Z =  z,
            n = n, 
            theta = theta)%>%
  ggplot(aes(x = z, y = real, col = imag, group = n))+
  geom_line()+
  scale_color_gradient(low = '#e2e7ea', high = '#728998')+
  theme_void()+
  theme(panel.background = element_rect(fill = '#39444c'), legend.position = "none")

  

























