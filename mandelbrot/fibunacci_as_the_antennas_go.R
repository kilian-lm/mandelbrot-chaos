


x = seq(0,9,1)
y = c(0,1,1,2,3,5,8,13,21, 34)

plot_ly(x=x, y=y, type='scatter', mode='lines+markers')






# Load the required packages
library(caTools)
library(repr)
library(rgl)

# Define the fractal generating function
fibonacci <- function(x,y) {
  maxiter = 100
  z <- x+i*y
  c <- z
  n <- 0
  
  for (n in 0:maxiter) {
    if (abs(z) > 2) {
      break
    }
    z <- z^2+c
  }
  
  return(n)
}

# Generate data for the fractal
x <- seq(-2, 0.5, length.out = 1000)
y <- seq(-1, 1, length.out = 1000)

# Calculate the fractal
res <- outer(x, y, Vectorize(fibonacci))

# Plot the fractal
rgl::rgl.open()
rgl::surface3d(x, y, res, color = terrain.colors(100))
