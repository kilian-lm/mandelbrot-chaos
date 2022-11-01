# mandelbrot_naive.R
# "Naive" implementation of Mandelbrot Set in R
# Myles Harrison
# http://www.everydayanalytics.ca

# parameters
cols=colorRampPalette(c("blue","yellow","red","black"))(11)
xmin = -2
xmax = 2
nx = 150
ymin = -1.5
ymax = 1.5
ny = 150
n=200

# variables
x <- seq(xmin, xmax, length.out=nx)
y <- seq(ymin, ymax, length.out=ny)
c <- outer(x,y*1i,FUN="+")
z <- matrix(0.0, nrow=length(x), ncol=length(y))
k <- matrix(0.0, nrow=length(x), ncol=length(y))


Mod(2)
??Mod
n

for (rep in 1:n) { 
  print(rep)
  for (i in 1:nx) { 
    for (j in 1:ny) { 
      if(Mod(z[i,j]) < 2 && k[i,j] < n) {
        z[i,j] <- z[i,j]^2 + c[i,j]
        k[i,j] <- k[i,j] + 1
      }
    }
  }
}

summary(z)
z[1]

image(x,y,k, col=cols)
x
y

# df= data.frame(x,y,z)



# 3d ----------------------------------------------------------------------



# Load the required packages
library(caTools)
library(repr)
library(rgl)

# Define the fractal generating function
mandelbrot <- function(x,y, maxiter) {
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
res <- outer(x, y, Vectorize(mandelbrot))

# Plot the fractal
rgl::rgl.open()
rgl::surface3d(x, y, res, color = terrain.colors(100))


