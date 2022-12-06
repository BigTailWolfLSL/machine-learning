#################################################################################################
## This script will help you get started with your project                                     ##
## Specifically, you'll learn how to load an image into R and obtain the RGB data frame        ##
## You'll also see the expected result, once the image is compressed using k-means clustering  ##
## Here, we will invoke an existing kmeans function.                                           ##  
## For your project you may use this code but you are also welcome to write your own.          ##
#################################################################################################

# If you have not installed the following packages, then you have to run the following lines once.
# There is not need to run them each time.
# install.packages("jpeg")
# install.packages("gridExtra")
# install.packages("ggplot2")

library(jpeg)
library(ggplot2)
library(grid)
require(gridExtra)

# Load Image.jpg
# Ensure that your Image.jpg file is in your working directory
# You can modify the working directory using setwd(), e.g.
# setwd("~/Desktop/MATH336/Project/")
img <- readJPEG("Image.jpg") # Read the image

# Obtain Image dimension
imgDm <- dim(img)

# Assign RGB channels to data frame 
imgRGB <- data.frame(
  x = rep(1:imgDm[2], each = imgDm[1]),
  y = rep(imgDm[1]:1, imgDm[2]),
  R = as.vector(img[ , , 1]),
  G = as.vector(img[ , , 2]),
  B = as.vector(img[ , , 3])
)

# Obtain a plot of the original image
plotOriginal <- ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = rgb(imgRGB[c("R", "G", "B")])) + 
  labs(title = "Original Image")

# Compress the image using k-means clustering 
k <- 3 # Number of clusters, you can play with it to obtain different compression levels!
kMeans <- kmeans(imgRGB[, c("R", "G", "B")], centers = k) 
num.of.colours <- rgb(kMeans$centers[kMeans$cluster, ])

# Obtain a plot of the compressed image
plotCompressed <-ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = num.of.colours) + 
  labs(title = paste("k-Means Clustering of ", k, " Colours"))

# Plot the original and compressed image side by side
grid.arrange(plotOriginal, plotCompressed, nrow = 2)  
