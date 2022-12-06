library(jpeg)
library(ggplot2)
library(grid)
require(gridExtra)

img <- readJPEG("C:\\Users\\lisilong\\Desktop\\Moodle\\math336\\Group project\\Image.jpg")
#make sure that the path is correct in your PC.

imgDm <- dim(img)

imgRGB <- data.frame(
  x = rep(1:(imgDm[2]%/%2), each = imgDm[1]),
  y = rep(imgDm[1]:1, (imgDm[2]%/%2) ),
  R = as.vector(img[ ,1:(imgDm[2]%/%2), 1]),
  G = as.vector(img[ ,1:(imgDm[2]%/%2), 2]),
  B = as.vector(img[ ,1:(imgDm[2]%/%2), 3])
)

plotOriginal <- ggplot(data = imgRGB) + 
  geom_point(colour = rgb(imgRGB[c("R", "G", "B")]), aes(x = x, y = y)) + 
  labs(title = "Original Image")

k <- 16 # Number of clusters, you can play with it to obtain different compression levels!
kMeans <- kmeans(imgRGB[,c("R", "G", "B")], centers = k) 
num.of.colours <- rgb(kMeans$centers[kMeans$cluster, ])

plotCompressed <-ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = num.of.colours) + 
  labs(title = paste("k-Means Clustering of ", k, " Colours"))

grid.arrange(plotOriginal, plotCompressed, nrow = 2)