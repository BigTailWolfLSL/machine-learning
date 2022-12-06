library(jpeg)
library(ggplot2)
library(grid)
require(gridExtra)

img <- readJPEG("C:\\Users\\lisilong\\Desktop\\Moodle\\math336\\Group project\\Image.jpg")
#make sure that the path is correct in your PC.

imgDm <- dim(img)

imgR<-img[2*(1:(imgDm[1]%/%2) ),2*(1:(imgDm[2]%/%2) ), 1]+
  img[2*(1:(imgDm[1]%/%2) )-1,2*(1:(imgDm[2]%/%2) ), 1]+
  img[2*(1:(imgDm[1]%/%2) ),2*(1:(imgDm[2]%/%2) )-1, 1]+
  img[2*(1:(imgDm[1]%/%2) ),2*(1:(imgDm[2]%/%2) ), 1]
imgR<-imgR/4
imgG<-img[2*(1:(imgDm[1]%/%2) ),2*(1:(imgDm[2]%/%2) ), 2]+
  img[2*(1:(imgDm[1]%/%2) )-1,2*(1:(imgDm[2]%/%2) ), 2]+
  img[2*(1:(imgDm[1]%/%2) ),2*(1:(imgDm[2]%/%2) )-1, 2]+
  img[2*(1:(imgDm[1]%/%2) ),2*(1:(imgDm[2]%/%2) ), 2]
imgG<-imgG/4
imgB<-img[2*(1:(imgDm[1]%/%2) ),2*(1:(imgDm[2]%/%2) ), 3]+
  img[2*(1:(imgDm[1]%/%2) )-1,2*(1:(imgDm[2]%/%2) ), 3]+
  img[2*(1:(imgDm[1]%/%2) ),2*(1:(imgDm[2]%/%2) )-1, 3]+
  img[2*(1:(imgDm[1]%/%2) ),2*(1:(imgDm[2]%/%2) ), 3]
imgB<-imgB/4
imgRGB <- data.frame(
  x = rep(1:(imgDm[2]%/%2), each = (imgDm[1]%/%2) ),
  y = rep((imgDm[1]%/%2):1, (imgDm[2]%/%2) ),
  R = as.vector(imgR),
  G = as.vector(imgG),
  B = as.vector(imgB)
)

plotOriginal <- ggplot(data = imgRGB) + 
  geom_point(colour = rgb(imgRGB[c("R", "G", "B")]), aes(x = x, y = y)) + 
  labs(title = "1/4 resolution Original Image")

k <- 16 # Number of clusters, you can play with it to obtain different compression levels!
kMeans <- kmeans(imgRGB[,c("R", "G", "B")], centers = k) 
num.of.colours <- rgb(kMeans$centers[kMeans$cluster, ])

plotCompressed <-ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = num.of.colours) + 
  labs(title = paste("1/4 resolution k-Means Clustering of ", k, " Colours"))

grid.arrange(plotOriginal, plotCompressed, nrow = 2)
