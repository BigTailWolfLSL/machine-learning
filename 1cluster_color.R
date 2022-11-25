#################################################################################################
## This script will help you get started with your project                                     ##
## Specifically, you'll learn how to load an image into R an? obtain the RGB data frame        ##
## You'll also see the expected result, once the image is compressed using k-means clustering  ##
## Here, we will invoke an existing kmeans function.                                           ##  
## For your project y?u may use this code but you are also welcome to write your own.          ##
#################################################################################################

# If you have not installed the following packages, then you have to run the foll?wing lines once.
# There is not need to run them each time.
# install.packages("jpeg")
# install.packages("gridExtra")
# install.packages("ggplot2")#选no

library(jpeg)
library(ggplot2)
library(grid)
require(gridExtra)

# Load Image.jpg
# Ensure that your ?mage.jpg file is in your working directory
# You can modify the working directory using setwd(), e.g.
# setwd("~/Desktop/MATH336/Project/")
img <- readJPEG("C:\\Users\\lisilong\\Desktop\\Moodle\\math336\\Image.jpg") # Read the image
#结果是一堆数值，三维数???

# Obtain Image dimension
imgDm <- dim(img)
#结果315 850 3 是这个数组的三维大小，最后的三是图片的RGB颜色

# Assign RGB channels to data frame 
imgRGB <- data.frame(
  x = rep(1:imgDm[2], each = imgDm[1]),#315个1，315个2.。。估计是列坐标/横坐标（第一个数字???列坐标值）
  y = rep(imgDm[1]:1, imgDm[2]),#815个315~1，两行相当于meshgrid 
  R = as.vector(img[ , , 1]),#变vector，按左上坐标（1，315）左下（1，1）右上（850，315）右下（850，1）
  G = as.vector(img[ , , 2]),#因为坐标轴在左下角建立，同数学
  B = as.vector(?mg[ , , 3])
)#变成二维数组五列，x坐标y坐标RGB数值

# for (j in 1:50){
#   for (i in 1:100){
#     imgRGB[(j-1)*315+i,3]=1
#     imgRGB[(j-1)*315+i,4]=1
#     imgRGB[(j-1)*315+i,5]=1
#   }
# }

# Obtain a plot of the original image
plotOriginal <- ggplot(da?a = imgRGB) + #aes在ggplot中用(在geom_point里用也可以)，告诉ggplot它x坐标和y坐标的列表
  geom_point(colour = rgb(imgRGB[c("R", "G", "B")]), aes(x = x, y = y)) + #通过点给图片上色
  labs(title = "Original Image")#imgRGB【。。。】是对imgRGB对应三列的一个截取???是一个二维有三列的数组

# Compress the image using k-means clustering 
k <- 3 # Number of clusters, you can play with it to obtain different compression levels!
kMeans <- kmeans(imgRGB[,c("R", "G", "B")], centers = k) #c()前面有没有逗号也是一样的。kmeans是???类，将数据集分成k类，返回一个k个mean值，和一个分类的数据
num.of.colours <- rgb(kMeans$centers[kMeans$cluster, ])# 意思就是自动分类，粗略地分为三个颜色

# Obtain a plot of the compressed image
plotCompressed <-ggplot(data = imgRGB, aes(x = x, y = y)) + 
  g?om_point(colour = num.of.colours) + 
  labs(title = paste("k-Means Clustering of ", k, " Colours"))

# Plot the original and compressed image side by side
grid.arrange(plotOriginal, plotCompressed, nrow = 2)  
