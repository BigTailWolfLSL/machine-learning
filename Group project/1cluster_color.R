#################################################################################################
## This script will help you get started with your project                                     ##
## Specifically, you'll learn how to load an image into R an? obtain the RGB data frame        ##
## You'll also see the expected result, once the image is compressed using k-means clustering  ##
## Here, we will invoke an existing kmeans function.                                           ##  
## For your project you may use this code but you are also welcome to write your own.          ##
#################################################################################################

# If you have not installed the following packages, then you have to run the following lines once.
# There is not need to run them each time.
# install.packages("jpeg")
# install.packages("gridExtra")
# install.packages("ggplot2")#ѡno

library(jpeg)
library(ggplot2)
library(grid)
require(gridExtra)

# Load Image.jpg
# Ensure that your ?mage.jpg file is in your working directory
# You can modify the working directory using setwd(), e.g.
# setwd("~/Desktop/MATH336/Project/")
img <- readJPEG("C:\\Users\\lisilong\\Desktop\\Moodle\\math336\\Image.jpg") # Read the image
#�����һ����ֵ����ά��???

# Obtain Image dimension
imgDm <- dim(img)
#���315 850 3 ������������ά��С����������ͼƬ��RGB��ɫ

# Assign RGB channels to data frame 
imgRGB <- data.frame(
  x = rep(1:imgDm[2], each = imgDm[1]),#315��1��315��2.����������������/�����꣨��һ������������ֵ��
  y = rep(imgDm[1]:1, imgDm[2]),#815��315~1�������൱��meshgrid 
  R = as.vector(img[ , , 1]),#��vector�����������꣨1��315�����£�1��1�����ϣ�850��315�����£�850��1��
  G = as.vector(img[ , , 2]),#��Ϊ�����������½ǽ�����ͬ��ѧ
  B = as.vector(img[ , , 3])
)#��ɶ�ά�������У�x����y����RGB��ֵ

# for (j in 1:50){
#   for (i in 1:100){
#     imgRGB[(j-1)*315+i,3]=1
#     imgRGB[(j-1)*315+i,4]=1
#     imgRGB[(j-1)*315+i,5]=1
#   }
# }

# Obtain a plot of the original image
plotOriginal <- ggplot(data = imgRGB) + #aes��ggplot����(��geom_point����Ҳ����)������ggplot��x�����y������б�
  geom_point(colour = rgb(imgRGB[c("R", "G", "B")]), aes(x = x, y = y)) + #ͨ�����ͼƬ��ɫ
  labs(title = "Original Image")#imgRGB�����������Ƕ�imgRGB��Ӧ���е�һ����ȡ����һ����ά�����е�����

# Compress the image using k-means clustering 
k <- 3 # Number of clusters, you can play with it to obtain different compression levels!
kMeans <- kmeans(imgRGB[,c("R", "G", "B")], centers = k) #c()ǰ����û�ж���Ҳ��һ���ġ�kmeans�Ƕ�ά�����࣬�����ݼ��ֳ�k�࣬����һ��k��meanֵ����һ�����������
num.of.colours <- rgb(kMeans$centers[kMeans$cluster, ])# ��˼�����Զ����࣬���Եط�Ϊ������ɫ

# Obtain a plot of the compressed image
plotCompressed <-ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = num.of.colours) + 
  labs(title = paste("k-Means Clustering of ", k, " Colours"))

# Plot the original and compressed image side by side
grid.arrange(plotOriginal, plotCompressed, nrow = 2)  