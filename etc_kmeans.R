# package
library("ggplot2")
set.seed(1234)

# import dataset
df <- read.csv("C:/Users/stat_pc/Desktop/code/etc/M0405.csv")
df_ <- df[df$s_stat == "01F0880S", ]

# time series
x_ <- 1:nrow(df_)
ggplot(aes(x = x_, y = speed), data = df_) + geom_line()

# kmeans
X <- cbind(df_$clock, df_$number, df_$speed)
kmeans.result <- kmeans(X,5)
kmeans.result
plot(X[ ,-c(1)], col=kmeans.result$cluster)
plot(X[ ,-c(2)], col=kmeans.result$cluster)
plot(X[ ,-c(3)], col=kmeans.result$cluster)

# pca
x_pca <- prcomp(X, center=TRUE, scale.=TRUE) 
x_pca
screeplot(x_pca, type="lines",col=3)
plot(x_pca$x, col=kmeans.result$cluster)
# plot label of kmeans
x_kmeans <- data.frame(cbind(df_$speed, kmeans.result$cluster))

plot(x_pca$x, col=kmeans.result$cluster)


df_ <- x_kmeans[x_kmeans$X2 == "1", ]
x_ <- 1:nrow(df_)
ggplot(aes(x = x_, y = X1), data = df_) + geom_line()
for (i in c(1,2,3,4,5)){
  df_ <- x_kmeans[x_kmeans$X2 == i, ]
  x_ <- 1:nrow(df_)
  print (ggplot(aes(x = x_, y = X1), data = df_) + geom_line())
}


