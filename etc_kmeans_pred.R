# package
set.seed(1234)
library(ggplot2)
library(miscTools)

# input condition
stat = "01F0005S"
stat_next = "01F0017S" # 
mon = 9
day = 8
hr = 12
mins = 40
t <- mon*100000 + day*1000 + (hr*60 + mins)/5

# import dataset
df <- read.csv("C:/Users/stat_pc/Desktop/etc/M0405.csv")
df$time_ <- (df$mon*100000 + df$day*1000 + df$mins/5)
df <- df[ -c(1:4) ]
df_t <- df[df$time_ < t, ]
df_ <- df_t[df_t$s_stat == "01F0880S", ]

# time series
x_ <- 1:nrow(df_)
ggplot(aes(x = x_, y = speed), data = df_) + geom_line()

# kmeans
X <- cbind(df_$clock, df_$number, df_$speed)
km <- kmeans(X,5)
km
plot(X[ ,-c(1)], col=km$cluster)

# pca
x_pca <- prcomp(X, center=TRUE, scale.=TRUE) 
x_pca
screeplot(x_pca, type="lines",col=3)
plot(x_pca$x, col=km$cluster)

# plot label of kmeans
x_kmeans <- data.frame(cbind(df_$speed, km$cluster))

plot(x_pca$x, col=km$cluster)

df_ <- x_kmeans[x_kmeans$X2 == "1", ]
x_ <- 1:nrow(df_)
ggplot(aes(x = x_, y = X1), data = df_) + geom_line()
for (i in c(1,2,3,4,5)){
  df_ <- x_kmeans[x_kmeans$X2 == i, ]
  x_ <- 1:nrow(df_)
  print (ggplot(aes(x = x_, y = X1), data = df_) + geom_line())
}

### lebel
df_stat <- df_t[df_t$s_stat == stat, 3:5]
closest.cluster <- function(x) {
  cluster.dist <- apply(km$centers, 1, function(y) sqrt(sum((x-y)^2)))
  return(which.min(cluster.dist)[1])
}
clusters2 <- apply(df_stat, 1, closest.cluster)
clusters2
