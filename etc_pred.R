# package
set.seed(1234)
library(randomForest)
library(miscTools)
library(ggplot2)
# function
plooot <- function(y_true, y_pred, xlim1, xlim2){
  p <- ggplot(aes(x=actual, y=pred),
              data=data.frame(actual=y_true, pred=y_pred))
  print (p + geom_point() + geom_abline(color="red") +
           ggtitle(paste("RandomForest Regression in R r^2=", r2, sep="")))

  y_ <- data.frame(cbind(y_true, y_pred))
  x_ <- 1:nrow(y_)
  print (ggplot(data = y_, aes(x = x_)) +
           geom_line(aes(y=y_true), colour="#FF6699") +
           geom_line(aes(y=y_pred), colour="#4400CC") + 
           coord_cartesian(xlim = c(xlim1, xlim2)))
}

# import dataset
df <- read.csv("C:/Users/stat_pc/Desktop/code/etc/M0405.csv")
# find station which we concern
df1 <- df[df$s_stat == "01F0750S", 7:9]
df2 <- df[df$s_stat == "01F0880S", 7:9]
df3 <- df[df$s_stat == "01F0928S", 7:9]
# use info of last, self and next station
df <- data.frame(cbind(df1,df2,df3,df2$speed))
colnames(df) <- c("last_c","last_n","last_s","_c","_n","_s","next_c","next_n","next_s","speed")
rm(df1, df2, df3)
# X, y <- use row_data at (t-1) to predict row_data (t)
X <- df[1:2591, 1:9]
y <- data.frame(df[2:2592, 10])
colnames(y) <- c("speed")
df <- data.frame(cbind(X, y))
# train & test
train_test <- as.integer(nrow(X)*0.8)
train <- df[1:train_test, ]
test <- df[train_test:nrow(df), ]

# time series plot(use (t-1) to be prediction) BASIC
y_true <- test$speed[2:nrow(test)]
y_pred <- test$speed[1:nrow(test)-1] # r2=0.9622
mse <- mean( (y_true - y_pred)^2 ) # mse=17.0751
r2 <- rSquared(y_true, y_true - y_pred)
plooot(y_true, y_pred, 0, 520)

# random forest regression
rf <- randomForest(speed ~ ., data=train, ntree=500)
y_true <- test$speed
y_pred <- predict(rf, test[,1:9])
r2 <- rSquared(y_true, y_true - y_pred) # r2=0.9694
mse <- mean( (y_true - y_pred)^2 ) # mse=13.8668
# scatter plot(pred v.s true)
plooot(y_true, y_pred, 0, 520)
# zoom in
plooot(y_true, y_pred, 300, 400)



