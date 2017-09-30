# package
set.seed(1234)
library(randomForest)
library(miscTools)
library(ggplot2)

### function
# scatter plot and time_series plot
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
# collect data at last_stat and next_stat  
dataCollect_ln <- function(df_t, list_){
  if(nrow(list_) == 0){
    y <- df_t[df_t$s_stat == stat, 3:5]
  }else{
    if(nrow(list_) == 1){
      y <- df_t[df_t$s_stat == list_[1], 3:5]
    } else{
      y <- df_t[df_t$s_stat == list_[1], 3:5]
      for (i in 2:nrow(list_)){
        df1 <- df_t[df_t$s_stat == list_[i], 3:5]
        if( nrow(df1) == 0){
          next
        } else{
          y <- cbind(y, df1)
        }
      }
    }
  }
  return(y)
}
# collect data at select_stat
dataCollect_self <- function(df1, df2, df3){
  inter = nrow(df2) / min(nrow(df1), nrow(df2), nrow(df3))
  if(inter == 1){
    y <- df2[1:min(nrow(df1), nrow(df2), nrow(df3)), ]
  } else{
    y <- df2[1:min(nrow(df1), nrow(df2), nrow(df3)), ]
    for(i in 2:inter){
      t1 <- i*min(nrow(df1), nrow(df2), nrow(df3))
      t2 <- (i+1)*min(nrow(df1), nrow(df2), nrow(df3))
      df2_ <- df2[t1:t2, ]
      y <- cbind(y, df1)
    }
  }
  return(y)
}

# import dataset
g <- read.csv("C:/Users/stat_pc/Desktop/etc/group.csv")
df <- read.csv("C:/Users/stat_pc/Desktop/etc/M0405.csv")
df$time_ <- (df$mon*100000 + df$day*1000 + df$mins/5)
df <- df[ -c(1:4) ]

# input condition
stat = "01F0005S"
stat_next = "01F0017S" # 
mon = 9
day = 8
hr = 12
mins = 40

# calculate time convert, last_stat and next_stat
t <- mon*100000 + day*1000 + (hr*60 + mins)/5
df_t <- df[df$time_ < t, ]
last_stat <- array(g[(g$e_stat == stat), ]$s_stat)
next_stat <- array(g[(g$s_stat == stat), ]$e_stat)

# collect dataset
df1 <- dataCollect_ln(df_t, last_stat)
df2 <- df_t[df_t$s_stat == stat, ]
df2 <- df2[order( df2[,2], df2[,6] ), 3:5]
df3 <- dataCollect_ln(df_t, next_stat)

df1 <- df1[1:min(nrow(df1), nrow(df2), nrow(df3)), ]
df2 <- dataCollect_self(df1, df2, df3)
df3 <- df3[1:min(nrow(df1), nrow(df2), nrow(df3)), ]
df4 <- df_t[ df_t$s_stat == stat & df_t$e_stat == stat_next, ]$speed
df_ <- cbind(df1,df2,df3,df4)

rm(df1,df2,df3,df4)
# last one
X_test <- df_[nrow(df_), 1:ncol(df_)-1]
true_speed <- df[df$time_ == t & df_t$s_stat == stat & df_t$e_stat == stat_next, ]$speed

# X, y <- use row_data(t-1) to predict row_data(t)
X <- df_[1: nrow(df_)-1, 1:ncol(df_)-1]
y <- data.frame(df_[2:nrow(df_), ncol(df_)])
colnames(y) <- c("y_speed")
df <- data.frame(cbind(X, y))
rm(X, y)
# train & test
train_test <- as.integer(nrow(df)*0.8)
train <- df[1:train_test, ]
test <- df[train_test:nrow(df), ]

# random forest regression
rf <- randomForest(y_speed ~ ., data=train, ntree=500)

y_true <- test$y_speed
y_pred <- predict(rf, test[ , 1:ncol(df_)-1 ])
r2 <- rSquared(y_true, y_true - y_pred)
r2
mse <- mean( (y_true - y_pred)^2 ) 
mse

# scatter plot(pred v.s true)
plooot(y_true, y_pred, 0, 520)
# zoom in
plooot(y_true, y_pred, 350, 450)

# we concern!
pred_speed <- predict(rf, X_test)
pred_speed
true_speed
