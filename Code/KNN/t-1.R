df.t1 <- read.csv("t-1.csv")
#
scale.t1 <- scale(df.t1[1:7])
#
var(scale.t1[,1])
df.final.t1 <- cbind(scale.t1,df.t1[8])
#split
set.seed(101)
library(caTools)
sample.t1 <- sample.split(df.final.t1$br, SplitRatio = 0.7)
train.t1 <- subset(df.final.t1, sample.t1 == T)
test.t1 <- subset(df.final.t1, sample.t1 == F)

#train the data
library(class)

predict.t1 <- knn(train.t1[1:7], test.t1[1:7], train.t1$br, k = 2)
predict.t1

#misclassification rate calculation 
mean(test.t1$br !=predict.t1)

# changing k values to get better results
predict.t1 <- NULL
error.rate.t1 <- NULL

for (i in 1:10) {
  set.seed(101)
  predict.t1 <- knn(train.t1[1:7], test.t1[1:7], train.t1$br, k = i)
  error.rate.t1[i] <- mean(test.t1$br != predict.t1)
}

#using elbow method to find k
library(ggplot2)
k.values.t1 <- 1:10
error.t1 <- data.frame(error.rate.t1,k.values.t1)

pl.t1 <- ggplot(error.t1, aes(x=k.values.t1, y=error.rate.t1)) + geom_point() 
pl.t1 <- pl.t1 + geom_line(lty = "dotted", color = "red")
plot(pl.t1)

# k = 2 is best