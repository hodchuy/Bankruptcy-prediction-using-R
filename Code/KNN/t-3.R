df.t3 <- read.csv("t-3.csv")
#
scale.t3 <- scale(df.t3[1:7])
#
var(scale.t3[,1])
df.final.t3 <- cbind(scale.t3,df.t3[8])
#split
set.seed(101)
library(caTools)
sample.t3 <- sample.split(df.final.t3$br, SplitRatio = 0.7)
train.t3 <- subset(df.final.t3, sample.t3 == T)
test.t3 <- subset(df.final.t3, sample.t3 == F)

#train the data
library(class)

predict.t3 <- knn(train.t3[1:7], test.t3[1:7], train.t3$br, k = 2)
predict.t3

#misclassification rate calculation 
mean(test.t3$br !=predict.t3)

# changing k values to get better results
predict.t3 <- NULL
error.rate.t3 <- NULL

for (i in 1:10) {
  set.seed(101)
  predict.t3 <- knn(train.t3[1:7], test.t3[1:7], train.t3$br, k = i)
  error.rate.t3[i] <- mean(test.t3$br != predict.t3)
}

#using elbow method to find k
library(ggplot2)
k.values.t3 <- 1:10
error.t3 <- data.frame(error.rate.t3,k.values.t3)

pl.t3 <- ggplot(error.t3, aes(x=k.values.t3, y=error.rate.t3)) + geom_point() 
pl.t3 <- pl.t3 + geom_line(lty = "dotted", color = "red")
plot(pl.t3)

# k = 2 is best