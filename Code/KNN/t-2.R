df.t2 <- read.csv("t-2.csv")
#
scale.t2 <- scale(df.t2[1:7])
#
var(scale.t2[,1])
df.final.t2 <- cbind(scale.t2,df.t2[8])
#split
set.seed(101)
library(caTools)
sample.t2 <- sample.split(df.final.t2$br, SplitRatio = 0.7)
train.t2 <- subset(df.final.t2, sample.t2 == T)
test.t2 <- subset(df.final.t2, sample.t2 == F)

#train the data
library(class)

predict.t2 <- knn(train.t2[1:7], test.t2[1:7], train.t2$br, k = 2)
predict.t2

#misclassification rate calculation 
mean(test.t2$br !=predict.t2)

# changing k values to get better results
predict.t2 <- NULL
error.rate.t2 <- NULL

for (i in 1:10) {
  set.seed(101)
  predict.t2 <- knn(train.t2[1:7], test.t2[1:7], train.t2$br, k = i)
  error.rate.t2[i] <- mean(test.t2$br != predict.t2)
}

#using elbow method to find k
library(ggplot2)
k.values.t2 <- 1:10
error.t2 <- data.frame(error.rate.t2,k.values.t2)

pl.t2 <- ggplot(error.t2, aes(x=k.values.t2, y=error.rate.t2)) + geom_point() 
pl.t2 <- pl.t2 + geom_line(lty = "dotted", color = "red")
plot(pl.t2)

# k = 2 is best