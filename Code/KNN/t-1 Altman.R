df.t1a <- read.csv("t-1 Altman.csv")
#
scale.t1a <- scale(df.t1a[1:4])
#
var(scale.t1a[,1])
df.final.t1a <- cbind(scale.t1a,df.t1a[5])
#split
set.seed(101)
library(caTools)
sample.t1a <- sample.split(df.final.t1a$br, SplitRatio = 0.7)
train.t1a <- subset(df.final.t1a, sample.t1a == T)
test.t1a <- subset(df.final.t1a, sample.t1a == F)

#train the data
library(class)

predict.t1a <- knn(train.t1a[1:4], test.t1a[1:4], train.t1a$br, k = 2)
predict.t1a

#misclassification rate calculation 
mean(test.t1a$br !=predict.t1a)

# changing k values to get better results
predict.t1a <- NULL
error.rate.t1a <- NULL

for (i in 1:10) {
  set.seed(101)
  predict.t1a <- knn(train.t1a[1:4], test.t1a[1:4], train.t1a$br, k = i)
  error.rate.t1a[i] <- mean(test.t1a$br != predict.t1a)
}

#using elbow method to find k
library(ggplot2)
k.values.t1a <- 1:10
error.t1a <- data.frame(error.rate.t1a,k.values.t1a)

pl.t1a <- ggplot(error.t1a, aes(x=k.values.t1a, y=error.rate.t1a)) + geom_point() 
pl.t1a <- pl.t1a + geom_line(lty = "dotted", color = "red")
plot(pl.t1a)

# k = 2 is best