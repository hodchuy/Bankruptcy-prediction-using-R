df.t2a <- read.csv("t-2 Altman.csv")
#
scale.t2a <- scale(df.t2a[1:4])
#
var(scale.t2a[,1])
df.final.t2a <- cbind(scale.t2a,df.t2a[5])
#split
set.seed(101)
library(caTools)
sample.t2a <- sample.split(df.final.t2a$br, SplitRatio = 0.7)
train.t2a <- subset(df.final.t2a, sample.t2a == T)
test.t2a <- subset(df.final.t2a, sample.t2a == F)

#train the data
library(class)

predict.t2a <- knn(train.t2a[1:4], test.t2a[1:4], train.t2a$br, k = 2)
predict.t2a

#misclassification rate calculation 
mean(test.t2a$br !=predict.t2a)

# changing k values to get better results
predict.t2a <- NULL
error.rate.t2a <- NULL

for (i in 1:10) {
  set.seed(101)
  predict.t2a <- knn(train.t2a[1:4], test.t2a[1:4], train.t2a$br, k = i)
  error.rate.t2a[i] <- mean(test.t2a$br != predict.t2a)
}

#using elbow method to find k
library(ggplot2)
k.values.t2a <- 1:10
error.t2a <- data.frame(error.rate.t2a,k.values.t2a)

pl.t2a <- ggplot(error.t2a, aes(x=k.values.t2a, y=error.rate.t2a)) + geom_point() 
pl.t2a <- pl.t1a + geom_line(lty = "dotted", color = "red")
plot(pl.t2a)

# k = 2 is best