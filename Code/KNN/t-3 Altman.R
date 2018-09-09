df.t3a <- read.csv("t-3 Altman.csv")
#
scale.t3a <- scale(df.t3a[1:4])
#
var(scale.t3a[,1])
df.final.t3a <- cbind(scale.t3a,df.t3a[5])
#split
set.seed(101)
library(caTools)
sample.t3a <- sample.split(df.final.t3a$br, SplitRatio = 0.7)
train.t3a <- subset(df.final.t3a, sample.t3a == T)
test.t3a <- subset(df.final.t3a, sample.t3a == F)

#train the data
library(class)

predict.t3a <- knn(train.t3a[1:4], test.t3a[1:4], train.t3a$br, k = 2)
predict.t3a

#misclassification rate calculation 
mean(test.t3a$br !=predict.t3a)

# changing k values to get better results
predict.t3a <- NULL
error.rate.t3a <- NULL

for (i in 1:10) {
  set.seed(101)
  predict.t3a <- knn(train.t3a[1:4], test.t3a[1:4], train.t3a$br, k = i)
  error.rate.t3a[i] <- mean(test.t3a$br != predict.t3a)
}

#using elbow method to find k
library(ggplot2)
k.values.t3a <- 1:10
error.t3a <- data.frame(error.rate.t3a,k.values.t3a)

pl.t3a <- ggplot(error.t3a, aes(x=k.values.t3a, y=error.rate.t3a)) + geom_point() 
pl.t3a <- pl.t3a + geom_line(lty = "dotted", color = "red")
plot(pl.t3a)

# k = 2 is best