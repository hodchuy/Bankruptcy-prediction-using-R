df.t1a <- read.csv("t-1 Altman.csv")
#
scale.t1a <- scale(df.t1[1:4])
#
var(scale.t1a[,1])
df.final.t1a <- cbind(scale.t1a,df.t1a[5])
#split
set.seed(101)
library(caTools)
sample.t1a <- sample.split(df.final.t1a$br, SplitRatio = 0.7)
train.t1a <- subset(df.final.t1a, sample.t1a == T)
test.t1a <- subset(df.final.t1a, sample.t1a == F)

# training data using random forest
library(randomForest)
rf.t1a <- randomForest(br ~ . , data = train.t1a,importance = TRUE)


# predict
predict.t1a <- predict(rf.t1a, test.t1a)

predict.t1a.round <- sapply(predict.t1a, round)

table(predict.t1a.round, test.t1a$br)
