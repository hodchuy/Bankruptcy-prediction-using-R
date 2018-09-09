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

# training data using random forest
library(randomForest)
rf.t1 <- randomForest(br ~ . , data = train.t1,importance = TRUE)
rf.t1$confusion
rf.t1$importance

# predict
predict.t1 <- predict(rf.t1, test.t1)

predict.t1.round <- sapply(predict.t1, round)

table(predict.t1.round, test.t1$br)
