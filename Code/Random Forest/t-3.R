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

# training data using random forest
library(randomForest)
rf.t3 <- randomForest(br ~ . , data = train.t3,importance = TRUE)
rf.t3$confusion
rf.t3$importance

# predict
predict.t3 <- predict(rf.t3, test.t3)

predict.t3.round <- sapply(predict.t3, round)

table(predict.t3.round, test.t3$br)
