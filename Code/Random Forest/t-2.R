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

# training data using random forest
library(randomForest)
rf.t2 <- randomForest(br ~ . , data = train.t2,importance = TRUE)

# predict
predict.t2 <- predict(rf.t2, test.t2)

predict.t2.round <- sapply(predict.t2, round)

table(predict.t2.round, test.t2$br)
