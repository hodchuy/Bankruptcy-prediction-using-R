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

# training data using random forest
library(randomForest)
rf.t2a <- randomForest(br ~ . , data = train.t2a,importance = TRUE)


# predict
predict.t2a <- predict(rf.t2a, test.t2a)

predict.t2a.round <- sapply(predict.t2a, round)

table(predict.t2a.round, test.t2a$br)
