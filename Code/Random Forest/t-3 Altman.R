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

# training data using random forest
library(randomForest)
rf.t3a <- randomForest(br ~ . , data = train.t3a,importance = TRUE)


# predict
predict.t3a <- predict(rf.t3a, test.t3a)

predict.t3a.round <- sapply(predict.t3a, round)

table(predict.t3a.round, test.t3a$br)
