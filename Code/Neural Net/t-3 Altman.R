#import file
df.t3a <- read.csv("t-3 Altman.csv")

#scaling variables
scale.t3a <- scale(df.t3a [1:4])
var(scale.t3a[,1])
df.final.t3a <- cbind(scale.t3a,df.t3a[5])

#loading split library
library(caTools)
set.seed(101)
split.t3a = sample.split(df.final.t3a$br, SplitRatio = 0.7)
train.t3a = subset(df.final.t3a, split == TRUE)
test.t3a = subset(df.final.t3a, split == FALSE)
str(train.t3a)

#loading neural network library
library(neuralnet)

#training
nn.t3a <- neuralnet(br ~ a + b + c + d, data = train.t3a, hidden = 5, linear.output = FALSE)
predicted.nn.t3a <- compute(nn.t3a, test.t3a[,1:4])

#rounding predicted values to either 1 or 0
predictions.t3a <- sapply(predicted.nn.t3a$net.result, round)

#testing results
table(predictions.t3a,test.t3a$br)
