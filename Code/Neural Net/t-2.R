#import file
df.t2 <- read.csv("t-2.csv")

#scaling variables
scale.t2 <- scale(df.t2[1:7])
var(scale.t2[,1])
df.final.t2 <- cbind(scale.t2,df.t2[8])

#loading split library
library(caTools)
set.seed(101)
split.t2 = sample.split(df.final.t2$br, SplitRatio = 0.7)
train.t2 = subset(df.final.t2, split == TRUE)
test.t2 = subset(df.final.t2, split == FALSE)
str(train.t2)

#loading neural network library
library(neuralnet)

#training
nn.t2 <- neuralnet(br ~ a + b + c + d + e + f + g, data = train.t2, hidden = 5, linear.output = FALSE)
predicted.nn.t2 <- compute(nn.t2, test.t2[,1:7])

#rounding predicted values to either 1 or 0
predictions.t2 <- sapply(predicted.nn.t2$net.result, round)

#testing results
table(predictions.t2,test.t2$br)
