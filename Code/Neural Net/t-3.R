#import file
df.t3 <- read.csv("t-3.csv")

#scaling variables
scale.t3 <- scale(df.t3[1:7])
var(scale.t3[,1])
df.final.t3 <- cbind(scale.t3,df.t3[8])

#loading split library
library(caTools)
set.seed(101)
split.t3 = sample.split(df.final.t3$br, SplitRatio = 0.7)
train.t3 = subset(df.final.t3, split == TRUE)
test.t3 = subset(df.final.t3, split == FALSE)
str(train.t3)

#loading neural network library
library(neuralnet)

#training
nn.t3 <- neuralnet(br ~ a + b + c + d + e + f + g, data = train.t3, hidden = 5, linear.output = FALSE)
predicted.nn.t3 <- compute(nn.t3, test[,1:7])

#rounding predicted values to either 1 or 0
predictions.t3 <- sapply(predicted.nn.t3$net.result, round)

#testing results
table(predictions.t3,test.t3$br)

