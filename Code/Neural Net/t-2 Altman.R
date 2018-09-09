#import file
df.t2a <- read.csv("t-2 Altman.csv")

#scaling variables
scale.t2a <- scale(df.t2a [1:4])
var(scale.t2a[,1])
df.final.t2a <- cbind(scale.t2a,df.t2a[5])

#loading split library
library(caTools)
set.seed(101)
split.t2a = sample.split(df.final.t2a$br, SplitRatio = 0.7)
train.t2a = subset(df.final.t2a, split == TRUE)
test.t2a = subset(df.final.t2a, split == FALSE)
str(train.t2a)

#loading neural network library
library(neuralnet)

#training
nn.t2a <- neuralnet(br ~ a + b + c + d, data = train.t2a, hidden = 5, linear.output = FALSE)
predicted.nn.t2a <- compute(nn.t2a, test.t2a[,1:4])

#rounding predicted values to either 1 or 0
predictions.t2a <- sapply(predicted.nn.t2a$net.result, round)

#testing results
table(predictions.t2a,test.t2a$br)
