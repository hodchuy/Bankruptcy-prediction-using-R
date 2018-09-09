#import file
df <- read.csv("t-1.csv")

#scaling variables
scale.inde.var <- scale(df[1:7])
var(scale.inde.var[,1])
df.final <- cbind(scale.inde.var,df[8])

#loading split library
library(caTools)
set.seed(101)
split = sample.split(df.final$br, SplitRatio = 0.7)
train = subset(df.final, split == TRUE)
test = subset(df.final, split == FALSE)
str(train)

#loading neural network library
library(neuralnet)

#training
nn <- neuralnet(br ~ a + b + c + d + e + f + g, data = train, hidden = 5, linear.output = FALSE)
predicted.nn.values <- compute(nn, test[,1:7])

#rounding predicted values to either 1 or 0
predictions <- sapply(predicted.nn.values$net.result, round)

#testing results
table(predictions,test$br)

