df.t3 <- read.csv("t-3.csv")

#scaling variables
scale.t3 <- scale(df.t3[1:7])
var(scale.t3[,1])
df.final.t3 <- cbind(scale.t3,df.t3[8])

#loading split library
library(caTools)
set.seed(101)
split.t3 = sample.split(df.final.t3$br, SplitRatio = 0.7)
train.t3 = subset(df.final.t3, split.t3 == TRUE)
test.t3 = subset(df.final.t3, split.t3 == FALSE)

#training the data set
library(rpart)
tree.t3 <- rpart(br ~.,method='class',data = train.t3)
predict.t3 <- predict (tree.t3,test.t3)

head(predict.t3)
predict.t3 <- as.data.frame(predict.t3)

joiner.t3 <- function(x){
  if (x>=0.5){
    return('1')
  }else{
    return("0")
  }
}
#
predict.t3$br <- sapply(predict.t3$"1",joiner.t3)
#
head(predict.t3)
#
table(predict.t3$br,test.t3$br)
