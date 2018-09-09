df.t2 <- read.csv("t-2.csv")

#scaling variables
scale.t2 <- scale(df.t2[1:7])
var(scale.t2[,1])
df.final.t2 <- cbind(scale.t2,df.t2[8])

#loading split library
library(caTools)
set.seed(101)
split.t2 = sample.split(df.final.t2$br, SplitRatio = 0.7)
train.t2 = subset(df.final.t2, split.t2 == TRUE)
test.t2 = subset(df.final.t2, split.t2 == FALSE)

#training the data set
library(rpart)
tree.t2 <- rpart(br ~.,method='class',data = train.t2)
predict.t2 <- predict (tree.t2,test.t2)

head(predict.t2)
predict.t2 <- as.data.frame(predict.t2)

joiner.t2 <- function(x){
  if (x>=0.5){
    return('1')
  }else{
    return("0")
  }
}
#
predict.t2$br <- sapply(predict.t2$"1",joiner.t2)
#
head(predict.t2)
#
table(predict.t2$br,test.t2$br)
