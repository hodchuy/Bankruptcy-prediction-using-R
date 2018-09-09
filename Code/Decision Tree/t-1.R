df.t1 <- read.csv("t-1.csv")

#scaling variables
scale.t1 <- scale(df.t1[1:7])
var(scale.t1[,1])
df.final.t1 <- cbind(scale.t1,df.t1[8])

#loading split library
library(caTools)
set.seed(101)
split.t1 = sample.split(df.final.t1$br, SplitRatio = 0.7)
train.t1 = subset(df.final.t1, split.t1 == TRUE)
test.t1 = subset(df.final.t1, split.t1 == FALSE)

#training the data set
library(rpart)
tree.t1 <- rpart(br ~.,method='class',data = train.t1)
predict.t1 <- predict (tree.t1,test.t1)

head(predict.t1)
predict.t1 <- as.data.frame(predict.t1)
                            
joiner.t1 <- function(x){
  if (x>=0.5){
    return('1')
  }else{
    return("0")
  }
}
#
predict.t1$br <- sapply(predict.t1$"1",joiner.t1)
#
head(predict.t1)
#
table(predict.t1$br,test.t1$br)
