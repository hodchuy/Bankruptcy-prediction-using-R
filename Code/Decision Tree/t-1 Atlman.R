df.t1a <- read.csv("t-1 Altman.csv")

#scaling variables
scale.t1a <- scale(df.t1a[1:4])
var(scale.t1a[,1])
df.final.t1a <- cbind(scale.t1a,df.t1a[5])

#loading split library
library(caTools)
set.seed(101)

split.t1a = sample.split(df.final.t1a$br, SplitRatio = 0.7)

train.t1a = subset(df.final.t1a, split.t1a == TRUE)
test.t1a = subset(df.final.t1a, split.t1a == FALSE)

#training the data set
library(rpart)
tree.t1a <- rpart(br ~.,method='class',data = train.t1a)
predict.t1a <- predict (tree.t1a,test.t1a)

head(predict.t1a)
predict.t1a <- as.data.frame(predict.t1a)

joiner.t1a <- function(x){
  if (x>=0.5){
    return('1')
  }else{
    return("0")
  }
}
#
predict.t1a$br <- sapply(predict.t1a$"1",joiner.t1a)
#
head(predict.t1a)
#
table(predict.t1a$br,test.t1a$br)

