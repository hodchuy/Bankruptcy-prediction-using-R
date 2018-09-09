df.t2a <- read.csv("t-2 Altman.csv")

#scaling variables
scale.t2a <- scale(df.t2a[1:4])
var(scale.t2a[,1])
df.final.t2a <- cbind(scale.t2a,df.t2a[5])

#loading split library
library(caTools)
set.seed(101)

split.t2a = sample.split(df.final.t2a$br, SplitRatio = 0.7)

train.t2a = subset(df.final.t2a, split.t2a == TRUE)
test.t2a = subset(df.final.t2a, split.t2a == FALSE)

#training the data set
library(rpart)
tree.t2a <- rpart(br ~.,method='class',data = train.t2a)
predict.t2a <- predict (tree.t2a,test.t2a)

head(predict.t2a)
predict.t2a <- as.data.frame(predict.t2a)

joiner.t2a <- function(x){
  if (x>=0.5){
    return('1')
  }else{
    return("0")
  }
}
#
predict.t2a$br <- sapply(predict.t2a$"1",joiner.t2a)
#
head(predict.t2a)
#
table(predict.t2a$br,test.t2a$br)

