df.t3a <- read.csv("t-3 Altman.csv")

#scaling variables
scale.t3a <- scale(df.t3a[1:4])
var(scale.t3a[,1])
df.final.t3a <- cbind(scale.t3a,df.t3a[5])

#loading split library
library(caTools)
set.seed(101)

split.t3a = sample.split(df.final.t3a$br, SplitRatio = 0.7)

train.t3a = subset(df.final.t3a, split.t3a == TRUE)
test.t3a = subset(df.final.t3a, split.t3a == FALSE)

#training the data set
library(rpart)
tree.t3a <- rpart(br ~.,method='class',data = train.t3a)
predict.t3a <- predict (tree.t3a,test.t3a)

head(predict.t3a)
predict.t3a <- as.data.frame(predict.t3a)

joiner.t3a <- function(x){
  if (x>=0.5){
    return('1')
  }else{
    return("0")
  }
}
#
predict.t3a$br <- sapply(predict.t3a$"1",joiner.t3a)
#
head(predict.t3a)
#
table(predict.t3a$br,test.t3a$br)

