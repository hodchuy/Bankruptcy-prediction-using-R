df.t1 <- read.csv("t-1.csv")
# split data
library(caTools)
set.seed(101)
sample.t1 <- sample.split(df.t1$br, 0.7)
train.t1 <- subset(df.t1, sample.t1 == T)
test.t1 <- subset(df.t1, sample.t1 == F)

# create model and test predictions
library(e1071)
model.t1 <- svm(br ~ ., data=train.t1)
summary(model.t1)

#Predict with the model
predict.t1 <- predict(model.t1,test.t1[1:7])
predict.t1.round <- sapply(predict.t1, round)
table(predict.t1.round,test.t1$br)

#tuning the function
tune.t1 <- tune(svm,train.x=df.t1[1:7],train.y=df.t1[,8],kernel='radial',
                     ranges=list(cost=c(1,2,3,4,5,6,7,8,9,10,11,12,13), 
                                 gamma=c(.01,.02,.03,.04,.05,.1,.15,.2,.5,1)))
summary(tune.t1)

# best tune gamma and cost
#gamma = 0.03
#cost = 7

#tuning the model again
tuned.svm.t1 <- svm(br ~ ., data=train.t1, kernel="radial", cost=7, gamma=0.03)
summary(tuned.svm.t1)

#predict with tuned model
tuned.predict.t1 <- predict(tuned.svm.t1,test.t1[1:7])
tuned.predict.t1.round <- sapply(tuned.predict.t1,round)
table(tuned.predict.t1.round,test.t1[,8])
