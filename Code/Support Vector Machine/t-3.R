df.t3 <- read.csv("t-3.csv")
# split data
library(caTools)
set.seed(101)
sample.t3 <- sample.split(df.t3$br, 0.7)
train.t3 <- subset(df.t3, sample.t3 == T)
test.t3 <- subset(df.t3, sample.t3 == F)

# create model and test predictions
library(e1071)
model.t3 <- svm(br ~ ., data=train.t3)
summary(model.t3)

#Predict with the model
predict.t3 <- predict(model.t3,test.t3[1:7])
predict.t3.round <- sapply(predict.t3, round)
table(predict.t3.round,test.t3$br)

#tuning the function
tune.t3 <- tune(svm,train.x=df.t3[1:7],train.y=df.t3[,8],kernel='radial',
                ranges=list(cost=c(1,2,3,4,5,6,7,8,9,10,11,12,13), 
                            gamma=c(.01,.02,.03,.04,.05,.1,.15,.2,.5,1)))
summary(tune.t3)

# best tune gamma and cost
#gamma = 1
#cost = 13

#tuning the model again
tuned.svm.t3 <- svm(br ~ ., data=train.t3, kernel="radial", cost=13, gamma=1)
summary(tuned.svm.t3)

#predict with tuned model
tuned.predict.t3 <- predict(tuned.svm.t3,test.t3[1:7])
tuned.predict.t3.round <- sapply(tuned.predict.t3,round)
table(tuned.predict.t3.round,test.t3[,8])
