df.t2 <- read.csv("t-2.csv")
# split data
library(caTools)
set.seed(101)
sample.t2 <- sample.split(df.t2$br, 0.7)
train.t2 <- subset(df.t2, sample.t2 == T)
test.t2 <- subset(df.t2, sample.t2 == F)

# create model and test predictions
library(e1071)
model.t2 <- svm(br ~ ., data=train.t2)
summary(model.t2)

#Predict with the model
predict.t2 <- predict(model.t2,test.t2[1:7])
predict.t2.round <- sapply(predict.t2, round)
table(predict.t2.round,test.t2$br)

#tuning the function
tune.t2 <- tune(svm,train.x=df.t2[1:7],train.y=df.t2[,8],kernel='radial',
                ranges=list(cost=c(1,2,3,4,5,6,7,8,9,10,11,12,13), 
                            gamma=c(.01,.02,.03,.04,.05,.1,.15,.2,.5,1)))
summary(tune.t2)

# best tune gamma and cost
#gamma = 0.1
#cost = 3

#tuning the model again
tuned.svm.t2 <- svm(br ~ ., data=train.t2, kernel="radial", cost=3, gamma=0.1)
summary(tuned.svm.t2)

#predict with tuned model
tuned.predict.t2 <- predict(tuned.svm.t2,test.t2[1:7])
tuned.predict.t2.round <- sapply(tuned.predict.t2,round)
table(tuned.predict.t2.round,test.t2[,8])
