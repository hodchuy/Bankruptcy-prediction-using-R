df.t1a <- read.csv("t-1 Altman.csv")
# split data
library(caTools)
set.seed(101)
sample.t1a <- sample.split(df.t1a$br, 0.7)
train.t1a <- subset(df.t1a, sample.t1a == T)
test.t1a <- subset(df.t1a, sample.t1a == F)

# create model and test predictions
library(e1071)
model.t1a <- svm(br ~ ., data=train.t1a)
summary(model.t1a)

#Predict with the model
predict.t1a <- predict(model.t1a,test.t1a[1:4])
predict.t1a.round <- sapply(predict.t1a, round)
table(predict.t1a.round,test.t1a$br)

#tuning the function
tune.t1a <- tune(svm,train.x=df.t1a[1:4],train.y=df.t1a[,5],kernel='radial',
                ranges=list(cost=c(1,2,3,4,5,6,7,8,9,10,11,12,13), 
                            gamma=c(.01,.02,.03,.04,.05,.1,.15,.2,.5,1)))
summary(tune.t1a)

# best tune gamma and cost
#gamma = 0.05
#cost = 11

#tuning the model again
tuned.svm.t1a <- svm(br ~ ., data=train.t1a, kernel="radial", cost=11, gamma=0.05)
summary(tuned.svm.t1a)

#predict with tuned model
tuned.predict.t1a <- predict(tuned.svm.t1a,test.t1a[1:4])
tuned.predict.t1a.round <- sapply(tuned.predict.t1a,round)
table(tuned.predict.t1a.round,test.t1a[,5])
