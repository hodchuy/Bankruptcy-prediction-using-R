df.t2a <- read.csv("t-2 Altman.csv")
# split data
library(caTools)
set.seed(101)
sample.t2a <- sample.split(df.t2a$br, 0.7)
train.t2a <- subset(df.t2a, sample.t1a == T)
test.t2a <- subset(df.t2a, sample.t1a == F)

# create model and test predictions
library(e1071)
model.t2a <- svm(br ~ ., data=train.t2a)
summary(model.t2a)

#Predict with the model
predict.t2a <- predict(model.t2a,test.t2a[1:4])
predict.t2a.round <- sapply(predict.t2a, round)
table(predict.t2a.round,test.t2a$br)

#tuning the function
tune.t2a <- tune(svm,train.x=df.t2a[1:4],train.y=df.t2a[,5],kernel='radial',
                 ranges=list(cost=c(1,2,3,4,5,6,7,8,9,10,11,12,13), 
                             gamma=c(.01,.02,.03,.04,.05,.1,.15,.2,.5,1)))
summary(tune.t2a)

# best tune gamma and cost
#gamma = 0.1
#cost = 3

#tuning the model again
tuned.svm.t2a <- svm(br ~ ., data=train.t1a, kernel="radial", cost=3, gamma=0.1)
summary(tuned.svm.t2a)

#predict with tuned model
tuned.predict.t2a <- predict(tuned.svm.t2a,test.t2a[1:4])
tuned.predict.t2a.round <- sapply(tuned.predict.t2a,round)
table(tuned.predict.t2a.round,test.t2a[,5])
