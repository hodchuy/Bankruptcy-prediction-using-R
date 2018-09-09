df.t3a <- read.csv("t-3 Altman.csv")
# split data
library(caTools)
set.seed(101)
sample.t3a <- sample.split(df.t3a$br, 0.7)
train.t3a <- subset(df.t3a, sample.t3a == T)
test.t3a <- subset(df.t3a, sample.t3a == F)

# create model and test predictions
library(e1071)
model.t3a <- svm(br ~ ., data=train.t3a)
summary(model.t3a)

#Predict with the model
predict.t3a <- predict(model.t3a,test.t3a[1:4])
predict.t3a.round <- sapply(predict.t3a, round)
table(predict.t3a.round,test.t3a$br)

#tuning the function
tune.t3a <- tune(svm,train.x=df.t3a[1:4],train.y=df.t3a[,5],kernel='radial',
                 ranges=list(cost=c(1,2,3,4,5,6,7,8,9,10,11,12,13), 
                             gamma=c(.01,.02,.03,.04,.05,.1,.15,.2,.5,1)))
summary(tune.t3a)

# best tune gamma and cost
#gamma = 1
#cost = 13

#tuning the model again
tuned.svm.t3a <- svm(br ~ ., data=train.t3a, kernel="radial", cost=13, gamma=1)
summary(tuned.svm.t3a)

#predict with tuned model
tuned.predict.t3a <- predict(tuned.svm.t3a,test.t3a[1:4])
tuned.predict.t3a.round <- sapply(tuned.predict.t3a,round)
table(tuned.predict.t3a.round,test.t3a[,5])
