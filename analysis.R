str(data)
table(data$event)
names(data)
data[65,]
data[2,]
tds = getNodeSet(pbp[[303]], "td")


b = data[data$event=="SHOT" | data$event=="GOAL",]
head(b)$description
str(b)
names(b)
m = gregexpr("\\s([0-9]+)\\s", b$description)
shotDistance = regmatches(b$description, m)
shotDistance = as.numeric(shotDistance)
shotDistance[265]
isGoal = b$event=="GOAL"
isGoal =isGoal*1
isGoal
isPowerPlay = b$strength == "PP"
isPowerPlay

isPowerPlay = b$strength == "PP"
isPowerPlay
isPowerPlay = b$strength == "PP"
isPowerPlay


df = data.frame(cbind(shotDistance, isGoal, isPowerPlay))
str(df)
table(df$isGoal)

### support Vector
library(e1071)
svm_fit<-svm(isGoal ~ . , data=df, cross=10)
summary(svm_fit)

svmpredict <-predict(svm_fit, newdata=df)

mod1 = glm(isGoal ~ ., data = df, family=binomial)
summary(mod1)
predict = predict(mod1, data=df, type="response")

# Test set AUC 
library(ROCR)
ROCRpred = prediction(predict, df$isGoal)
as.numeric(performance(ROCRpred, "auc")@y.values)

table(df$isGoal, predict > 0.9)

max(predict)

library(randomForest)
goalForest = randomForest(isGoal ~ . , data = df, ntree=200, nodesize=25)
# Make predictions
PredictForest = predict(goalForest, newdata = df)
table(df$isGoal, PredictForest)


# this is an individual stat row in the play by play table
# if you parse all the rows then you have all the data
x = pbpRows[[1]]

xmlSApply(x, xmlValue)
