install.packages('mlbench')
data(BreastCancer, package="mlbench")
bc <- BreastCancer[complete.cases(BreastCancer), ]

bc <- bc[,-1]

for(i in 1:9) {
  bc[, i] <- as.numeric(as.character(bc[, i]))
}

bc$Class <- ifelse(bc$Class == "malignant", 1, 0)
bc$Class <- factor(bc$Class, levels = c(0, 1))
install.packages('caret')
library(caret)
'%ni%' <- Negate('%in%')
options(scipen = 999)

set.seed(100)
trainDataIndex <- createDataPartition(bc$Class, p = 0.7, list = F)
trainData <- bc[trainDataIndex, ]
testData <- bc[-trainDataIndex, ]

table(trainData$Class)

set.seed(100)
down_train <- downSample(x = trainData[, colnames(trainData) %ni% "Class"],
                         y = trainData$Class)
table(down_train$Class)

up_train <- upSample(x = trainData[, colnames(trainData) %ni% "Class"],
                     y = trainData$Class)
table(up_train$Class)

logitmod <- glm(Class ~ Cl.thickness + Cell.size + Cell.shape, family = "binomial",
                data = down_train)

summary(logitmod)
pred <- predict(logitmod, newdata = testData, type = "response")
pred

y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels = c(0, 1))
y_act <- testData$Class

mean(y_pred == y_act)



