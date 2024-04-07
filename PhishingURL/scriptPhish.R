install.packages(c('dplyr', 'ggplot2', 'corrplot'))
install.packages('caret')
library('dplyr')
library('ggplot2')
library('corrplot')
library('caret')

## import dataset
df <- read.csv('dataset_cybersecurity_michelle.csv')
summary(df)


## select columns of url
df <- select(df, contains("url", ignore.case = TRUE), phishing)
summary(df)

## count the number of NA value
sum(is.na(df))

## function to plot histogram and boxplot of data
describe <- function(col_name) {
  layout(matrix(c(1,2), nrow=1))
  hist(df[[col_name]], main=col_name)
  boxplot(df[[col_name]], main=col_name)
}

for (col in names(df)) {
  describe(col)
}

corrplot(cor(df))
## logistic model
model <- glm(phishing ~ ., data=df, family='binomial')
summary(model)
step_model <- step(model, direction='both')

predicted_class <- ifelse(predict(model, df, type='response') >= 0.5, 1, 0)
confusionMatrix(factor(predicted_class), factor(df$phishing))

install.packages('rpart')
library('rpart')
dt <- rpart(phishing ~ ., data=df)
summary(dt)
dt_predict <- ifelse(predict(dt, df) >= 0.5, 1, 0)
confusionMatrix(factor(dt_predict), factor(df$phishing))

install.packages('randomForest')
library('randomForest')
rf <- randomForest(phishing ~ ., data=df, ntree=200)
