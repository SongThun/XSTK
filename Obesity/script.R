install.packages(c('dplyr', 'ggplot2','corrplot'))
install.packages('caret')
library('dplyr')
library('ggplot2')
library('corrplot')
library('caret')

df <- read.csv('obesity_level.csv')
summary(df)

unique(df$CAEC)
unique(df$CALC)
unique(df$MTRANS)
unique(df$X0be1dad)

df <- df %>%
  mutate(CAEC_freq=case_when(
    CAEC == 'Always' ~ 1,
    CAEC == 'Frequently' ~ 0.75,
    CAEC == 'Sometimes' ~ 0.5,
    CAEC == '0' ~ 0
  )) %>%
  mutate(CALC_freq=case_when(
    CALC == 'Frequently' ~ 1,
    CALC == 'Sometimes' ~ 0.5,
    CALC == '0' ~ 0
  )) %>%
  mutate(labels = factor(X0be1dad, 
                         levels=c("Insufficient_Weight", "0rmal_Weight", "Overweight_Level_I", "Overweight_Level_II",
                                  "Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"))) %>%
  mutate(male=ifelse(Gender=="Male", 1, 0)) %>%
  select(-Age, -CAEC, -CALC, -MTRANS, -X0be1dad, -Gender, -Height, -Weight)

summary(df)



install.packages('randomForest')
library('randomForest')

model <- randomForest(labels ~ ., data = df, ntree = 100)
summary(model)

predictions <- predict(model, df)

confusionMatrix(predictions, df$labels)




#install.packages('nnet')
#library('nnet')
#help(multinom)
#model <- multinom(labels ~ ., data = df)

# Predict on test data
#predictions <- predict(model, newdata = df, type = "class")

# Evaluate the model
#predict(predictions, df)


