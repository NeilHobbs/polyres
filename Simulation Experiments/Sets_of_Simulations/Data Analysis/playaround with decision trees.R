#playaround with decisions trees

library(caret)       # meta engine for decision tree application
library(vip)         # for feature importance
library(pdp)         # for feature effects

temp.df = outcome.df.set.4%>%
  dplyr::select("Heritability",
                "Male.Insecticide.Exposure",
                "Female.Insecticide.Exposure",
                "Fitness.Cost",
                "Intervention.Coverage",
                "Dispersal",
                "cross.resistance",
                "operational.outcome")
## set the seed to make your partition reproducible; using todays date
## 75% of the sample size
smp_size <- floor(0.75 * nrow(temp.df))

set.seed(1205)
train_ind <- sample(seq_len(nrow(temp.df)), size = smp_size)

data_train <- temp.df[train_ind, ]
data_test <- temp.df[-train_ind, ]


fit <- rpart(operational.outcome~., data = data_train, method = 'class')

predict_unseen <-predict(fit, data_test, type = 'class')

table_mat <- table(data_test$operational.outcome, predict_unseen)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, data_test, type = 'class')
  table_mat <- table(data_test$operational.outcome, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}

control <- 

tune_fit <- rpart(operational.outcome~., 
                  data = data_train, 
                  method = 'class', 
                  control = rpart.control(minsplit = 525,
                         maxdepth = 1,
                         cp = 0))

accuracy_tune(tune_fit)

rpart.plot(tune_fit,
           type = 0,
           tweak = 1,
           extra = 100,
           box.palette = list("#3690c0", "#ffff33", "#b2df8a", "#f03b20"))


final.model = rpart(operational.outcome~., 
                    data = temp.df, 
                    method = 'class', 
                    control = rpart.control(minsplit = 525,
                                            maxdepth = 4,
                                            cp = 0))

rpart.plot(final.model,
           type = 0,
           tweak = 1.1,
           extra = 100,
           box.palette = list("#3690c0", "#ffff33", "#b2df8a", "#f03b20"))
