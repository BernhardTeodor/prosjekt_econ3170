library(tidyverse)
library(tidymodels)
library(xgboost)

###Laste inn datasett
titanic <- read_csv("Titanic-Dataset.csv")

titanic <- titanic |> 
  select(c(Age, Survived, Sex, Fare, SibSp, Pclass)) |>
  mutate(across(where(is.character), as.factor)) |>
  mutate(Survived = as.factor(Survived),
         Pclass = as.factor(Pclass)) 


set.seed(3170)
split <- initial_split(titanic, prop = .8, strata = Survived)
titanic_train <- training(split)
titanic_test <- testing(split)

titanic.rec <- recipe(Survived~., data = titanic_train) |>
  step_dummy(Pclass, Sex) |> 
  step_normalize(Age, Fare) |>
  prep()

data_train <- bake(titanic.rec, new_data = NULL)
data_test <- bake(titanic.rec, new_data = titanic_test)

####Fjerner variablen vi ønsker å predikere fra matrisen
train.matrix <- as.matrix(select(data_train, -Survived))
test.matrix <- as.matrix(select(data_test, -Survived))

#####Endre til numeriske variabler
train_label <- as.numeric(pull(data_train, Survived)) - 1
test_label <- as.numeric(pull(data_test, Survived)) - 1

dtrain <- xgb.DMatrix(data = train.matrix, label= train_label)
dtest <- xgb.DMatrix(data = test.matrix, label= test_label)


model <- xgboost(data = dtrain, 
                 nround = 2, 
                 objective = "binary:logistic")


params <- list(
  booster = "gbtree", 
  objective = "binary:logistic", 
  eta = 0.1, 
  max_depth = 6, 
  subsample = 0.8,
  colsample_bytree = 0.8
)

watchlist <- list(train = dtrain, eval = dtest)

xgb_model <- xgb.train(
  params = params, 
  data = dtrain, 
  nrounds = 100, 
  watchlist = watchlist, 
  early_stopping_rounds = 10, 
  print_every_n = 10
)

pred <- predict(xgb_model, newdata = dtest)

### Convert predictions to binary class (0 or 1)
pred_class <- ifelse(pred > 0.5, 1, 0)

error <- (pred_class-(as.numeric(titanic_test$Survived)-1))^2 |> 
  mean()
error
