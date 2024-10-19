library(tidyverse)
library(tidymodels)

titanic <- read_csv("Titanic-Dataset.csv") |>
  select(c(Age, Survived, Sex, Fare, SibSp)) |>
  mutate(across(where(is.character), as.factor)) |>
  mutate(Survived = as.factor(Survived)) |>
  filter(!is.na(Age),
         !is.na(Fare),
         !is.na(SibSp))

split <- titanic |>
  initial_split(prop = .9)

titanic.train <- training(split)
titanic.test <- testing(split)

titanic.rec <- recipe(Survived~., data = titanic.train) |>
  step_dummy(all_nominal_predictors())

wf <- workflow()|>
  add_recipe(titanic.rec)

LASSO_model <- logistic_reg()|>
  set_args(penalty = .1, mixture = 1) |>
  set_engine("glmnet") |>
  set_mode("classification")

LASSO.fit <- wf |>
  add_model(LASSO_model) |>
  fit(data = titanic.train)

LASSO.prediction <- predict(LASSO.fit, titanic.test) 

prediksjoner <- as.numeric(LASSO.prediction$.pred_class)
prediksjoner

Error <- (prediksjoner - as.numeric(titanic.test$Survived))^2 |>
  mean() |>
  sqrt()
          
          