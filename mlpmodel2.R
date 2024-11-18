library(tidyverse)
library(tidymodels)

titanic <- read_csv("Titanic-Dataset.csv") 
titanic <- titanic[,-1]
titanic <- titanic |> 
  select(-c(Cabin, Ticket, Name)) |>
  filter(!is.na(Embarked)) |> 
  mutate(Pclass = as.factor(Pclass),
         Survived = as.factor(Survived))

set.seed(3170)
titanic.split <- titanic |> 
  initial_split(prop = .8)

titanic.train <- training(titanic.split)
titanic.test <- testing(titanic.split)

cv <- vfold_cv(titanic.train)

rec <- recipe(Survived ~ ., data = titanic.train) |>
  step_impute_median(Age) |>
  step_dummy(Embarked, Pclass, Sex) 

mlp.model <- mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) |> 
  set_engine("nnet") |> 
  set_mode("classification")

mlp_wflow <- workflow() |> 
  add_model(mlp.model) |> 
  add_recipe(rec)

mlp_params <- extract_parameter_set_dials(mlp_wflow)

print(mlp_params)

roc_res <- metric_set(roc_auc)



mlp_tune <- tune_grid(
  mlp_wflow, 
  resamples = cv, 
  grid = mlp_params |> grid_random(size=100),
  metrics = roc_res
)


logistic_param.reg <- select_best(mlp_tune, metric = "roc_auc") |>
  select(-.config)

fn.mlp_wflow <- mlp_wflow |> 
  finalize_workflow(logistic_param.reg)

fn.mlp_fit <- fn.mlp_wflow |> 
  fit(titanic.train)

mlp_pred <- fn.mlp_fit |> 
  predict(new_data = titanic.test)

Error <- (as.numeric(mlp_pred$.pred_class)-as.numeric(titanic.test$Survived))^2 |> 
  mean()
Error
