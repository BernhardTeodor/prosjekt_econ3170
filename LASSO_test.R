library(tidyverse)
library(tidymodels)

titanic <- read_csv("Titanic-Dataset.csv") |>
  select(c(Age, Survived, Sex, Fare, SibSp, Pclass)) |>
  mutate(across(where(is.character), as.factor)) |>
  mutate(Survived = as.factor(Survived)) |>
  filter(!is.na(Age),
         !is.na(Fare),
         !is.na(SibSp))

split <- titanic |>
  initial_split(prop = .8, strata = Pclass)

titanic.train <- training(split)
titanic.test <- testing(split)

titanic.rec <- recipe(Survived~., data = titanic.train) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_predictors())


LASSO_model <- logistic_reg()|>
  set_args(penalty = tune(),
           mixture = 1) |>
  set_engine("glmnet") |>
  set_mode("classification")

wf <- workflow()|>
  add_recipe(titanic.rec) |>
  add_model(LASSO_model)

LASSO_grid <- grid_regular(
  penalty(range = c(-4,1)),
  levels = 50
)

cv <- vfold_cv(titanic.train, v = 10)

tune_results <- tune_grid(
  wf,
  resamples = cv,
  grid = LASSO_grid,
  control = control_grid(save_pred = TRUE)
)

tune_results |>
  collect_metrics() |> 
  filter(.metric == "accuracy") |>
  arrange(mean)

best_params <- tune_results |>
  select_best(metric = "accuracy")

fn.LASSO_wf <- wf |>
  finalize_workflow(best_params)

fn.LASSO_model <- fn.LASSO_wf |>
  fit(data = titanic.train)

pred <- fn.LASSO_model |>
  predict(new_data = titanic.test)

Error <- (as.numeric(pred$.pred_class)-as.numeric(titanic.test$Survived))^2 |>
  mean()

Error
