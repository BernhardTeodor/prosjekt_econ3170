library(tidyverse)
library(tidymodels)
library(xgboost)

###Laste inn datasett
titanic <- read_csv("Titanic-Dataset.csv") |> 
  mutate(Pclass = as.factor(Pclass),
         Survived = as.factor(Survived))

master.median <- list()
mr.median <- list()
mrs.median <- list()
miss.median <- list()

for (i in 1:3){
  for (j in c("Master", "Mr.", "Mrs.", "Miss")) {
    if (j == "Master"){
      master.median[[i]] <- titanic |> 
        filter(grepl("Master.", Name, fixed = TRUE), Pclass == i) |> 
        summarise(median_age = median(Age, na.rm = TRUE)) |> 
        pull(median_age)
    } else if(j == "Mr."){
      mr.median[[i]] <- titanic |> 
        filter(grepl("Mr.", Name, fixed = TRUE), Pclass == i) |> 
        summarise(median_age = median(Age, na.rm = TRUE)) |> 
        pull(median_age)
    } else if (j == "Mrs.") {
      mrs.median[[i]] <- titanic |> 
        filter(grepl("Mrs.", Name, fixed = TRUE), Pclass == i) |> 
        summarise(median_age = median(Age, na.rm = TRUE)) |> 
        pull(median_age)
    } else if(j == "Miss") {
      miss.median[[i]] <- titanic |> 
        filter(grepl("Miss.", Name, fixed = TRUE), Pclass == i) |> 
        summarise(median_age = median(Age, na.rm = TRUE)) |> 
        pull(median_age)
    }
  }
}

for (i in 1:3){
  titanic <- titanic |> 
    mutate(Age = ifelse(grepl("Mr.", Name, fixed = T) & Pclass == i & is.na(Age), mr.median[[i]], Age)) |> 
    mutate(Age = ifelse(grepl("Miss", Name, fixed = T) & Pclass == i & is.na(Age), miss.median[[i]], Age)) |> 
    mutate(Age = ifelse(grepl("Mrs.", Name, fixed = T) & Pclass == i & is.na(Age), mrs.median[[i]], Age)) |> 
    mutate(Age = ifelse(grepl("Master", Name, fixed = T) & Pclass == i & is.na(Age), master.median[[i]], Age))
}

titanic <- titanic |> 
  mutate(is.minor = ifelse(Age < 18, 1, 0),
         fam.size = SibSp+Parch,
         is.alone = ifelse(fam.size == 0, 1, 0)) |> 
  add_count(Ticket, name = "pers.pr.ticket")

titanic <- titanic |> 
  select(-c(Name, PassengerId, Cabin, Ticket)) |> 
  filter(!is.na(Embarked))


set.seed(3170)
split <- initial_split(titanic, prop = .8, strata = Survived)
titanic_train <- training(split)
titanic_test <- testing(split)
cv <- vfold_cv(titanic_train, v = 10, strata = Survived)

titanic.rec <- recipe(Survived~., data = titanic_train) |>
  step_impute_mean(Age) |> 
  step_dummy(Pclass, Sex, Embarked)

xgboost_model<- boost_tree(
  trees = 1000,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  learn_rate = tune(),
  mtry = tune()) |> 
  set_engine("xgboost") |> 
  set_mode("classification")

xgboost_wflow <- workflow() |> 
  add_recipe(titanic.rec) |> 
  add_model(xgboost_model)


grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_prop(),
  learn_rate(),
  finalize(mtry(), titanic_train),
  size = 100
)


xgb_tuned <- tune_grid(
  xgboost_wflow,
  resamples = cv, 
  grid = grid,
  control = control_grid(save_pred = TRUE),
  metrics = metric_set(accuracy, roc_auc, brier_class)
)

params <- select_best(xgb_tuned, metric = "brier_class")

fn.xgb_wflow <- xgboost_wflow |> 
  finalize_workflow(params)

fn.xgb_fit <- fn.xgb_wflow |> 
  fit(titanic_train)

xgboost_pred <- fn.xgb_fit |> 
  predict(new_data = titanic_test)

error <- (as.numeric(xgboost_pred$.pred_class)-(as.numeric(titanic_test$Survived))) |> 
  abs() |> 
  mean()
error



