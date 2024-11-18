library(tidyverse)
library(tidymodels)
tidymodels_prefer()
set.seed(3170)

titanic <- read_csv("Titanic-Dataset.csv") |>
  select(-Cabin)


mean_age_master <- titanic |> 
  filter(grepl("Master", Name)) |> 
  summarise(mean_age = mean(Age, na.rm = T)) |> 
  pull(mean_age)


titanic <- titanic |> 
  mutate(Age = ifelse(is.na(Age) & (grepl("Master", Name)), 
                      yes = round(mean_age_master,0), 
                      no = Age))


mean_age_menn_p <- rep(0,3)
mean_age_kvinne_p <- rep(0,3) 
for (i in 1:3){
  mean_age_menn_p[i] <- titanic |> 
    filter(Pclass == i &  Sex == "male") |> 
    summarise(round(mean(Age, na.rm = T),0)) |> 
    pull()
  
  mean_age_kvinne_p[i] <- titanic|> 
    filter(Pclass == i &  Sex == "female") |> 
    summarise(round(mean(Age, na.rm = T),0)) |> 
    pull()
}

for (i in 1:3)
{
  titanic <- titanic |> 
    mutate(Age = ifelse(is.na(Age) & Sex == "male" & Pclass == i, 
                        mean_age_menn_p[i], 
                        Age)) |> 
    mutate(Age = ifelse(is.na(Age) & Sex == "female" & Pclass == i, 
                        mean_age_kvinne_p[i], 
                        Age)) 
}

titanic |> 
  filter(Sex == "female" & Pclass == 1) |> 
  group_by(Embarked, Pclass) |>
  summarise(mean_survived = mean(Survived), antall = n())


tilfeldig_embarked <- sample(c("C", "S"), size = 1)
titanic <- titanic |> 
  mutate(Embarked = ifelse(is.na(Embarked),
                           yes = tilfeldig_embarked,
                           no = Embarked))

titanic <- titanic |> 
  select(c(Age, Survived, Sex, Fare, SibSp, Pclass, Embarked)) |>
  mutate(across(where(is.character), as.factor)) |>
  mutate(Survived = as.factor(Survived),
         Pclass = as.factor(Pclass)) 

titanic.split <- initial_split(titanic, prop = .8, strata = Survived)
titanic.train <- training(titanic.split)
titanic.test <- testing(titanic.split)

titanic_folds <- vfold_cv(titanic.train)

mlp_spec <- 
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) |> 
  set_engine("nnet", trace = 0) |> 
  set_mode("classification")

mlp_rec <-
  recipe(Survived ~., data = titanic.train) |> 
  step_dummy(Pclass, Embarked, Sex) |>
  step_normalize(Age, Fare) |> 
  step_pca(all_predictors()) 


wf <- 
  workflow() |> 
  add_model(mlp_spec) |>
  add_recipe(mlp_rec)

mlp_params <- 
  wf |> 
  extract_parameter_set_dials() |> 
  update(
    epochs = epochs(c(50,200)),
    )

roc_res <- metric_set(roc_auc)

mlp_reg_tune <- 
  wf |> 
  tune_grid(
    titanic_folds,
    grid = mlp_params |> grid_regular(levels = 3),
    metrics = roc_res
  )

mlp_sfd_tune <-
  wf |> 
  tune_grid(
    titanic_folds,
    grid = 20,
    param_info = mlp_params,
    metrics = roc_res)

logistic_param.reg <- select_best(mlp_reg_tune, metric = "roc_auc") |>
  select(-.config)


logistic_param.sfd <- select_best(mlp_sfd_tune, metric = "roc_auc") |>
  select(- .config)

final_wf.reg <- 
  wf |> 
  finalize_workflow(logistic_param.reg)

final_wf.sfd <- 
  wf  |> 
  finalize_workflow(logistic_param.sfd)

final.mlp.reg_fit <- 
  final_wf.reg |> 
  fit(titanic.train)

final.mlp.sfd_fit <- 
  final_wf.sfd |>
  fit(titanic.train)

pred.reg <- final.mlp.reg_fit |>
  predict(new_data = titanic.test)

pred.sfd <- final.mlp.sfd_fit |>
  predict(new_data = titanic.test)


Error.reg <- (as.numeric(pred.reg$.pred_class)-as.numeric(titanic.test$Survived))^2 |>
  mean()
Error.reg

Error.sfd <- (as.numeric(pred.sfd$.pred_class)-(as.numeric(titanic.test$Survived)))^2|>
  mean()
Error.sfd
