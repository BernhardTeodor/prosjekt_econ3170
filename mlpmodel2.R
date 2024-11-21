library(tidyverse)
library(tidymodels)
#Laste inn datasettet, og fjerne og manipulere kolonner
titanic <- read_csv("Titanic-Dataset.csv") |> 
  mutate(Pclass = as.factor(Pclass),
         Survived = as.factor(Survived))


#Lager behandling av NAs
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


#Lager en dummyvariabel for om vedkommende er over eller under 18, og legger til antall personer per bilett
titanic <- titanic |> 
  mutate(is.minor = ifelse(Age < 18, 1, 0),
         fam.size = SibSp+Parch,
         is.alone = ifelse(fam.size == 0, 1, 0)) |> 
  add_count(Ticket, name = "pers.pr.ticket")


#Fjerner varibaler med Strings, og eliminerer NA-verider i embarked
titanic <- titanic |> 
  select(-c(Name, PassengerId, Cabin, Ticket)) |> 
  filter(!is.na(Embarked))


#Deler datasetet inn i trenings- og testsett
set.seed(3170)
titanic.split <- titanic |> 
  initial_split(prop = .8, strata = Survived)

titanic.train <- training(titanic.split)
titanic.test <- testing(titanic.split)


#Deler datasettet inn i mapper for kryssvalidering

cv <- vfold_cv(titanic.train, v = 10, strata = Survived)


#Lager en recipe for å forberede dataene    
rec <- recipe(Survived ~ ., data = titanic.train) |>
  step_impute_mean(all_numeric_predictors()) |> 
  step_dummy(Embarked, Pclass, Sex) |> 
  step_normalize(Age, Fare, pers.pr.ticket, fam.size, Parch) |> 
  step_zv(all_numeric_predictors()) 


mlp.model <- mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) |> 
  set_engine("nnet") |> 
  set_mode("classification")

mlp_wflow <- workflow() |> 
  add_model(mlp.model) |> 
  add_recipe(rec)

mlp_params <- extract_parameter_set_dials(mlp_wflow)
print(mlp_params)

meterics <- metric_set(roc_auc, accuracy, brier_class)

grid <- grid_latin_hypercube(
  mlp_params,
  size = 100
)

mlp_tune <- tune_grid(
  mlp_wflow, 
  resamples = cv, 
  grid = grid,
  metrics = meterics,
  control = control_grid(save_pred = TRUE)
)
logistic_param.reg <- select_best(mlp_tune, metric = "brier_class") |>
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

rec |>
  prep() |> 
  bake(new_data = NULL) |> 
  View()