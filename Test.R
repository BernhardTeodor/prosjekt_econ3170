library(tidyverse)
library(tidymodels)


###Laste inn datasett
titanic <- read_csv("Titanic-Dataset.csv") |>
  select(c(Age, Survived, Sex, Fare, SibSp, Pclass)) |>
  mutate(across(where(is.character), as.factor)) |>
  mutate(Survived = as.factor(Survived))


#Deler inn i trening og test
split <- titanic |>
  initial_split(prop = .8, strata = Survived)

titanic.train <- training(split)
titanic.test <- testing(split)


#Lager en recipe; Ønsker å predikere Survived ved hjelp av alle variabler
titanic.rec <- recipe(Survived~., data = titanic.train) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_predictors())


### Definerer motor; LASSO model
LASSO_model <- logistic_reg()|>
  set_args(penalty = tune(),
           mixture = 1) |>
  set_engine("glmnet") |>
  set_mode("classification")


##Lager en workflow
wf <- workflow()|>
  add_recipe(titanic.rec) |>
  add_model(LASSO_model)


###Definerer en grid for å tilpasse parametere
LASSO_grid <- grid_regular(
  penalty(range = c(-4,1)),
  levels = 50
)

#Deler dataen inn i 10 mapper for kryssvalidering
cv <- vfold_cv(titanic.train, v = 10)


#Tilpasser parapmeterene ved hjelp av kryssvalidering
tune_results <- tune_grid(
  wf,
  resamples = cv,
  grid = LASSO_grid,
  control = control_grid(save_pred = TRUE)
)


##Bruker de optimale parameter resultatene
tune_results |>
  collect_metrics() |> 
  filter(.metric == "accuracy") |>
  arrange(mean)

best_params <- tune_results |>
  select_best(metric = "accuracy")

###Bruker parameterne fra tilpassingen
fn.LASSO_wf <- wf |>
  finalize_workflow(best_params)


###Trener modellen på dataene
fn.LASSO_model <- fn.LASSO_wf |>
  fit(data = titanic.train)


###Kjører prediksjonene
pred <- fn.LASSO_model |>
  predict(new_data = titanic.test)


###Finner gjennomsnittlig feilmargin
Error <- (as.numeric(pred$.pred_class)-as.numeric(titanic.test$Survived))^2 |>
  mean()

Error