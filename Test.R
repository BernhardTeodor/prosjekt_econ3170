library(tidyverse)
library(tidymodels)
library(parsnip)

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

titanic <- titanic |> 
  mutate(is.minor = ifelse(Age < 18, 1, 0),
         fam.size = SibSp+Parch,
         is.alone = ifelse(fam.size == 0, 1, 0)) |> 
  add_count(Ticket, name = "pers.pr.ticket")

titanic <- titanic |> 
  select(-c(Name, PassengerId, Cabin, Ticket)) |> 
  filter(!is.na(Embarked))



### Definerer motor; LASSO model
LASSO_model <- logistic_reg()|>
  set_args(penalty = tune(),
           mixture = 1) |>
  set_engine("glmnet") |>
  set_mode("classification")

rec <- recipe(Survived ~ ., data = titanic.train) |>
  step_impute_mean(all_numeric_predictors()) |> 
  step_dummy(Embarked, Pclass, Sex)  |>
  step_normalize(Age, Fare, Parch)

##Lager en workflow
wf <- workflow()|>
  add_recipe(rec) |>
  add_model(LASSO_model)


###Definerer en grid for å tilpasse parametere
LASSO_params <- wf |> extract_parameter_set_dials(wf)

#Deler dataen inn i 10 mapper for kryssvalidering
cv <- vfold_cv(titanic.train, v = 10, strata = Survived)


#Tilpasser parameterene ved hjelp av kryssvalidering
tune_results <- tune_grid(
  wf,
  resamples = cv,
  grid = LASSO_params |> grid_random(size = 200) ,
  control = control_grid(save_pred = TRUE)
)


##Finner de optimale parameter resultatene
tune_results |>
  collect_metrics() |> 
  filter(.metric == "roc_auc") |>
  arrange(mean)

best_params <- tune_results |>
  select_best(metric = "roc_auc")

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




