library(tidyverse)
library(tidymodels)

titanic <- read_csv("Titanic-Dataset.csv")


mean_age_master <- titanic |> 
  filter(grepl("Master", Name)) |> 
  summarise(mean(Age, na.rm = T)) |> 
  pull()


titanic <- titanic |> 
  mutate(Age = ifelse(is.na(Age) & (grepl("Master", Name)), 
                      yes = round(mean_age_master,0), 
                      no = Age))





mean_age_menn_p <- rep(0,3)
mean_age_kvinne_p <- rep(0,3) 
for (i in 1:3)
{
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


tilfeldig_embarked <- sample(c("C", "S"), size = 1)

titanic <- titanic |> 
  mutate(Embarked = ifelse(is.na(Embarked),
                           yes = tilfeldig_embarked,
                           no = Embarked))



titanic <- titanic |> 
  select(-Cabin, -Name, -Ticket)

titanic <- titanic |> 
  select(-PassengerId)


titanic <- titanic |> 
  mutate(family_size = SibSp + Parch + 1,
         alone = ifelse(family_size == 1, 1, 0))
tit2 <- titanic

titanic <- titanic |> 
  mutate(Pclass = as.factor(Pclass),
         Survived = as.factor(Survived),
         alone = as.factor(alone))




titanic <- titanic |> 
  select(-Parch, -SibSp)


set.seed(3170)
split <- initial_split(titanic, prop = 0.8)
titanic_train <- training(split)
titanic_test <- testing(split)



## recepie ##

titanic_recipe <- 
  recipe(Survived ~ ., data = titanic_train) |> 
  step_dummy(Sex, Embarked, Pclass, alone) |> 
  step_normalize(Age, Fare)


View(titanic_recipe |> prep() |> bake(new_data= NULL))

lasso_model <- 
  logistic_reg(penalty = tune(), mixture = 1) |> 
  set_engine("glmnet") |> 
  set_mode("classification")


wflow_lasso <- 
  workflow() |> 
  add_recipe(titanic_recipe) |> 
  add_model(lasso_model)


penalty_grid <- grid_regular(penalty(range = c(-4, -1)), levels = 50)

folds <- vfold_cv(titanic_train, 10, strata = Survived)


lasso_tune <- 
  wflow_lasso |> 
  tune_grid(resamples = folds, grid = penalty_grid)

beste_lamda <- select_best(lasso_tune, metric = "roc_auc")




lasso_fit <- finalize_workflow(wflow_lasso, beste_lamda) |> 
  fit(data = titanic_train)


###########################################
#henter prediksjoner

predict(lasso_fit, titanic_test) |> 
  bind_cols(titanic_test)

predict(lasso_fit, titanic_test) |> 
  bind_cols(titanic_test) |> 
  metrics(truth = Survived, estimate = .pred_class) 


predict(lasso_fit, titanic_test) |> 
  bind_cols(titanic_test) |> 
  accuracy(truth = Survived, estimate = .pred_class)

predict(lasso_fit, titanic_test) |> 
  bind_cols(titanic_test) |> 
  conf_mat(truth = Survived, estimate = .pred_class)


predict(lasso_fit, titanic_test, type = "prob") |>
  bind_cols(predict(lasso_fit, titanic_test)) |> 
  bind_cols(titanic_test) |> 
  filter(Sex == "female" & .pred_class == 0) |> 
  View()



predict(lasso_fit, titanic_test, type = "prob") |>
  bind_cols(titanic_test) |> 
  roc_curve(Survived,.pred_0) |> 
  autoplot()


lasso_fit |> 
  extract_fit_parsnip() |> 
  vip::vip()

.########################################################




tit2 |> 
  group_by(family_size) |> 
  summarise(andel = sum(Survived)/n()) |> 
  ggplot(aes(x = family_size, y = andel)) +
  geom_col()



tit2 |> 
  group_by(alone, Pclass, Sex) |> 
  summarise(andel = sum(Survived)/n()) |> 
  ggplot(aes(x = alone, y = andel)) +
  geom_col() +
  facet_wrap(vars(Pclass, Sex))






























