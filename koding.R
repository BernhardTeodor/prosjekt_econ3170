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
  step_dummy(Sex, Embarked, Pclass, alone)


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


penalty_grid <- grid_regular(penalty(), levels = 10)

folds <- vfold_cv(titanic_train, 10, strata = Survived)


lasso_tune <- 
  wflow_lasso |> 
  tune_grid(resamples = folds, grid = penalty_grid)

beste_lamda <- select_best(lasso_tune, metric = "roc_auc")



lasso_fit <- finalize_workflow(wflow_lasso, beste_lamda) |> 
  fit(titanic_train)





###########################################
#henter prediksjoner




predict(lasso_fit, titanic_test)|> 
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
  roc_cure(Survived,.pred_0) |> 
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







hypotetisk <- tibble(
  Pclass = as.factor(2),
  Sex = "female",
  Age = 2,
  Fare = 2,
  Embarked = "S",
  family_size = 20,
  alone = as.factor(0)
)

predict(lasso_fit, hypotetisk, type = "prob") |> 
  mutate(Survived = ifelse(.pred_1 > .pred_0, 1, 0)) |> 
  bind_cols(hypotetisk)



lam <- 
  logistic_reg( mixture = 1) |> 
  set_engine("glmnet") |> 
  set_mode("classification")

lmw <- workflow() |> 
  add_model(lam) |> 
  add_recipe(titanic_recipe)

lf <- lmw |> 
  fit(data = titanic_train)

predict(lf, new_data = titanic_test) |> 
  bind_cols(titanic_test) |> 
  accuracy(truth = Survived, estimate = .pred_class) |> 
  pull(.estimate)


lasso_tune |> 
  collect_metrics() |> 
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_errorbar(aes(
    ymin = mean - std_err,
    ymax = mean + std_err
  ),
  alpha = 0.5
  ) +
  geom_line(size = 1.5) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  scale_x_log10() +
  theme(legend.position = "none")



set_engine("ranger", importance = "permutation")


rf_mod <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) |> 
  set_engine("ranger", importance = "permutation") |>  # For å kunne bruke Vipp
  set_mode("classification")


rf_rec <- recipe(Survived~., data = titanic_train)

rf_wflow <- workflow() |> 
  add_recipe(rf_rec) |> 
  add_model(rf_mod)


rf_grid <- grid_regular(
  mtry(range= c(2, 7)),
  min_n(range= c(2,7)),
  trees(range = c(100,1000))
  levels = 5
)

tune_rf <- tune_grid(
  rf_wflow,
  resamples = folds,
  grid = rf_grid
)



tune_rf |> 
  collect_metrics() |> 
  filter(.metric == "roc_auc") |> 
  mutate(min_n = factor(min_n)) |> 
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC") +
  facet_wrap(~trees, scales = "free", nrow = 2)


tune_rf |> 
  collect_metrics() |> 
  filter(.metric == "accuracy") |> 
  mutate(min_n = factor(min_n)) |> 
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC") +
  facet_wrap(~trees, scales = "free", nrow = 2)




beste_mtry_min <- select_best(tune_rf, metric = "accuarcy")



rf_fit <- finalize_workflow(rf_wflow, beste_mtry_min) |> 
  fit(titanic_train)

predict(rf_fit, titanic_test) |> 
  bind_cols(titanic_test) |> 
  accuracy(Survived, .pred_class)


predict(rf_fit, titanic_test) |> 
  bind_cols(titanic_test) |> 
  conf_mat(truth = Survived, estimate = .pred_class)

predict(rf_fit, titanic_test, type = "prob") |> 
  bind_cols(titanic_test) |> 
  roc_auc(Survived, .pred_0)


rf_roc_curve <- predict(rf_fit, titanic_test, type = "prob") |> 
  bind_cols(titanic_test) |> 
  roc_curve(Survived, .pred_0) |> 
  mutate(type = "rf_roc")


lasso_roc_curve <- predict(lasso_fit, titanic_test, type = "prob") |> 
  bind_cols(titanic_test) |> 
  roc_curve(Survived, .pred_0) |> 
  mutate(type = "lasso_roc")



test <- lasso_roc_curve |> 
  bind_rows(rf_roc_curve)



test |> 
  ggplot(aes(x = (1 - specificity), y = sensitivity, col = type)) + 
  geom_path() +
  geom_abline(slope = 1, linetype = "dashed") +
  coord_equal()+
  theme_bw() +
  ggtitle("ROC-Kurve")

rf_fit |> 
  extract_fit_parsnip() |> 
  vip::vip()


test$fit |> 
  vip::vi()




set.seed(3170)
rf_mod <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) |> 
  set_engine("ranger") |>  # For å kunne bruke Vipp
  set_mode("classification")


rf_rec <- recipe(Survived~., data = titanic_train)

rf_wflow <- workflow() |> 
  add_recipe(rf_rec) |> 
  add_model(rf_mod)


rf_grid <- grid_regular(
  mtry(range= c(2, 7)),
  min_n(range= c(2,7)),
  trees(range = c(100,1000)),
  levels = 5
)

tune_rf <- tune_grid(
  rf_wflow,
  resamples = folds,
  grid = rf_grid
)


tune_rf |> 
  collect_metrics() |> 
  View()

select_best(tune_rf, metric = "roc_auc")

rf_fit <- finalize_workflow(rf_wflow, select_best(tune_rf, metric = "roc_auc")) |> 
  last_fit(split)


## visual

library(rpart)
library(rpart.plot)
final_tree <- extract_workflow(rf_fit)

final_tree |> 
  extract_fit_engine() |> 
  rpart.plot(roundint = FALSE)

## VIsual

tune_rf |> 
  collect_metrics() |> 
  filter(.metric == "roc_auc" ) |> 
  group_by(trees, mtry)|>
  mutate(mtry = as.factor(mtry)) |> 
  summarise(avg_auc = mean(mean))|> 
  ggplot(aes(x = trees, y = avg_auc, col = mtry))  +
  geom_line() +
  geom_point()+
  labs(title = "ROC AUC vs. Number of Trees",
       x = "Number of Trees",
       y = "ROC AUC") +
  theme_minimal()


tune_rf |> 
  collect_metrics() |> 
  filter(.metric == "roc_auc" ) |> 
  group_by(trees, min_n, mtry)|>
  mutate(min_n = as.factor(min_n)) |> 
  summarise(avg_auc = (mean))|> 
  ggplot(aes(x = trees, y = avg_auc, col = min_n))  +
  geom_line() +
  geom_point()+
  labs(title = "ROC AUC vs. Number of Trees",
       x = "Number of Trees",
       y = "ROC AUC") +
  theme_minimal() +
  facet_wrap(vars(mtry))

select_best(tune_rf, metric = "roc_auc")


tb <- boost_tree(
  trees = tune()
) |> 
  set_engine("xgboost") |> 
  set_mode("classification")


tbw <- workflow() |> 
  add_model(tb) |> 
  add_recipe(lasso_recipe)

tbgrid <- grid_regular(
  trees(range = c(50, 500)),
  levels = 5
)

tune_tb <- tune_grid(
  tbw,
  resamples = folds,
  grid = tbgrid
)
set.seed(3170)
tb_fit <- finalize_workflow(tbw, select_best(tune_tb, metric = "accuracy")) |> 
  last_fit(split)


tb_fit |> 
  collect_metrics()

tb_fit

predict(tb_fit, titanic_test, type = "prob") |> 
  bind_cols(titanic_test)

predict(lasso_fit, titanic_test) |> 
  bind_cols(titanic_test) |> 
  accuracy(Survived, .pred_class)

predict(lasso_fit, titanic_train) |> 
  bind_cols(titanic_train) |> 
  accuracy(Survived, .pred_class)

