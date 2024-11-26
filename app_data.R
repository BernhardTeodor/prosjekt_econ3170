library(tidyverse)
library(tidymodels)

titanic <- read_csv("Titanic-Dataset.csv")
titanic <- titanic |> 
  select(-Cabin)


#Lager lister til å fylle inn verdiene
master.mean <- list() 
mr.mean <- list()
mrs.mean <- list()
miss.mean <- list()


#Setter opp en for-loop til å finne gjennomsnittet for alle med en gitt tittel, i en gitt klasse
for (i in 1:3){
  for (j in c("Master", "Mr.", "Mrs.", "Miss")) {
    if (j == "Master"){
      master.mean[[i]] <- titanic |> 
        filter(grepl("Master.", Name, fixed = TRUE), Pclass == i, Sex == "male") |> 
        summarise(mean_age = mean(Age, na.rm = TRUE)) |> 
        pull(mean_age)
    } else if(j == "Mr."){
      mr.mean[[i]] <- titanic |> 
        filter(grepl("Mr.", Name, fixed = TRUE), Pclass == i, Sex == "male") |> 
        summarise(mean_age = mean(Age, na.rm = TRUE)) |> 
        pull(mean_age)
    } else if (j == "Mrs.") {
      mrs.mean[[i]] <- titanic |> 
        filter(grepl("Mrs.", Name, fixed = TRUE), Pclass == i, Sex == "female") |> 
        summarise(mean_age = mean(Age, na.rm = TRUE)) |> 
        pull(mean_age)
    } else if(j == "Miss") {
      miss.mean[[i]] <- titanic |> 
        filter(grepl("Miss.", Name, fixed = TRUE), Pclass == i, Sex == "female") |> 
        summarise(mean_age = mean(Age, na.rm = TRUE)) |> 
        pull(mean_age)
    }
  }
}


#Fyller inn de manglende verdiene
for (i in 1:3){
  titanic <- titanic |> 
    mutate(Age = ifelse(grepl("Mr.", Name, fixed = T) & Pclass == i & is.na(Age), mr.mean[[i]], Age)) |> 
    mutate(Age = ifelse(grepl("Miss", Name, fixed = T) & Pclass == i & is.na(Age), miss.mean[[i]], Age)) |> 
    mutate(Age = ifelse(grepl("Mrs.", Name, fixed = T) & Pclass == i & is.na(Age), mrs.mean[[i]], Age)) |> 
    mutate(Age = ifelse(grepl("Master", Name, fixed = T) & Pclass == i & is.na(Age), master.mean[[i]], Age))
}


mean.dr.p1 <- titanic |> 
  filter(grepl("Dr.", Name, fixed = TRUE) & Pclass == 1) |> 
  summarise(round(mean(Age, na.rm = TRUE))) |> 
  pull()

#Setter inn for verdien
titanic <- titanic |> 
  mutate(Age = ifelse(is.na(Age), mean.dr.p1, Age))


tilfeldig_embarked <- sample(c("C", "S"), size = 1)

titanic <- titanic |> 
  mutate(Embarked = ifelse(is.na(Embarked),
                           yes = tilfeldig_embarked,
                           no = Embarked))

titanic <- titanic |> 
  add_count(Ticket, name = "Person_per_ticket") |> 
  mutate(Minor = ifelse(Age < 18, 1, 0),
         Family_size = SibSp+Parch,
         Alone = ifelse(Family_size == 0, 1, 0)) |> 
  select(-c(Name, Ticket, SibSp, Parch, PassengerId)) |> 
  mutate(Pclass = as.factor(Pclass),
         Alone = as.factor(Alone),
         Survived = as.factor(Survived),
         Minor = as.factor(Minor),
         Sex = as.factor(Sex),
         Embarked = as.factor(Embarked))

set.seed(3170)
titanic_split<- initial_split(titanic, prop = .8, strata = Survived)
titanic_train <- training(titanic_split)
titanic_test <- testing(titanic_split)
titanic_split


titanic_recipe <- 
  recipe(Survived ~ ., data = titanic_train) |>
  step_dummy(Sex, Embarked, Alone, Pclass, Minor)




lasso_model <- 
  logistic_reg(penalty = tune(), mixture = 1) |> 
  set_engine("glmnet") |> 
  set_mode("classification")


wflow_lasso <- 
  workflow() |> 
  add_recipe(titanic_recipe) |> 
  add_model(lasso_model)

folds <- vfold_cv(titanic_train, 5, strata = Survived)

penalty_grid <- grid_regular(penalty(range = c(-4, -1)), levels = 100)

doParallel::registerDoParallel() # Parallelprogramering for raskre tune

lasso_tune <- 
  wflow_lasso |> 
  tune_grid(resamples = folds, grid = penalty_grid)



beste_lamda <- select_best(lasso_tune, metric = "roc_auc")
beste_lamda

lasso_fit <- finalize_workflow(wflow_lasso, beste_lamda) |> 
  fit(data = titanic_train)

