library(tidyverse)
library(tidymodels)


###Laste inn datasett
titanic <- read_csv("Titanic-Dataset.csv")

titanic <- titanic |> 
  select(-Cabin)

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

mann_p3 <- "Moran, Mr. James"
mann_p2 <- "Williams, Mr. Charles Eugene"
mann_p1 <- "Woolner, Mr. Hugh"

kvinne_p3 <- "Moran, Miss. Bertha"
kvinne_p1 <- "Thorne, Mrs. Gertrude Maybelle"
kvinne_p2 <- "Keane, Miss. Nora A"

navn_med_na <- c(mann_p1, mann_p2, mann_p3,
                 kvinne_p1, kvinne_p2, kvinne_p3)

aldere <- c(mean_age_menn_p, mean_age_kvinne_p)

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


for (i in 1:6)
{
  person_ald <- titanic |> 
    filter(Name == navn_med_na[i]) |> 
    pull(Age)
  
  stopifnot(person_ald == aldere[i])
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
select(c(Age, Survived, Sex, Fare, SibSp, Pclass)) |>
  mutate(across(where(is.character), as.factor)) |>
  mutate(Survived = as.factor(Survived),
         Pclass = as.factor(Pclass)) 


#Deler inn i trening og test

set.seed(1881)
split <- titanic |>
  initial_split(prop = .8, strata = Survived)

titanic.train <- training(split)
titanic.test <- testing(split)


titanic.rec <- recipe(Survived~., data = titanic.train) |>
  step_dummy(Pclass, Sex) |> 
  step_normalize(Age, Fare)


titanic.rec |> 
  prep() |>
  bake(new_data = NULL) |> 
  View()


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
cv <- vfold_cv(titanic.train, v = 25)


#Tilpasser parapmeterene ved hjelp av kryssvalidering
tune_results <- tune_grid(
  wf,
  resamples = cv,
  grid = LASSO_grid,
  control = control_grid(save_pred = TRUE)
)


##Finner de optimale parameter resultatene
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
