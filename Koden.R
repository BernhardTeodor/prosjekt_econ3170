library(tidyverse) 
library(tidymodels) 
library(cvms)
library(rpart)
library(rpart.plot)
library(NeuralNetTools)
library(patchwork)

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
#Sjekker om det er fler manglende verider
titanic |> 
  filter(is.na(Age))

#Finner gjennomsnittet for tittel gitt klasse
mean.dr.p1 <- titanic |> 
  filter(grepl("Dr.", Name, fixed = TRUE) & Pclass == 1) |> 
  summarise(round(mean(Age, na.rm = TRUE))) |> 
  pull()

#Setter inn for verdien
titanic <- titanic |> 
  mutate(Age = ifelse(is.na(Age), mean.dr.p1, Age))

#Sjekker antal manglende verdier for Embarked
titanic |> 
  filter(is.na(Embarked))

#Finner Embarked for kvinner i Pclass 1
titanic |> 
  filter(Sex == "female" & Pclass == 1) |> 
  group_by(Embarked, Pclass) |>
  summarise(mean_survived = mean(Survived), antall = n())

tilfeldig_embarked <- sample(c("C", "S"), size = 1)


#Fyller manglende verdier for Embarked basert på funnene over
titanic <- titanic |> 
  mutate(Embarked = ifelse(is.na(Embarked),
                           yes = tilfeldig_embarked,
                           no = Embarked))

#Finner antallet overlevne og omkommne
titanic |> 
  group_by(Survived) |> 
  summarise(Antall = n())

#Finner overlevelsesandeler
titanic |> 
  group_by(Pclass) |> 
  summarise(Overlevelsesandel = sum(Survived)/n())
  
#Plot over andel, klasser og kjønn
titanic |> 
  group_by(Pclass,Sex) |> 
  summarise(Overlevelsesandel = sum(Survived)/n()) |> 
  ggplot(aes(x = Pclass, y = Overlevelsesandel, fill = Sex)) +
  geom_col(position = position_dodge()) +
  ylab("Andel som overlevde") +
  xlab("Klasse") +
  ggtitle("Figur 1.1 - Fordeling over andel, klasser og kjønn") +
  scale_fill_brewer(palette = "Set1")

#Plot over aldershistogram delt på overlevelse
titanic |> 
  ggplot(aes(x = Age, fill = Sex)) +
  geom_histogram() +
  facet_wrap(vars(Survived)) +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Figur 1.2 - Aldershistogram for overlevelse") +
  xlab("Alder") +
  ylab("Antall personer")

#Plot for aldersgrupper og kjønn
titanic |> 
  mutate(
    Agegroup = cut(
      Age, 
      breaks = c(-Inf,20, 30, 40, 50, 60, Inf), 
      labels = c("0-19", "20-29", "30-39", "40-49", "50-59", "60+"))) |> 
  group_by(Agegroup,Sex) |> 
  summarise(Andel = sum(Survived)/n())|>
  ggplot(aes(x = Agegroup, y = Andel, fill = Sex)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set1") +  
  xlab("Aldersgrupper") +
  ylab("Andel overlevd") +
  ggtitle("Figur 1.3 Andelen overlevne i ulike aldersgrupper, fordelt på kjønn")

#Plot for Fare gruppe og overlevelse
titanic |> 
  filter(Fare < 200) |> 
  mutate(Fare_group = ntile(Fare, 6)) |>
  mutate(Fare_group = factor(Fare_group, labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6"))) |> 
  group_by(Fare_group, Sex) |> 
  summarise(andel = sum(Survived)/n()) |> 
  ggplot(aes(x = Fare_group, y = andel, fill = Sex)) + 
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  xlab("Fare gruppe") +
  ylab("Overlevelsesandel") +
  ggtitle("Figur 2.1 - Overlevelsesandel for Fare gruppe delt på kjønn")

#Plot for Fare gruppe og Pclass
titanic |> 
  group_by(Embarked, Pclass) |> 
  summarise(andel_overlevde = sum(Survived)/ n()) |> 
  ggplot(aes(x = Pclass, y = andel_overlevde, fill = Embarked )) +
  geom_col(position = position_dodge()) +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Figur 2.2 - Overlevelsesandel for Embarked fordelt på Pclass") +
  xlab("Klasse") +
  ylab("Overlevelsesandel")

#Oppretter nye variabler
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

#Deler inn i treningsdata og testdata
set.seed(3170) #Setter seed for å få de samme dataene
titanic_split<- initial_split(titanic, prop = .8, strata = Survived)
titanic_train <- training(titanic_split)
titanic_test <- testing(titanic_split)
titanic_split

#Lager en recipe, og definerer modell
titanic_recipe <- 
  recipe(Survived ~ ., data = titanic_train) |>
  step_dummy(Sex, Embarked, Alone, Pclass, Minor)

lasso_model <- 
  logistic_reg(penalty = tune(), mixture = 1) |> 
  set_engine("glmnet") |> 
  set_mode("classification")

#Definerer en workflow, setter opp en grid til tuning, og plotter beste lambda
wflow_lasso <- 
  workflow() |> 
  add_recipe(titanic_recipe) |> 
  add_model(lasso_model)

folds <- vfold_cv(titanic_train, 5, strata = Survived)

penalty_grid <- grid_regular(penalty(range = c(-4, -1)), levels = 100)

doParallel::registerDoParallel() # Parallelprogramering for raskere tune

lasso_tune <- 
  wflow_lasso |> 
  tune_grid(resamples = folds, grid = penalty_grid)

lasso_tune |>
  collect_metrics() |> 
  filter(.metric != "brier_class") |> 
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_errorbar(aes(
    ymin = mean - std_err,
    ymax = mean + std_err
  )) +
  geom_line(size = 1.5) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  scale_x_log10() +
  theme(legend.position = "none") +
  ggtitle("Modellens prestasjon for ulike verdier av lambda")+
  xlab("Straff(lambda)") +
  ylab("Gjennomsnit")

#Henter ut de beste lambdaveridene, og trener modellen med disse parameterne
beste_lamda <- select_best(lasso_tune, metric = "roc_auc")
beste_lamda

lasso_fit <- finalize_workflow(wflow_lasso, beste_lamda) |> 
  fit(data = titanic_train)

#Kjører prediksjoner og finnner ROC og AUC verider
predict(lasso_fit, titanic_test) |> 
  bind_cols(titanic_test) |> 
  accuracy(truth = Survived, estimate = .pred_class)

auc_lasso <- 
  predict(lasso_fit, titanic_test, type = "prob") |> 
  bind_cols(titanic_test) |> 
  roc_auc(Survived, .pred_0) |> 
  pull(.estimate)

lasso_roc <- 
  predict(lasso_fit, titanic_test, type = "prob") |> 
  bind_cols(titanic_test) |> 
  roc_curve(Survived, .pred_0)

#Finner de viktigste variablene for modellen
lasso_fit |> 
  extract_fit_parsnip() |> 
  vip::vi() |> 
  ggplot(aes(y = reorder(Variable, Importance), 
             x = Importance, fill = Sign)) +
  geom_col() +
  theme() + 
  ggtitle("Viktigheten av ulike variabler") +
  xlab("Betydning") +
  ylab("Variabler")

#Konstruerer forivrringsmatrise
prediksjoner_lasso <- predict(lasso_fit, titanic_test) |> 
  bind_cols(titanic_test)

forvirringsmatrise_lasso <- prediksjoner_lasso |> 
  conf_mat(Survived, .pred_class)

riktigmatrise_lasso <- as.tibble(forvirringsmatrise_lasso$table) |> 
  mutate(Prediction = ifelse(Prediction == 0, "Not survived", "Survived")) |> 
  mutate(Truth = ifelse(Truth == 0, "Not survived", "Survived"))

plot_confusion_matrix(riktigmatrise_lasso, "Prediction", "Truth", "n", 
                      add_normalized = F,
                      add_row_percentages = F,
                      add_col_percentages = F) +
  xlab("Faktisk") +
  ylab("Predikert") +
  ggtitle("Forvirringsmatrise Lasso")

#Bruker samme recipe, og følger samme fremgangsmåte for tilpassning av hyperparametere og trening av modell

rf_mod <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) |> 
  set_engine("ranger", importance = "impurity") |>  
  # For å kunne bruke Vip pakken
  set_mode("classification")



rf_wflow <- workflow() |> 
  add_recipe(titanic_recipe) |> 
  add_model(rf_mod)


rf_grid <- grid_regular(
  mtry(range = c(2, 7)),
  min_n(range = c(2,7)),
  trees(range = c(100,1200)),
  levels = 5
)


doParallel::registerDoParallel()

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
  facet_wrap(vars(trees))

beste_mtry_min_tree <- select_best(tune_rf, metric = "roc_auc")

rf_fit <- finalize_workflow(rf_wflow, beste_mtry_min_tree) |> 
  fit(titanic_train)


#Finner ROC og AUC for prediksjonen
auc_rf <- 
  predict(rf_fit, titanic_test, type = "prob") |> 
  bind_cols(titanic_test) |> 
  roc_auc(Survived, .pred_0) |> 
  pull(.estimate)

rf_roc <- predict(rf_fit, titanic_test, type = "prob") |> 
  bind_cols(titanic_test) |> 
  roc_curve(Survived, .pred_0)

#Finner variablene som er viktige for denne modellen
rf_fit |> 
  extract_fit_parsnip() |> 
  vip::vi() |> 
  ggplot(aes(y = reorder(Variable, Importance), x = Importance, fill = "#F8766D")) +
  geom_col() +
  theme(legend.position = "none") + 
  ggtitle("Viktigheten av ulike variabler") +
  xlab("Betydning") +
  ylab("Variabler")

#Konstuerer en forrvirringsmatrise
prediksjoner_rf <- predict(rf_fit, titanic_test) |> 
  bind_cols(titanic_test)

forvirringsmatrise_rf <- prediksjoner_rf |> 
  conf_mat(Survived, .pred_class)

riktigmatrise_rf <- as.tibble(forvirringsmatrise_rf$table) |> 
  mutate(Prediction = ifelse(Prediction == 0, "Not survived", "Survived")) |> 
  mutate(Truth = ifelse(Truth == 0, "Not survived", "Survived"))

plot_confusion_matrix(riktigmatrise_rf, "Prediction", "Truth", "n", 
                      add_normalized = F,
                      add_row_percentages = F,
                      add_col_percentages = F) +
  xlab("Faktisk") +
  ylab("Predikert") +
  ggtitle("Forvirringsmatrise Random Forest")

#Følger samme prosedyre for decicion tree som i de andre modellene, med unntak av range på parametere

decision_model <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()) |> 
  set_engine("rpart") |> 
  set_mode("classification")



decision_wflow <- workflow() |> 
  add_recipe(titanic_recipe) |> 
  add_model(decision_model)

decision_tree_params <- parameters(decision_model) |> 
  update(
    cost_complexity = cost_complexity(),
    tree_depth = tree_depth(),
    min_n = min_n()
  )

decision_grid <- grid_regular(
  decision_tree_params,
  levels = 5
)

doParallel::registerDoParallel()

tune_decision <- tune_grid(
  decision_wflow,
  resamples = folds,
  grid = decision_grid,
)

tune_decision |> 
  collect_metrics() |> 
  filter(.metric == "roc_auc") |> 
  mutate(min_n = factor(min_n)) |> 
  ggplot(aes(cost_complexity, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC") +
  facet_wrap(vars(tree_depth))

#Finner beste parametere, implementerer de i modellen, og kjører prediksjoner
best_cost_depth_min <- select_best(tune_decision, metric = "roc_auc")
best_cost_depth_min

decision_fit <- finalize_workflow(decision_wflow, best_cost_depth_min) |> 
  fit(titanic_train)


predict(decision_fit, titanic_test) |> 
  bind_cols(titanic_test) |> 
  accuracy(Survived, .pred_class)


#Finner ROC og AUC for prediksjonen
auc_decision <- 
  predict(decision_fit, titanic_test, type = "prob") |> 
  bind_cols(titanic_test) |> 
  roc_auc(Survived, .pred_0) |> 
  pull(.estimate)

decision_roc <- predict(decision_fit, titanic_test, type = "prob") |> 
  bind_cols(titanic_test) |> 
  roc_curve(Survived, .pred_0) 

#For å bruke rpart-funksjonen må vi trene modellen på en annen måte.
#Vi kunne ha brukt denne metodene med last_fit gjennom hele, men vi syntes det var greiere å kun jobbe med fit,
#isteden for å bruke last_fit

decision_final_fit <- finalize_workflow(decision_wflow, best_cost_depth_min) |> 
  last_fit(titanic_split)

final_tree <- extract_workflow(decision_final_fit)

final_tree |> 
  extract_fit_engine() |> 
  rpart.plot(roundint = FALSE)

#Finner viktigheten av ulike variabler
decision_fit |> 
  extract_fit_parsnip() |> 
  vip::vi() |> 
  ggplot(aes(y = reorder(Variable, Importance), x = Importance, fill = "#F8766D")) +
  geom_col() +
  theme(legend.position = "none") + 
  ggtitle("Viktigheten av ulike variabler decision tree") +
  xlab("Betydning") +
  ylab("Variabler")

#Konstruerer en forvirringsmatrise
prediksjoner_decision <- predict(decision_fit, titanic_test) |> 
  bind_cols(titanic_test)

forvirringsmatrise_decision <- prediksjoner_decision |> 
  conf_mat(Survived, .pred_class)

riktigmatrise_decision <- as.tibble(forvirringsmatrise_decision$table) |> 
  mutate(Prediction = ifelse(Prediction == 0, "Not survived", "Survived")) |> 
  mutate(Truth = ifelse(Truth == 0, "Not survived", "Survived"))

plot_confusion_matrix(riktigmatrise_decision, "Prediction", "Truth", "n", 
                      add_normalized = F,
                      add_row_percentages = F,
                      add_col_percentages = F) +
  xlab("Faktisk") +
  ylab("Predikert") +
  ggtitle("Forvirringsmatrise Decision tree")

#Definerer modellen, implementerer den i en workflow, 
#og definerer en grid ved hjelp av funksjonen grid_latin_hypercube
xg_model <- boost_tree(
  trees = 1000,
  tree_depth = tune(),
  learn_rate = tune(),
  mtry = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune()
)|> 
  set_engine("xgboost") |> 
  set_mode("classification")


xg_wflow <- workflow() |> 
  add_recipe(titanic_recipe) |> 
  add_model(xg_model)

xg_grid <- grid_latin_hypercube(
  tree_depth(),
  learn_rate(),
  finalize(mtry(), titanic_train),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  size = 50
)

doParallel::registerDoParallel()

tune_xg <- tune_grid(
  xg_wflow,
  resamples = folds,
  grid = xg_grid,
)

#Henter de beste parameterne fra tilpassningen og implementerer de i modellen, og kjører prediksjoner
best_param_xg <- select_best(tune_xg, metric = "roc_auc")
best_param_xg

xg_fit <- finalize_workflow(xg_wflow, best_param_xg) |> 
  fit(titanic_train)

predict(xg_fit, titanic_test) |> 
  bind_cols(titanic_test) |> 
  accuracy(Survived, .pred_class)

#Finnner ROC og AUC for prediksjonene
auc_xg <- 
  predict(xg_fit, titanic_test, type = "prob") |> 
  bind_cols(titanic_test) |> 
  roc_auc(Survived, .pred_0) |> 
  pull(.estimate)

xg_roc <- predict(xg_fit, titanic_test, type = "prob") |> 
  bind_cols(titanic_test) |> 
  roc_curve(Survived, .pred_0) 

#Finner hvilke varibaler som er viktige for modellen
xg_fit |> 
  extract_fit_parsnip() |> 
  vip::vi() |> 
  ggplot(aes(y = reorder(Variable, Importance), x = Importance, fill = "#F8766D")) +
  geom_col() +
  theme(legend.position = "none") + 
  ggtitle("Viktigheten av ulike variabler decision tree") +
  xlab("Betydning") +
  ylab("Variabler")

#Konstruerer en forvirringsmatrise
prediksjoner_xg <- predict(xg_fit, titanic_test) |> 
  bind_cols(titanic_test)

forvirringsmatrise_xg <- prediksjoner_xg |> 
  conf_mat(Survived, .pred_class)

riktigmatrise_xg <- as.tibble(forvirringsmatrise_xg$table) |> 
  mutate(Prediction = ifelse(Prediction == 0, "Not survived", "Survived")) |> 
  mutate(Truth = ifelse(Truth == 0, "Not survived", "Survived"))

plot_confusion_matrix(riktigmatrise_xg, "Prediction", "Truth", "n", 
                      add_normalized = F,
                      add_row_percentages = F,
                      add_col_percentages = F) +
  xlab("Faktisk") +
  ylab("Predikert") +
  ggtitle("forvirringsmatrise XGboost")


#Bruker lik metode som extreme-gradient boosting tree til å tilpasse parametere,
#trene, og kjøre prediksjoner
mlp.model <- mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) |> 
  set_engine("nnet") |> 
  set_mode("classification")

mlp_wflow <- workflow() |> 
  add_model(mlp.model) |> 
  add_recipe(titanic_recipe)


#Henter så ut hyperparameterne og implempenterer de i en grid ved hjelp av grid_latin_hypercube med 50 nivåer
mlp_params <- extract_parameter_set_dials(mlp_wflow)
meterics <- metric_set(roc_auc, accuracy, brier_class)

grid <- grid_latin_hypercube(
  mlp_params,
  size = 50
)

doParallel::registerDoParallel()

mlp_tune <- tune_grid(
  mlp_wflow, 
  resamples = folds, 
  grid = grid,
  metrics = meterics,
  control = control_grid(save_pred = TRUE)
)


#Henter deretter ut de beste parameterne, og tilpasser modellen
logistic_param.reg <- select_best(mlp_tune, metric = "roc_auc") |>
  select(-.config)

fn.mlp_wflow <- mlp_wflow |> 
  finalize_workflow(logistic_param.reg)

fn.mlp_fit <- fn.mlp_wflow |> 
  fit(titanic_train)


#Kjører prediksjoner, og plotter ROC-AUC.

auc_mlp <- 
  predict(fn.mlp_fit, titanic_test, type = "prob") |> 
  bind_cols(titanic_test) |> 
  roc_auc(Survived, .pred_0) |> 
  pull(.estimate)

mlp_roc <- predict(fn.mlp_fit, titanic_test, type = "prob") |> 
  bind_cols(titanic_test) |> 
  roc_curve(Survived, .pred_0) 

#Plotter viktige varibler
fn.mlp_fit |> 
  extract_fit_parsnip() |> 
  vip::vi() |> 
  ggplot(aes(y = reorder(Variable, Importance), x = Importance, fill = "#F8766D"))+
  geom_col() +
  theme(legend.position = "none") + 
  ggtitle("Viktigheten av ulike variabler Multi Layer percepton") +
  xlab("Betydning") +
  ylab("Variabler")

#Konstruerer en forvirringsmatrise 
prediksjoner_mlp <- predict(fn.mlp_fit, titanic_test) |> 
  bind_cols(titanic_test)

forvirringsmatrise_xg <- prediksjoner_xg |> 
  conf_mat(Survived, .pred_class)

riktigmatrise_xg <- as.tibble(forvirringsmatrise_xg$table) |> 
  mutate(Prediction = ifelse(Prediction == 0, "Not survived", "Survived")) |> 
  mutate(Truth = ifelse(Truth == 0, "Not survived", "Survived"))

plot_confusion_matrix(riktigmatrise_xg, "Prediction", "Truth", "n", 
                      add_normalized = F,
                      add_row_percentages = F,
                      add_col_percentages = F) +
  xlab("Faktisk") +
  ylab("Predikert") +
  ggtitle("Forvirringsmatrise Multi-Layer Percepton")


#Lager en funksjon for å lage plott
plot_roc_curve <- function(roc_data, auc_value, title) {
  roc_data |> 
    ggplot(aes(x = 1 - specificity, y = sensitivity)) + 
    geom_path(col = "blue") +
    geom_abline(slope = 1, linetype = "dashed") +
    coord_equal() +
    annotate("text", x = 0.6, y = 0.1, 
             label = paste("AUC =", round(auc_value, 3)), size = 5) +
    theme_bw() +
    ggtitle(title)
}

#Lager de individuelle plottene
lasso_plot <- plot_roc_curve(lasso_roc, auc_lasso, "LASSO")
rf_plot <- plot_roc_curve(rf_roc, auc_rf, "Random Forest")
decision_plot <- plot_roc_curve(decision_roc, auc_decision, "Decision Tree")
xg_plot <- plot_roc_curve(xg_roc, auc_xg, "XGBoost")
mlp_plot <- plot_roc_curve(mlp_roc, auc_mlp, "MLP")

#Viser plottene i samme figur
(lasso_plot | rf_plot | xg_plot) / (decision_plot | mlp_plot) 