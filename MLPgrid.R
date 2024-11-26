library(tidymodels)
tidymodels_prefer()

mlp_spec <- 
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) |> 
  set_engine("nnet", trace = 0) |> 
  set_mode("classification")

mlp_params <- extract_parameter_set_dials(mlp_spec)
mlp_params |> extract_parameter_dials("hidden_units")
mlp_params |> extract_parameter_dials("penalty")
mlp_params |> extract_parameter_dials("epochs")


crossing(hidden_units = 1:3,
         penalty = c(0.0,0.1),
         epochs = c(100,200)
         )

mlp_params |> 
  grid_regular(levels = 2)

mlp_params |> 
  grid_regular(leder = c(hidden_units = 3, penalty = 2, epochs = 2)) 

mlp_params |> 
  grid_random(size = 1000)
