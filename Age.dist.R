library(tidyverse)

Titanic <- read_csv("Titanic-Dataset.csv") 

age.group <- function(x){
  
if (is.na(x)) {
    group <- "NA"
  } else if (x <= 20) {
    group <- 1
  } else if(x %in% 21:40) {
    group <- 2
  } else if (x %in% 41:60) {
    group <- 3
  } else {
    group <- 4
  }
  return(group)
  
}

grupper <- NA

for (i in 1:length(Titanic$Age)) {
  grupper[i] <- age.group(Titanic$Age[i])
}

Titanic <- Titanic |>
  mutate(age.grp = grupper)

Titanic |>
  ggplot(aes(age.grp)) +
  geom_bar()
