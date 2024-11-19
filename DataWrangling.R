library(tidyverse)


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


#Imputerer de manglende veridene
for (i in 1:3){
  titanic <- titanic |> 
    mutate(Age = ifelse(grepl("Mr.", Name, fixed = T) & Pclass == i & is.na(Age), mr.median[[i]], Age)) |> 
    mutate(Age = ifelse(grepl("Miss", Name, fixed = T) & Pclass == i & is.na(Age), miss.median[[i]], Age)) |> 
    mutate(Age = ifelse(grepl("Mrs.", Name, fixed = T) & Pclass == i & is.na(Age), mrs.median[[i]], Age)) |> 
    mutate(Age = ifelse(grepl("Master", Name, fixed = T) & Pclass == i & is.na(Age), master.median[[i]], Age))
}


#Lager en dummyvaribel for om noen er over eller under 18
titanic <- titanic |> 
  mutate(is.minor = ifelse(Age < 18, 1, 0),
         fam.size = SibSp+Parch,
         is.alone = ifelse(fam.size == 0, 1, 0)) 


#Finner ut om det er flere personer per bilett
unique(titanic$Ticket) |> length()
unique(titanic$PassengerId) |> length()

#
titanic <- titanic |> 
  add_count(Ticket, name = "pers.pr.ticket")




  
  
  