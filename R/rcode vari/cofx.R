library(tidyverse)
library(here)
library(readxl)

library(gt)

 

pubblicazioni <- read_excel(here("data", "raw", "pubblicazioni.xlsx"))
pubblicazioni$AU <- str_to_upper(pubblicazioni$AU)
pubblicazioni$AU <- str_remove(pubblicazioni$AU, " ")
pubblicazioni$AU <- gsub("_", " ", pubblicazioni$AU)
pubblicazioni$Nome <- str_extract( pubblicazioni$AU, ",.*$")
pubblicazioni$Nome <- str_remove(pubblicazioni$Nome, ",")
pubblicazioni$Nome <- gsub("\\s.*$", "", pubblicazioni$Nome)
pubblicazioni$Cognome <- gsub(",.*$", "", pubblicazioni$AU)



pub <- pubblicazioni %>% 
  mutate(articoliif = ifelse(Congr == "IF ; Int" | Congr == "IF",  "IF", NA), 
         INT = ifelse(Congr == "IF ; Int" | Congr == "Int",  "Int", NA ), 
         NAZ = ifelse(Congr == "Naz", "Naz", NA), 
         Oth = ifelse(Congr == "Others" , "Others", NA), 
         IF = as.numeric(IF))  






