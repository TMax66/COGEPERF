library(tidyverse)
library(here)
library(readxl)
library(knitr)
library(kableExtra)



prj <- read_excel(sheet = "PRJ", here("data", "raw", "prj2020.xlsx"))



dtf1 <- c( "2017-01-01","2018-01-01","2019-01-01","2020-01-01", "2021-01-01")

dti <-  c("2017-12-31","2018-12-31","2019-12-31","2020-12-31","2021-12-31" )
anno <- seq(from = 2017, to = 2021, by=1)

x <- data.frame(dtf1, dti, anno)


prj_func <- function( dtf1, dti, anno)
{ prj %>%
    group_by( Tipologia) %>% 
    mutate("Stato" = ifelse(DataFine < as.Date(dtf1), "Archiviato", "Attivo")) %>% 
    filter(Stato == "Attivo" & DataInizio <= as.Date(dti)) %>%
    # mutate("Statoanno" = ifelse(DataFine <=as.Date(dtf1), "Concluso", "Aperto")) %>%
    # filter(Statoanno == "Aperto") %>% 
    summarise( 
      # "N.Progetti"=nlevels(factor(Codice)), 
      Budget = sum(Budget, na.rm = T)) %>% 
    mutate(anno = anno)
  
}


z <- list()

for (i in 1:5) { 
  z[[i]]<- prj_func(  dtf1 = x[i, 1], dti = x[i, 2], anno = x[i, 3])
  
}
progetti <- do.call(rbind, z)

progetti %>% 
  # pivot_wider(names_from = "Tipologia", values_from = "N.Progetti") %>% 
  pivot_wider(names_from = "Tipologia", values_from = "Budget") %>%  
  select(anno, Corrente, Finalizzato, Europeo, Regionali, Istituzionale, Autofinanziato, CCM, "Altro tipo" ) %>% 
  rowwise() %>% 
  #mutate(Competitivi = sum(Finalizzato, Europeo, Regionali, Istituzionale, CCM,`Altro tipo`)) %>% 
  mutate(Competitivi = sum(Finalizzato, Europeo, Regionali, Istituzionale,  CCM,`Altro tipo`, na.rm = T)) %>% 
  select(Anno = anno, "Ricerca Corrente" = Corrente, "Ricerca Competitiva" = Competitivi) %>% 
  knitr::kable(digits = 2, caption = "Progetti in corso") %>%
  kable_styling()  



dt1 <- c( "2017-01-01","2018-01-01","2019-01-01","2020-01-01", "2021-01-01")

dt2 <-  c("2017-12-31","2018-12-31","2019-12-31","2020-12-31","2021-12-31" )
anno <- seq(from = 2017, to = 2021, by=1)

x <- data.frame(dt1, dt2, anno)

prj_func2 <- function(  dt1, dt2, anno)
{  prj %>% 
    group_by( Tipologia) %>% 
    filter(DataInizio >= as.Date(dt1) & DataInizio <=as.Date(dt2)) %>% 
    summarise( 
      # "N.Progetti"=nlevels(factor(Codice)), 
      Budget = sum(Budget, na.rm = T)) %>% 
    mutate(anno = anno) 
  
}

z <- list()

for (i in 1:5) { 
  z[[i]]<- prj_func2(  dt1 = x[i, 1], dt2 = x[i, 2], anno = x[i, 3])
  
}
nprogetti <- do.call(rbind, z)


nprogetti %>% 
  # pivot_wider(names_from = "Tipologia", values_from = "N.Progetti") %>% 
  pivot_wider(names_from = "Tipologia", values_from = "Budget") %>%  
  select(anno, Corrente, Finalizzato, Europeo, Regionali, Istituzionale, Autofinanziato, CCM, "Altro tipo" ) %>% 
  rowwise() %>% 
  #mutate(Competitivi = sum(Finalizzato, Europeo, Regionali, Istituzionale, CCM,`Altro tipo`)) %>% 
  mutate(Competitivi = sum(Finalizzato, Europeo, Regionali, Istituzionale, CCM,`Altro tipo`, na.rm = T)) %>% 
  select(Anno = anno, "Ricerca Corrente" = Corrente, "Ricerca Competitiva" = Competitivi) %>% 
  knitr::kable(digits = 2, caption = "Nuovi Progetti") %>%
  kable_styling()  
 

