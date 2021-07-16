library("tidyverse")
library("here")
library("readxl")

attività2019 <- read_excel(sheet = "riepilogo", here("programmazione", "NUOVA VERSIONE", "data", "raw", "attivita2019.xlsx"))
attività2020 <- read_excel(sheet = "Foglio1", here("programmazione", "NUOVA VERSIONE", "data", "raw", "attivita2020.xlsx"))

attività2019 %>% 
  group_by(Reparto) %>% 
  summarise(N.esami = sum(N.esami), 
            Ricavi = sum(Valore), 
            VP = sum(`Vendita Prodotti`, na.rm = TRUE),
            AI = sum(`Attività Interna`, na.rm = TRUE)) %>% 
  mutate(Anno = rep(2019, nrow(.))) %>% 
  
  bind_rows( 

attività2020 %>% 
  group_by(Reparto) %>% 
  summarise(N.esami = sum(n.esami), 
            Ricavi = sum(valore), 
            VP = sum(`vendita prodotti`, na.rm = TRUE),
            AI = sum(`attività interna`, na.rm = TRUE)) %>% 
  mutate(Anno = rep(2020, nrow(.)))

  )
