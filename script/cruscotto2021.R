library(tidyverse)
library(readxl)
library(here)
library(writexl)

ob2021 <- read_excel("PROGRAMMAZIONE/performances/Estrazione_Operativo_Strategico_2020_2021.xlsx", 
                                                        sheet = "Operativo 2021")


ob2021 %>% 
  select(MacroArea, ObiettivoStrategico, Indicatore, StrutturaAssegnataria, Valore, Periodo) %>% 
  filter(Periodo == 4) %>% View()
  pivot_wider(names_from = "StrutturaAssegnataria", values_from = Valore) %>% View()
  

# ob2021 %>% 
#   select(StrutturaAssegnataria) %>% 
#   mutate(StrutturaAssegnataria = factor(StrutturaAssegnataria)) %>% 
#   unique() %>% 
#   write.table(., file = "strutture.csv")


# datiSB %>% 
#   select(Dipartimento, Reparto, Struttura) %>% 
#   unique() %>%
#   write_xlsx(path = "strutture2.xlsx")
  
#tabella di conversione strutture
strutture <- read_excel( here("PROGRAMMAZIONE", "performances", "tabella conversione strutture.xlsx"))

strutture$StrutturaAssegnataria <- gsub("\\d+", "", strutture$StrutturaAssegnataria)

strutture$StrutturaAssegnataria <- gsub("\"", "",  strutture$StrutturaAssegnataria)
strutture$StrutturaAssegnataria <- str_trim(strutture$StrutturaAssegnataria)

ob2021 %>% 
  left_join(strutture, by = "StrutturaAssegnataria") %>% View()


