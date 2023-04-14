library(tidyverse)
library(here)
library(readxl)
#library(gt)
library(openxlsx)

dati22 <- read_excel(here("data", "raw", "presenze2023.xlsx"))
CC <- readRDS(here("data", "processed", "CC.RDS"))

dati22 %>% 
  mutate(peror = as.numeric(`Perc Orario`), 
         ricercatore = ifelse(Profilo == "Collaboratore Professionale di Ricerca Sanitaria" |
                                Profilo == "Ricercatore Sanitario", "ricercatore", "altro")) %>%  
  filter(ricercatore == "altro") %>% 
  left_join(  
    (CC %>% 
       select(Dipartimento, Reparto, Laboratorio, CDC, CodiceCDC) %>% 
       unique() ) , by = c("CODICE_CDC" = "CodiceCDC")) %>%  
  
  mutate(Laboratorio = recode(Laboratorio, 
                              "LABORATORIO DI CONTROLLO DI PRODOTTI BIOLOGICI, FARMACEUTICI E CONVALIDA DI PROCESSI PRODUTTIVI" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO", 
                              "LABORATORIO PRODUZIONE TERRENI" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO", 
                              "LABORATORIO ANALISI GENOMICHE, LABORATORIO DIAGNOSTICA MOLECOLARE, OGM" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE", 
                              "LABORATORIO BATTERIOLOGIA SPECIALIZZATA" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE", 
                              "LABORATORIO DI PROTEOMICA E DIAGNOSTICA TSE" = "REPARTO VIROLOGIA", 
                              "LABORATORIO DI VIROLOGIA E SIEROLOGIA SPECIALIZZATA, MICROSCOPIA ELETTRONICA" = "REPARTO VIROLOGIA", 
                              "LABORATORIO CHIMICA APPLICATA ALLE TECNOLOGIE ALIMENTARI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI", 
                              "LABORATORIO CONTAMINANTI AMBIENTALI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI", 
                              "LABORATORIO MANGIMI E TOSSICOLOGIA" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI")) %>%  
  
  
  # mutate(contr  = ifelse( Dirigente == "N", (36*peror)/100, (38*peror)/100),
  #        hcontr =  contr*47.4) %>% 
  
  select(Dipartimento, Reparto, Laboratorio, Dirigente) %>%   
  group_by(Dipartimento, Reparto, Laboratorio, Dirigente) %>% 
    summarize(n = n()) %>%   
  pivot_wider(names_from = Dirigente, values_from = n) %>%
  rename( "Dirigenza" = S, 
          "Comparto" = N)  %>%  
  ungroup() %>% 
  write.xlsx(file = "teste.xlsx")
  
  # personale della ricerca----

dati22 %>% 
  mutate(peror = as.numeric(`Perc Orario`), 
         ricercatore = ifelse(Profilo == "Collaboratore Professionale di Ricerca Sanitaria" |
                                Profilo == "Ricercatore Sanitario", "ricercatore", "altro")) %>%  
  filter(ricercatore != "altro") %>%  
  left_join(  
    (CC %>% 
       select(Dipartimento, Reparto, Laboratorio, CDC, CodiceCDC) %>% 
       unique() ) , by = c("CODICE_CDC" = "CodiceCDC")) %>%  
  
  mutate(Laboratorio = recode(Laboratorio, 
                              "LABORATORIO DI CONTROLLO DI PRODOTTI BIOLOGICI, FARMACEUTICI E CONVALIDA DI PROCESSI PRODUTTIVI" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO", 
                              "LABORATORIO PRODUZIONE TERRENI" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO", 
                              "LABORATORIO ANALISI GENOMICHE, LABORATORIO DIAGNOSTICA MOLECOLARE, OGM" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE", 
                              "LABORATORIO BATTERIOLOGIA SPECIALIZZATA" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE", 
                              "LABORATORIO DI PROTEOMICA E DIAGNOSTICA TSE" = "REPARTO VIROLOGIA", 
                              "LABORATORIO DI VIROLOGIA E SIEROLOGIA SPECIALIZZATA, MICROSCOPIA ELETTRONICA" = "REPARTO VIROLOGIA", 
                              "LABORATORIO CHIMICA APPLICATA ALLE TECNOLOGIE ALIMENTARI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI", 
                              "LABORATORIO CONTAMINANTI AMBIENTALI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI", 
                              "LABORATORIO MANGIMI E TOSSICOLOGIA" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI")) %>%  
  
  
  
  select(Dipartimento, Reparto, Laboratorio ) %>%   
  group_by(Dipartimento, Reparto, Laboratorio ) %>% 
  summarize(n = n()) %>%    
  # pivot_wider(names_from = Dirigente, values_from = n) %>%
  # rename( "Dirigenza" = S, 
  #         "Comparto" = N)  %>%  
  # ungroup() %>% 
  write.xlsx(file = "testeRic.xlsx")


  
  
  
  
  # summarise(hcontr = sum(hcontr)) %>%  
  # mutate(FTE = ifelse(Dirigente == "S", hcontr/(38*47.4), hcontr/(36*47.4))) %>%  
  # select(-hcontr) %>% 
  # 
  # pivot_wider(names_from = Dirigente, values_from = FTE) %>%  
  # rename( "FTED" = S, 
  #         "FTEC" = N)  %>% 
  # ungroup() %>%   
  # 
  # gt() %>% 
  # gtsave("teste.rtf")


