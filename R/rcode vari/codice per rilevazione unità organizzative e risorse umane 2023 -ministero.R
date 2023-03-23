dati23 %>% 
  mutate(peror = as.numeric(`Perc Orario`),
         peror = ifelse(Cognome == "VARISCO", 40, 
                        ifelse(Cognome == 'ARRIGONI', 40,
                               ifelse(Cognome == "FEDRIZZI", 40,
                                      ifelse(Cognome == "LAVAZZA", 50,
                                             ifelse(Cognome == "COCCHI", 30, peror))))),
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
  
  
  mutate(
    
    contr  = ifelse( Dirigente == "N", (36*peror)/100, (38*peror)/100),
    hcontr =  contr*47.4) %>% 
  
    mutate(categoria = ifelse(Mansione == "DIRIGENTE VETERINARIO", "Veterinarians", 
                            ifelse(Contratto == "DIRIGENZA MEDICA/VETERINARIA" & Mansione != "DIRIGENTE VETERINARIO", "Others", 
                                   ifelse(Contratto == "DIRIGENZA SPTA", "Administrative", 
                                          ifelse(Ruolo== "AMMINISTRATIVO", "Administrative", 
                                                 ifelse(Contratto == "COMPARTO" & Ruolo != "AMMINISTRATIVO", "Tecnici", "X")))))) %>%  View()
  
  
















select(Dipartimento, Reparto, Laboratorio, Dirigente, ricercatore, contr, hcontr) %>%   
  group_by(Dipartimento, Reparto, Laboratorio, Dirigente, ricercatore) %>% 
  
  summarise(hcontr = sum(hcontr)) %>%  
  mutate(FTE = ifelse(Dirigente == "S", hcontr/(38*47.4), hcontr/(36*47.4))) %>%  
  select(-hcontr) %>%  
  mutate(Dirigente = ifelse(Dirigente == "N" & ricercatore == "ricercatore", "R",
                            ifelse(Dirigente == "N" & ricercatore == "altro", "N", Dirigente))) %>%  
  select(-ricercatore) %>% ungroup() %>% 
  
  pivot_wider(names_from = Dirigente, values_from = FTE) %>%  
  rename( "FTED" = S, 
          "FTEC" = N) %>%  
  # "FTER" = R)  %>%  
  # mutate(FTER = ifelse(is.na(FTER), 0, FTER), 
  #        'FTEC' = FTEC-FTER) %>% 
  # select(- FTER) %>% 
  ungroup() %>%   
  write.xlsx(file = "FTE2023.xlsx")

# gt() %>% 
# gtsave("fte.rtf")