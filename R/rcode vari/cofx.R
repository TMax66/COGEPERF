

pr <-  prj %>% 
                 mutate("Stato" = ifelse(annofine < 2021, "Archiviato", "Attivo")) %>% 
                 filter(Stato == "Attivo" & annoinizio <= 2021)



 
pr  %>%
    group_by(CodIDIzler, Tipologia, DataInizio, DataFine, Descrizione, RespScient ) %>%
    
    summarise(Budget = sum(Budget)) %>% View()
  #   ungroup() %>%
  #     group_by(CodIDIzler, Tipologia, DataInizio, DataFine, Descrizione, RespScient, Budget, nUO) %>% 
  #    # summarise(Durata = as.numeric(DataFine-DataInizio), 
  #              # R = as.numeric(date("2019-12-31")-date(DataInizio)), 
  #               #Realizzazione = ifelse(R>Durata, 100, 100*(R/Durata)),
  #               #Realizzazione = paste(round(Realizzazione, 0), "%") )%>% 
  #     mutate(DataInizio = as.character(DataInizio), 
  #            DataFine = as.character(DataFine)) #%>% 
  #     #arrange(Realizzazione) %>% 
  #    # select(-R, -Durata)
  #   
 
  
  
  pr %>%
    group_by(CodIDIzler, Tipologia, DataInizio, DataFine, Descrizione, RespScient) %>% 
    summarise(Budget = sum(Budget), nUO = n()) %>% 
    ungroup() %>% 
    group_by(CodIDIzler, Tipologia, DataInizio, DataFine, Descrizione, RespScient, Budget, nUO) %>% 
    summarise(Durata = as.numeric(DataFine-DataInizio), 
              R = as.numeric(date("2019-12-31")-date(DataInizio)), 
              Realizzazione = ifelse(R>Durata, 100, 100*(R/Durata)),
              Realizzazione = paste(round(Realizzazione, 0), "%") )%>% 
    mutate(DataInizio = as.character(DataInizio), 
           DataFine = as.character(DataFine)) %>% 
    arrange(Realizzazione) %>% 
    select(-R, -Durata) %>% View()
  