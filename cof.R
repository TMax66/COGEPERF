prj %>% filter(Tipo_P_A == "P") %>% 
  mutate(annoscadfin = year(DataScadenzaRelazioneFinale), 
         annoscdint  = year(DataScadenzaRelazioneIntermedia)) %>% 
  select(CodIDIzsler, Descrizione, RespScientifico, DataScadenzaRelazioneFinale, annoscadfin, DataScadenzaRelazioneIntermedia,annoscdint, ) %>% 
  filter(annoscadfin == 2021) %>% 
  write.xlsx(file = "prjint21.xlsx")
