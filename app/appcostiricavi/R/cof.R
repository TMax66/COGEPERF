AU <- function(CC, Uff){
  
  dtanalisi %>% 
    filter(Uff == Uff & Costi == "Ricavo") %>% 
    group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC,ClassAnalisi, Classe, Area) %>% 
    filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%  
    summarise(N_Det = sum(Determinazioni, na.rm = TRUE),
              N_Num = sum(Numero, na.rm = TRUE), 
              S_Tariffa = sum(Tariffario, na.rm = TRUE), 
              S_Fatturato = sum(Fatturato, na.rm = TRUE))  %>% 
    filter(CDC == CC & Classe == "Prestazioni") %>% 
    group_by(ANNO, Quarter, Area) %>% 
    summarise(N = sum(N_Det, na.rm=TRUE)) %>% 
    mutate(YQ = paste(ANNO, "-", Quarter)) %>% ungroup() %>% 
    select(-ANNO, -Quarter) %>% 
    pivot_wider( names_from = YQ,  values_from = N, values_fill = 0) %>%   
    left_join(  
      
      (dtanalisi %>% 
         filter(Uff == Uff & Costi == "Ricavo") %>% 
         group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC,ClassAnalisi, Classe, Area) %>% 
         filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%  
         summarise(N_Det = sum(Determinazioni, na.rm = TRUE),
                   N_Num = sum(Numero, na.rm = TRUE), 
                   S_Tariffa = sum(Tariffario, na.rm = TRUE), 
                   S_Fatturato = sum(Fatturato, na.rm = TRUE))  %>% 
         filter(CDC == CC & Classe == "Prestazioni") %>% 
         group_by(ANNO, Quarter, Area) %>% 
         summarise(N = sum(N_Det, na.rm=TRUE)) %>% 
         mutate(YQ = paste(ANNO, "-", Quarter)) %>%
         select(-ANNO, -Quarter) %>% 
         group_by(Area) %>%
         summarise(trend = spk_chr(N, type= "line", options =
                                     list(paging = FALSE)))
      )) %>% rename("Prestazioni" = Area) %>% 
    
    format_table()  %>% 
    htmltools::HTML() %>% 
    div() %>% 
    spk_add_deps()
  
  
}




dtanalisi %>% 
  filter(Uff == "Non Ufficiale" & Costi == "Ricavo") %>%
  group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC,ClassAnalisi, Classe, Area) %>% 
  filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%  
  summarise(N_Det = sum(Determinazioni, na.rm = TRUE),
            N_Num = sum(Numero, na.rm = TRUE), 
            S_Tariffa = sum(Tariffario, na.rm = TRUE), 
            S_Fatturato = sum(Fatturato, na.rm = TRUE))  %>% 
  filter(CDC == "SEDE TERRITORIALE DI BOLOGNA" & Classe == "Prestazioni") %>%  
  group_by(ANNO, Quarter, Area) %>% 
  summarise(N = sum(N_Det, na.rm=TRUE)) %>% 
  mutate(YQ = paste(ANNO, "-", Quarter)) %>% ungroup() %>% 
  select(-ANNO, -Quarter) %>% 
  pivot_wider( names_from = YQ,  values_from = N, values_fill = 0) %>% View()


