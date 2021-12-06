## if FTE

tdip <-  
tizsler %>% filter(Anno == 2021) %>% View()
    left_join(
      (pub %>% filter(OA == 2021) %>% 
         filter(articoliif == "IF") %>%
         # count(Dipartimento, NR) %>%
         group_by(Dipartimento) %>%  
         # count(NR) %>%
         summarise("Pubblicazioni" = nlevels(factor(NR)), 
                   "Impact Factor" = sum(IF, na.rm = TRUE), 
                   "IF mediano" = median(IF, na.rm = TRUE))), by = "Dipartimento") 
    
    
    
    fte <- ore %>% 
      mutate(Dirigente = recode(Dirigente, N = "Comparto", S = "Dirigenza")) %>% 
      filter(Dipartimento != "Non applicabile" &
             !is.na(Dirigente) & 
               !is.na(Ore) & 
               Mese == 12 & 
               ANNO == 2020) %>%
      group_by(ANNO, Dipartimento, Reparto, Laboratorio, Dirigente) %>%
      mutate(fte = ifelse(Dirigente == "Comparto",  36*47.4,  38*47.4)) %>%  
      summarise(FTE = sum(fte)/fte) %>% 
      distinct() %>% 
      pivot_wider(names_from = "Dirigente", values_from = "FTE", names_repair = c("Dirigente", "FTE"))  
       
      
      
      
      
      
      
      fte <- ore %>% 
        mutate(Dirigente = recode(Dirigente, N = "Comparto", S = "Dirigenza")) %>% 
        filter(Dipartimento != "Non applicabile") %>% 
        group_by(ANNO, Dipartimento, Reparto, Laboratorio, Dirigente) %>%   
        filter(!is.na(Dirigente) & !is.na(Ore)) %>% 
        summarise(hworked = sum(Ore, na.rm = T)) %>% 
        mutate(FTE = ifelse(Dirigente == "Comparto", hworked/(36*47.4), hworked/(38*47.4))) %>% 
        pivot_wider(names_from = "Dirigente", values_from = c("hworked", "FTE"))  %>% 
        select(-hworked_, -FTE_)  
      