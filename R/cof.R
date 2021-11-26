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