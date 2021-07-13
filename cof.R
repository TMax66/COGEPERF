###grafici andamento parametri

tab <- tizsler %>% 
    left_join(
      (pub %>%
         filter(articoliif == "IF") %>%
         count(Dipartimento, NR) %>%
         group_by(Dipartimento) %>%  
         count(NR) %>%
         summarise("Pubblicazioni" = sum(n))), by = "Dipartimento") %>%    
    left_join(
      (prj %>%
         group_by(Dipartimento) %>%
         summarise("Progetti di Ricerca"=nlevels(factor(Codice)))
      ),  by = "Dipartimento" )
  
  
  
tab %>% select(Anno, Dipartimento, 'R-FTE', 'C-FTE') %>% 
  pivot_longer(cols = 3:4, names_to = "Parametro", values_to = "valore") %>% 
  ggplot(aes(x=Anno, y = valore , label= valore))+
  geom_line()+geom_point()+geom_text()+
  facet_grid(Parametro~Dipartimento)+
  theme_minimal()+
  theme(axis.text.y = element_blank())
  