IZSLER <- tizsler %>% 
  filter(Anno == 2019)  


pr <-  prj %>% 
  mutate("Stato" = ifelse(annofine < 2019, "Archiviato", "Attivo")) %>% 
                 filter(Stato == "Attivo" & annoinizio <= 2019)

pubs <- pub %>% 
  filter(OA == 2019)


# flextable(   
# (
  
x <- IZSLER %>%
  select(-Anno) %>% 
  left_join(
    (pubs %>%
       filter(articoliif == "IF") %>%
       count(Dipartimento, NR) %>%
       group_by(Dipartimento) %>%  
       count(NR) %>%
       summarise("Pubblicazioni" = sum(n))), by = "Dipartimento") %>%    
  left_join(
    (pr %>%
       group_by(Dipartimento) %>%
       summarise("Progetti di Ricerca"=nlevels(factor(Codice)))
    ),  by = "Dipartimento" )  



flextable(x[, -1]) %>% 
  theme_booktabs() %>%
  color(i = 1, color = "blue", part = "header") %>%
  bold( part = "header") %>%
  fontsize(size=15) %>%
  fontsize(part = "header", size = 15) %>%
  line_spacing(space = 2.5)
  

#))


#%>%
  theme_booktabs() %>%
  color(i = 1, color = "blue", part = "header") %>%
  bold( part = "header") %>%
  fontsize(size=15) %>%
  fontsize(part = "header", size = 15) %>%
  line_spacing(space = 2.5) %>%
  colformat_num(j = c( "RA", "RVP", "RAI", "RT", "R/FTET"), big.mark = ".", decimal.mark = ",", prefix = "€") %>%
  autofit() %>%
  color(j= "R/FTET", color = "red", part = "all") %>%
  color(j= "Pubblicazioni",color = "red", part = "all" ) %>%
  color(j= "Progetti di Ricerca", color = "red", part = "all") %>%
  vline(j= "RT", border = border, part = "all") %>%
  footnote(i=1, j=3:10,
           value = as_paragraph(
             c("Full Time Equivalenti Dirigenza",
               "Full Time Equivalenti Comparto",
               "Full Time Equivalenti Totale",
               "Ricavo da Analisi",
               "Ricavo Vendita Prodotti",
               "Ricavo Attività Interna",
               "Ricavo Totale",
               "Ricavo per Full Equivalenti Totale")
           ),
           ref_symbols = c("a","b","c","d","e","f","g","h"),
           part = "header", inline = T) %>%
  fontsize( i = NULL, j = NULL, size = 13, part = "footer") %>%
  htmltools_value()

