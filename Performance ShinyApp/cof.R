
pubsdip <- pub %>% 
   filter(OA == 2021 & Dipartimento == "Dipartimento area territoriale Lombardia")

prdip <-  
  prj %>% 
    mutate("Stato" = ifelse(annofine < 2021, "Archiviato", "Attivo")) %>% 
    filter(Stato == "Attivo" & annoinizio <= 2021 & Dipartimento == "Dipartimento area territoriale Lombardia")



tx <- tabIZSLER %>% 
  rename( "Prestazioni" = TotPrestazioni, "Valorizzazione" = TotTariff, "VP" = TotFattVP, "AI" = TAI, 
          "COSTI" = TotCost, "FTED" = FTE_Dirigenza, "FTEC"= FTE_Comparto, Anno = ANNO) %>%
  filter(Anno == 2021 & Dipartimento == "Dipartimento area territoriale Lombardia") %>% 
  
  group_by(Reparto) %>%
  summarise_at(c("Prestazioni", "Valorizzazione",  "VP", "AI", "FTED", "FTEC","COSTI"), sum, na.rm = T) %>%
  mutate(RT = (Valorizzazione+VP+AI),
         FTE_T = round((FTED+FTEC),1)) %>%
  arrange(desc(Prestazioni)) %>%
  mutate("R-FTE" = round(RT/FTE_T,0)) %>% 
  select(-FTED, -FTEC) %>% 
  left_join(
    (pubsdip %>%
       filter(articoliif == "IF") %>%
       count(Reparto, NR) %>%
       group_by(Reparto) %>%  
       count(NR) %>%
       summarise("Pubblicazioni" = sum(n))), by = "Reparto") %>%    
  left_join(
    (prdip %>%
       group_by(Reparto) %>%
       summarise("Progetti di Ricerca"=nlevels(factor(Codice)))
    ), by = "Reparto") 

# %>% 
# 
# 
# writexl::write_xlsx("tab20.xlsx")





tb2 <-  tx %>% 
  mutate(Reparto = recode(Reparto, "SEDE TERRITORIALE DI CREMONA - MANTOVA" = "ST-CRMN", 
                          "SEDE TERRITORIALE DI BRESCIA" = "ST-BS", 
                          "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO" = "ST-BGBISO", 
                          "SEDE TERRITORIALE DI LODI - MILANO" = "ST-LOMI", 
                          "SEDE TERRITORIALE DI PAVIA" = "ST-PV")) %>% 
  
    mutate(Esami = round(100*(Prestazioni/sum(Prestazioni)), 1), 
           Valore = round(100*(Valorizzazione/sum(Valorizzazione)),1), 
           "RVP" =round(100*(VP/sum(VP)),1), 
           "RAI" = round(100*(AI/sum(AI)), 1),
           "RT" = round(100*(RT/sum(RT)),1),
           "FTET" = round(100*(FTE_T/ sum(FTE_T)), 1), 
           Costi = round(100*(COSTI/sum(COSTI)), 1)) %>%  
    select(Reparto, Esami, RT, RVP, RT, FTET, Costi) %>% 
    pivot_longer(!Reparto, names_to = "KPI", values_to = "valore") %>%  
    mutate(KPI = factor(KPI, levels = c("Esami", "RT", "RVP", "RAI", "FTET", "Costi" )))
 
 
 
    


    ggplot(tb2,  aes( 
      x = KPI, 
      y = valore, 
      fill = KPI
    )) + geom_col(width = 0.9, color = "black")+
      coord_polar(theta = "x")+ facet_wrap(~Reparto, nrow = 1)+
      scale_fill_brewer(palette = "Blues")+
      geom_text(aes(y = valore-8, label = paste0(valore, "%")), color = "black", size=3)+
      theme(legend.position = "blank",
            panel.background= element_blank(),
            plot.background = element_blank(), 
            strip.text.x = element_text(size = 12, colour = "blue"), 
            axis.text.x = element_text(size = 10, color = "black"))+
      labs(x = "", y = "") + labs(title = "indicatori per reparto 2021")
    
  
    
    
    
    tb2 %>% 
      ggplot(aes( 
        x = Reparto, 
        y = valore, 
        fill = Reparto
      )) + geom_col(width = 0.9, color = "black")+
      coord_polar(theta = "x")+ facet_wrap(~KPI, nrow = 1)+
      scale_fill_brewer(palette = "Blues")+
      geom_text(aes(y = valore-8, label = paste0(valore, "%")), color = "black", size=3)+
      theme(legend.position = "blank",
            panel.background= element_blank(),
            plot.background = element_blank(), 
            strip.text.x = element_text(size = 15, colour = "blue"), 
            axis.text.x = element_text(size = 10, color = "black"))+
      labs(x = "", y = "")+ labs(title = "Confronto strutture per indicatore 2021")
 


