
IZSLER <- tizsler %>% 
          filter(Anno == 2020)

pr <-  prj %>% 
       mutate("Stato" = ifelse(annofine < 2020, "Archiviato", "Attivo")) %>% 
       filter(Stato == "Attivo" & annoinizio <= 2020)


pubs <- pub %>% 
        filter(OA == 2020)






tdip <- 
  IZSLER  %>%
    left_join(
      (pubs %>%
         filter(articoliif == "IF") %>%
         # count(Dipartimento, NR) %>%
         group_by(Dipartimento) %>%  
         # count(NR) %>%
         summarise("Pubblicazioni" = nlevels(factor(NR)))), by = "Dipartimento") %>%    
    left_join(
      (pr %>%
         group_by(Dipartimento) %>%
         summarise("Progetti di Ricerca"=nlevels(factor(Codice)))
      ),  by = "Dipartimento" ) %>% 
  ungroup()





tb <- tdip  %>% 
  select(-Anno) %>% 
 
    mutate("Prestazioni" = round(100*(ANALISI/sum(ANALISI)), 1), 
           "VP" = round(100*(VP/sum(VP)),1), 
           "AI" = round(100*(AI/sum(AI)),1), 
           "RT" = round(100*(RT/sum(RT)),1),
           "COSTI" =round(100*(COSTI/sum(COSTI)),1), 
           "ROI" = round(100*(ROI/sum(ROI)), 1), 
           "RFTE" = round(100*(`R-FTE`/sum(`R-FTE`)),1),
           "CFTE" = round(100*(`C-FTE`/ sum(`C-FTE`)), 1)
    ) %>% 
    select( Dipartimento, Prestazioni,   RT, COSTI, ROI, RFTE, CFTE) %>%  
    pivot_longer(!Dipartimento, names_to = "KPI", values_to = "valore") %>% 
    mutate(KPI = factor(KPI, levels = c("Prestazioni","RT", "COSTI", "ROI", "RFTE", "CFTE" )))
 



output$tbd <- 
  
  if(input$ind == "Dipartimento")
    
  {
    
    
    
    ggplot(tb,  aes( 
      x = KPI, 
      y = valore, 
      fill = KPI
    )) + geom_col(width = 0.9, color = "black")+
      coord_polar(theta = "x")+ facet_wrap(~Dipartimento, nrow = 1)+
      scale_fill_brewer(palette = "Blues")+
      geom_text(aes(y = valore-8, label = paste0(valore, "%")), color = "black", size=3)+
      theme(legend.position = "blank",
            panel.background= element_blank(),
            plot.background = element_blank(), 
            strip.text.x = element_text(size = 15, colour = "blue"), 
            axis.text.x = element_text(size = 10, color = "black"))+
      labs(x = "", y = "") 
    
  }
  
  else
    
  {
    tb %>% 
      mutate(Dipartimento = recode(Dipartimento, "Dipartimento sicurezza alimentare" = "DSA", 
                                   "Dipartimento tutela e salute animale" = "DTSA", 
                                   "Dipartimento area territoriale Lombardia" = "ATLOMB", 
                                   "Dipartimento area territoriale Emilia Romagna" = "ATER", 
                                   "Direzione Sanitaria" = "DirSAN")) %>%  
      ggplot(aes( 
        x = Dipartimento, 
        y = valore, 
        fill = Dipartimento
      )) + geom_col(width = 0.9, color = "black")+
      coord_polar(theta = "x")+ facet_wrap(~KPI, nrow = 1)+
      scale_fill_brewer(palette = "Blues")+
      geom_text(aes(y = valore-8, label = paste0(valore, "%")), color = "black", size=3)+
      theme(legend.position = "blank",
            panel.background= element_blank(),
            plot.background = element_blank(), 
            strip.text.x = element_text(size = 15, colour = "blue"), 
            axis.text.x = element_text(size = 10, color = "black"))+
      labs(x = "", y = "")
    
  }, bg = "transparent")













#####
library(here)
library(tidyverse)
library(writexl)

ob <- readRDS(here("data", "processed", "performance.RDS"))
write_xlsx(ob, path = "obiettivi.xlsx")
