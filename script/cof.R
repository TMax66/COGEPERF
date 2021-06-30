
tdiprep <-  
  tabIZSLER %>% 
     filter(Anno == 2019 & Dipartimento == "Direzione Sanitaria") %>% 
     rename( "ANALISI" = esami, "VALORE" = valore, "VP" = ricavovp, "AI" = valoreai, 
             "COSTI" = costi) %>%
     group_by(Reparto) %>%
     summarise_at(c("ANALISI", "VALORE",  "VP", "AI", "FTED", "FTEC","COSTI"), sum, na.rm = T) %>%
     mutate(RT = (VALORE+VP+AI),
            FTE_T = round((FTED+FTED),1)) %>%
     arrange(desc(ANALISI)) %>%
     mutate("R-FTE" = round(RT/FTE_T,0), 
            "C-FTE" = round(COSTI/FTE_T, 0), 
            "ROI" = round(RT/COSTI, 2)) %>% 
     select(-FTED, -FTEC)





f1 <- function(dati, Variabile)
          { dati %>% 
    summarise(totes = sum(Variabile)) %>% 
    select(totes)
  }
  
f1(dati =tdiprep, Variabile = "ANALISI")


ValueBOX <- function(Variabile, Titolo, colore, icona)
{ valore <- sum(tdiprep[, Variabile])
valueBox(prettyNum(valore, big.mark = "."), Titolo, icon = icon(icona), color = colore)
}

 

tabella %>% 
  summarise(totale = sum(ANALISI)) %>% 
  select(totale)





























IZSLER <- tizsler %>% 
  filter(Anno == 2019)  


pr <-  prj %>% 
  mutate("Stato" = ifelse(annofine < 2019, "Archiviato", "Attivo")) %>% 
                 filter(Stato == "Attivo" & annoinizio <= 2019)

pubs <- pub %>% 
  filter(OA == 2019)



 tabIZSLER %>% 
   filter(Anno == 2019 & Dipartimento == "Dipartimento area territoriale Emilia Romagna") %>% 
   rename( "ANALISI" = esami, "VALORE" = valore, "VP" = ricavovp, "AI" = valoreai, 
          "COSTI" = costi) %>%
  group_by(Reparto) %>%
  summarise_at(c("ANALISI", "VALORE",  "VP", "AI", "FTED", "FTEC","COSTI"), sum, na.rm = T) %>%
  mutate(RT = (VALORE+VP+AI),
         FTE_T = round((FTED+FTED),1)) %>%
  arrange(desc(ANALISI)) %>%
  mutate("R-FTE" = round(RT/FTE_T,0), 
         "C-FTE" = round(COSTI/FTE_T, 0)) %>% 
  select(-FTED, -FTEC) 


diprep <- function(Anno, Dipartimento)
{
  tabIZSLER %>% 
    filter(Anno == Anno & Dipartimento == "Dipartimento") %>% 
    rename( "ANALISI" = esami, "VALORE" = valore, "VP" = ricavovp, "AI" = valoreai, 
            "COSTI" = costi) %>%
    group_by(Reparto) %>%
    summarise_at(c("ANALISI", "VALORE",  "VP", "AI", "FTED", "FTEC","COSTI"), sum, na.rm = T) %>%
    mutate(RT = (VALORE+VP+AI),
           FTE_T = round((FTED+FTED),1)) %>%
    arrange(desc(ANALISI)) %>%
    mutate("R-FTE" = round(RT/FTE_T,0), 
           "C-FTE" = round(COSTI/FTE_T, 0)) %>% 
    select(-FTED, -FTEC) 
  
}

diprep(Anno = 2019, Dipartimento = "Direzione Sanitaria")

tabIZSLER %>% 
  filter(Anno == 2019 ) %>% 
  rename( "ANALISI" = esami, "VALORE" = valore, "VP" = ricavovp, "AI" = valoreai, 
          "COSTI" = costi) %>%
  group_by(Reparto) %>%
  summarise_at(c("ANALISI", "VALORE",  "VP", "AI", "FTED", "FTEC","COSTI"), sum, na.rm = T) %>%
  mutate(RT = (VALORE+VP+AI),
         FTE_T = round((FTED+FTED),1)) %>%
  arrange(desc(ANALISI)) %>%
  mutate("R-FTE" = round(RT/FTE_T,0), 
 
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
                 "C-FTE" = round(COSTI/FTE_T, 0)) %>% 
  select(-FTED, -FTEC)





flextable(
  (IZSLER() %>%
     left_join(
       (pubs() %>%
          filter(articoliif == "IF") %>%
          count(Dipartimento, NR) %>%
          group_by(Dipartimento) %>%  
          count(NR) %>%
          summarise("Pubblicazioni" = sum(n))), by = "Dipartimento") %>%    
     left_join(
       (pr() %>%
          group_by(Dipartimento) %>%
          summarise("Progetti di Ricerca"=nlevels(factor(Codice)))
       ),  by = "Dipartimento" )), 
  col_keys = c("Dipartimento", "ANALISI", "VALORE", "VP", "AI", "COSTI", "RT", "R-FTE", "C-FTE", "Pubblicazioni", "Progetti di Ricerca")
)