#codifica strutture e attività del file newanalisi1921


analisi <- read_excel(sheet = "Report 1", here(  "data", "raw",  "newanalisi1921.xls"))

names(analisi)[1:4] <- c("Dipartimento", "Reparto", "Laboratorio", "Centro di Costo")

dtanalisi <- analisi %>% 
  mutate(ClassAnalisi = recode(`Cod. classificazione`, 
                               `-1` = "Ufficiale a Pagamento", 
                               `-3` = "Ufficiale a Pagamento", 
                               `-8` = "Non Ufficiale a Pagamento", 
                               `-9` = "Non Ufficiale a Pagamento", 
                               `-4` = "Ufficiale Gratuito", 
                               `-5` = "Ufficiale Gratuito", 
                               `-7` = "Ufficiale Gratuito", 
                               `-11` = "Ufficiale Gratuito", 
                               `-6`  = "Non Ufficiale Gratuito", 
                               `-10` = "Non Ufficiale Gratuito", 
                               `-13` = "Non Ufficiale Gratuito" 
                               ), 
         Pagamento = recode(`Cod. classificazione`, 
                            `-1` = "Pagamento", 
                            `-3` = "Pagamento", 
                            `-8` = "Pagamento", 
                            `-9` = "Pagamento", 
                            `-4` = "Gratuito", 
                            `-5` = "Gratuito", 
                            `-7` = "Gratuito", 
                            `-11` = "Gratuito", 
                            `-6`  = "Gratuito", 
                            `-10` = "Gratuito", 
                            `-13` = "Gratuito" 
         ), 
         
         
         Quarter = factor(paste(Anno, ".",`N. Trimestre` )))


dt <- dtanalisi %>% 
  group_by(`Centro di Costo`, Pagamento, ClassAnalisi, Quarter) %>% 
  summarise(Valorizzato = sum(`A Tariffario`, na.rm = TRUE), 
            Fatturato = sum(Fatturato, na.rm = TRUE)) %>% 
  mutate(var_change = round((Valorizzato/lag(Valorizzato) - 1) * 100, 2 ))
  
  
library(hrbrthemes)
dt %>%
  filter(`Centro di Costo` == "SEDE TERRITORIALE DI BERGAMO" & Pagamento == "Gratuito") %>% 
  ggplot(
    aes(y = var_change, x = Quarter,  label=var_change))+
   geom_line(aes(group = ClassAnalisi), size= 1.1, col = "darkgrey")+ 
  geom_label(size = 4, col = "blue")+
  facet_wrap(.~ClassAnalisi, ncol = 1)+
  geom_hline(yintercept = 0, size = 0.5)+
  labs(y = "Variazione %", x = "Anno.Trimestre")+
  theme_ipsum_rc()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  
  
#   facet_wrap(.~ClassAnalisi)

















###grafici andamento parametri

tab <- tizsler %>% 
  left_join(
    (pub %>%
       filter(articoliif == "IF"  ) %>%  
       # count(Dipartimento, NR) %>%
       group_by(OA, Dipartimento) %>%  
       # count(NR) %>%  
       summarise("Pubblicazioni" = nlevels(factor(NR)))), by = "Dipartimento") %>%    
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