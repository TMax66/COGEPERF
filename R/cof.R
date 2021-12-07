## if FTE

# tdip <-  
# tizsler %>% filter(Anno == 2021) %>% View()
#     left_join(
#       (pub %>% filter(OA == 2021) %>% 
#          filter(articoliif == "IF") %>%
#          # count(Dipartimento, NR) %>%
#          group_by(Dipartimento) %>%  
#          # count(NR) %>%
#          summarise("Pubblicazioni" = nlevels(factor(NR)), 
#                    "Impact Factor" = sum(IF, na.rm = TRUE), 
#                    "IF mediano" = median(IF, na.rm = TRUE))), by = "Dipartimento") 
#     
    
    
    fte <- ore %>% 
      mutate(Dirigente = recode(Dirigente, N = "Comparto", S = "Dirigenza")) %>% 
      filter(Dipartimento != "Non applicabile" &
             !is.na(Dirigente) & 
               !is.na(Ore) & 
               Mese == 12 & 
               ANNO == 2020) %>%  
      mutate(fte = ifelse(Dirigente == "Comparto",  36*47.4,  38*47.4)) %>% 
      group_by(ANNO, Dipartimento, Reparto, Laboratorio, Dirigente) %>%
      summarise(FTE = sum(fte)/fte) %>% 
      distinct() %>%  
      select(-ANNO) %>%
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
      
      
      
      
      library(readxl)
      graf1 <- read_excel("data/raw/graf1.xlsx")
      
 p1 <-  graf1 %>% 
    mutate(ricavi = factor(ricavi, levels = c("Assegnazione Annua( Stato/Regioni)",
                           "Assegnazione Ricerca", 
                           "Ricavi Prestazioni Sanitarie a pagamento", 
                           "Contributi conto capitale", 
                           "Altri ricavi")
                           )) %>% 
    pivot_longer(cols = 2:4, names_to = "anno", values_to = "valore") %>% 
    ggplot(
      aes(x = anno, 
          y = valore/1000, group = ricavi)
    )+
    geom_point()+geom_line()+
    facet_wrap(~ricavi, ncol = 1, scales = "free")+
    labs(title = "Ricavi", y = "importo*1000", x = "Anno")+
    theme_bw()
      
  
  graf2 <- read_excel("data/raw/graf2.xlsx")
  
  p2 <- graf2 %>% 
    mutate(costi = factor(costi, levels = c("Beni", "Servizi", "Risorse umane", 
                                              "Ammortamenti","Accantonamenti","Imposte" ,
                                              "Altri costi" )
    )) %>% 
    pivot_longer(cols = 2:4, names_to = "anno", values_to = "valore") %>% 
    ggplot(
      aes(x = anno, 
          y = valore/1000, group = costi)
    )+
    geom_point()+geom_line()+
    facet_wrap(~costi, ncol = 1, scales = "free")+
    labs(title = "Costi", y = "importo*1000", x = "Anno")+
    theme_bw()
  
  
  bilancio <- tibble(anno= c(2018, 2019, 2020), 
                     utile = c(9608690,11105153, 8674779 ))
  
p3 <- bilancio %>% 
  
    ggplot(
      aes(x = as.factor(anno), 
          y = utile/1000, group=1)
    )+
    geom_point()+geom_line()+
    
  labs(title = "Risultato di Bilancio", y = "importo*1000", x = "Anno")+
    theme_bw()

library(patchwork)

 p3/(p1|p2)
