library(tidyverse)

library(lemon)
library(hrbrthemes)
library(readxl)
library(lubridate)

detach(package:plyr)
d <- tibble("Ruolo" = c(rep("Dirigenza", 2), rep("Comparto", 2), rep("Borse di studio", 2)), 
            "Genere" = rep(c("Uomini", "Donne"), 3), 
            "Numero" = c(58, 63, 172, 360, 15, 35))

 d %>% 
  ggplot(aes(x = Ruolo, y = Numero, fill = Genere))+
  geom_bar(data = subset(d, Genere == "Donne"), stat = "identity") + 
  geom_bar(data = subset(d, Genere == "Uomini"), stat = "identity") +
  scale_y_continuous(breaks = seq(-1000, 1000, 100)) + 
  coord_flip() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()
  
  
 test <- data.frame(v=sample(1:20,1000,replace=T), g=c('M','F'))
 
 
 ggplot(data = d, 
        mapping = aes(
          x = Ruolo, 
          y = ifelse(test = Genere == "Uomini",  yes = -Numero, no = Numero), 
          fill = Genere,
          label= Numero
        )) +
   geom_bar(stat = "identity")+
   geom_text(hjust=ifelse(test = d$Genere == "Uomini",  yes = 1.2, no = -0.2), size=4, colour="#505050") +
   scale_y_continuous(labels = abs, limits = c(-400, 400)) +
   coord_flip()+
   labs(
     x = "",
     y = "",
     fill=""
   ) +
   theme_ipsum_rc()+
   theme( 
     panel.grid.major = element_blank(), 
     panel.grid.minor = element_blank(),
     axis.text.x=element_blank(), 
     legend.position="bottom",
     legend.text=element_text(size=12), 
     axis.text.y=element_text(size=16),
     strip.text.x=element_text(size=16),
   )

 
 prj20 <- read_excel("PROGRAMMAZIONE/performances/Relazione Performances/2020 relazione performance tab e figure/progetti2020.xlsx")

 
 prj20 %>% 
    pivot_longer(names_to = "Stato", cols = 2:4) %>% 
    mutate(Stato = factor(Stato, levels = c("In corso", "Conclusi", "Nuovi"))) %>% 
    ggplot(aes(x= reorder(Tipologia, value),  y=value))+
    geom_bar(stat = "identity", fill = "gray")+
    geom_text(aes(label = value), position = "dodge", size = 7)+
    coord_flip()+
    labs(
       x = "",
       y = "",
       title = "Distribuzione del numero dei progetti di ricerca nell'anno 2020"
    ) +
    facet_wrap( ~Stato)+
    theme_ipsum_rc(base_size = 16, 
                   strip_text_size = 20) 


 
 form <- tibble("Tipologia" = c("Formazione sul campo", 
                               "Residenziale", 
                               "Formazione a distanza"), 
               "N.corsi" = c(9, 31, 23))
 

form %>% 
   ggplot(aes(x= reorder(Tipologia, N.corsi ), y= N.corsi))+
   geom_bar(stat = "identity", fill = "gray")+
   geom_text(aes(label = N.corsi), position = "dodge", size=6)+
   coord_flip()+
   labs(
      x = "",
      y = "",
      title = "Distribuzione del numero di corsi erogati nel 2020"
   ) +
   theme_ipsum_rc(base_size = 18) 


library("here")

ricercatori <- read_excel(here("programmazione",  "piramideR", "ricercatori.xlsx"))
ricercatori <- ricercatori[-c(1259:1268),]
 

ricercatori$Cognome <- gsub(",.*$", "", ricercatori$Name)
ricercatori <- ricercatori %>%
   mutate(Cognome = recode(Cognome, "Moreno" = "Moreno Martin", 
                           "Martin" = "Moreno Martin", 
                           "Elisabetta" = "Caprai", 
                           "Cosciani-Cunico" = "Cosciani Cunico", 
   ))


ricercatori %>% 
   filter(`Publication Year` == 2020) %>% 
   group_by(Cognome) %>% 
      summarise("Pubblicazioni" = sum(`Web of Science Documents`), 
                "Citazioni" = sum(`Times Cited`), 
                "Cit.Media" = median(Citazioni/Pubblicazioni)) %>% 
   pivot_longer(names_to = "parametro", cols = 2:4) %>% 
   mutate(parametro = factor(parametro, levels = c("Pubblicazioni", "Citazioni", "Cit.Media"))) %>% 
   ggplot(aes(x = value))+
   geom_histogram(bins = 15, fill = "gray", col = "black")+
   facet_wrap(~parametro)+
   labs(
      x = "",
      y = "",
      subtitle = "Dati produttivi e citazionali dell'attività di pubblicazione scientifica dell'IZSLER nel 2020"
   ) +
   theme_ipsum_rc() 
theme(
   axis.text.x=element_blank(),
)


ricercatori %>% 
   filter(`Publication Year` == 2020) %>% 
   unique() %>% 
   summarise(n=n())
   
ricercatori %>% 
   filter(`Publication Year` == 2020) %>% 
   group_by(Cognome) %>% 
   summarise("Pubblicazioni" = sum(`Web of Science Documents`), 
             "Citazioni" = sum(`Times Cited`), 
             "Cit.Media" = mean(Citazioni/Pubblicazioni)) %>% 
   ungroup() %>% 
   summarise(mP= mean(Pubblicazioni), 
             MP = median(Pubblicazioni),
             minP = min(Pubblicazioni), 
             maxP = max(Pubblicazioni), 
             mCit = mean(Citazioni),
             minCit = min(Citazioni), 
             maxCit = max(Citazioni), 
             MCit = median(Citazioni), 
             mCitm = mean(Cit.Media), 
             MCitm = median(Cit.Media), 
             minMcitm = min(Cit.Media), 
             maxMvitm = max(Cit.Media)) 


pubblicazioni <- read_excel(here("programmazione", "piramideR", "pub2000-2020.xlsx"))
pubblicazioni$Cognome <- str_to_lower(pubblicazioni$AU)
pubblicazioni$Cognome <- gsub(",.*$", "", pubblicazioni$Cognome)




prj_func <- function(dati, dtf1, dti, anno)
{ prj %>%
      mutate("Stato" = ifelse(DataFine < as.Date(dtf1), "Archiviato", "Attivo")) %>% 
      filter(Stato == "Attivo" & DataInizio <= as.Date(dti)) %>%
      # mutate("Statoanno" = ifelse(DataFine <=as.Date(dtf1), "Concluso", "Aperto")) %>%
      # filter(Statoanno == "Aperto") %>% 
      summarise( 
                "N.Progetti"=nlevels(factor(Codice)))%>%  
      mutate(anno = anno)
   
}

prj_func(dati = prj, dtf1 = "2020-01-01", dti = "2020-12-31", anno = 2019)


prj_func2 <- function(dati, dt1, dt2)
{  prj %>%
      filter(DataInizio >= as.Date(dt1) & DataInizio <=as.Date(dt2)) %>% 
      summarise("N.Progetti"=nlevels(factor(Codice))) 
   
}


prj_func2(prj, dt1 = "2016-01-01", dt2= "2016-12-31")


##covid####

covid <- read_excel("PROGRAMMAZIONE/performances/Relazione Performances/covid.xlsx", 
                    col_types = c("date", "text", "numeric", 
                                  "numeric", "numeric"))
   
library(zoo)
covid %>% 
   mutate(anno = year(data)) %>% 
   pivot_longer(names_to = "Laboratorio", cols = 3:5) %>% 
   group_by(data) %>% 
   summarise(Tot = sum(value, na.rm = T)) %>% 
   filter(Tot > 0) %>%
   mutate(sett = rollmean(Tot, k = 30, fill = NA) )%>% 
   ggplot(aes(
      x = data, 
      y = sett
   ))+
   geom_line(col = "blue", size = 1.5)+
   # geom_col( aes(y = Tot), 
   #    alpha = 1/5)+
   
   geom_point(aes(x = data, 
                  y = Tot), alpha = 1/5)+
   geom_line(aes(x = data, 
                 y = Tot), alpha = 1/5)+
   geom_hline(yintercept = 2729) +
   
   labs(
      y = "Numero Tamponi Naso Faringei", 
      x = "", 
      title = "Andamento del numero di tamponi naso-faringei processati dai laboratori COVID dell'IZSLER nel 2020 e primi mesi del 2021", 
      subtitle = " I punti rappresentano il numero di tamponi giornalieri, la linea blu la media mobile mensile"
   )+
   theme_ipsum_rc(base_size = 14,  axis_title_size = 15, 
                  plot_title_size = 12)+
   theme(
      axis.text.x=element_text(size = 14),
    
   )
   
 
   
covid %>% 
   mutate(anno = year(data)) %>% 
   pivot_longer(names_to = "Laboratorio", cols = 3:5) %>% 
   group_by(data) %>% 
   summarise(Tot = sum(value, na.rm = T)) %>%  
   filter(Tot > 0) %>% 
   summarise(m = max(Tot), 
             media = mean(Tot))
