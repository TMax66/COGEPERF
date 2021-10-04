library(tidyverse)
 

library(readr)
library(lubridate)





names(acc) <- c("anno","pc","nconf", "strpropr", "stracc","operatore", "locstracc", "finalità", 
                "settore", "pagamento", "dtreg", "prove", "tipoprel", "istrdp", "dtirdp", "dturdp", 
                "wday", "ncamp")




GCUpost <- c('ACC-CENTR2', 'PC-47326', 'PC-40780', 
        'MP-ACC3', 'BS-ASS-N', 'PC-47327', 
        'CH-ACC4-N', 'CH-ACC2-N', 'MP-SIVARS7', 
        'PC-47499', 'MP-SIVARS7-N')

GCUop <- c("Muhammad Ibraheem", "Zanoni Dr.ssa Mariagrazia", "Avisani Dominga", 
           "Savoldini Laura", "Merigo Silvia", "Marmaglio Giordano", "Baldin Silvia", 
           "Barbeno Claudio", "Boccacci Giuliana", "Bettinzoli Luana", "Bonometti Laura Camilla")




##
library(readxl)
contrACC <- read_excel("C:/Users/vito.tranquillo/Desktop/controllo accettazioni GTR.xlsx")

gest <- contrACC %>% 
  filter(`Utente che ha registrato` %in% GCUop)  
   

gest <- gest %>% 
  filter(!`Nome Stazione Inserimento` %in% c('ACC-CENTR2', 'PC-47326', 'PC-40780', 
                                            'MP-ACC3', 'BS-ASS-N', 'PC-47327', 
                                            'CH-ACC4-N', 'CH-ACC2-N', 'MP-SIVARS7', 
                                            'PC-47499', 'MP-SIVARS7-N') )
write.table(gest, file = "dati.txt")


# izsler <- acc %>% 
#   group_by(nconf) %>% 
#   distinct(nconf) %>%  
#   nrow()
  # count() %>% ungroup() %>% 
  # summarise(s = sum(n))

accBS <- acc %>% 
  filter(locstracc == "S" | stracc == "Sede Territoriale di Brescia")  %>%  
  group_by(nconf) %>%
  distinct(nconf) %>%  
  nrow()

  
noBS <- acc %>% 
  anti_join(accBS)

gest <- accBS %>% 
    filter(pc %in% GTRpost | operatore %in% GTRop)
gest %>% 
  group_by(nconf) %>% 
  distinct(nconf) %>% 
  nrow()
  
gestnoBS <- noBS %>% 
  filter(pc %in% GTRpost | operatore %in% GTRop) %>% 
  group_by(nconf) %>% 
  distinct(nconf) %>% 
  nr
   
 

#valorizzazione attività accettazione-----

acc <- read_delim("C:/Users/vito.tranquillo/Desktop/Git Projects/COGEPERF/Accettazione/postazioni.txt", 
                  "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

names(acc) <- c("nconf", "strpropr", "settore", "finalità", "pagamento", 
                "dtprel", "dtreg", "dtacc", "dtrdp", "IstRDP",  "pc", "gruppoprova")



acc %>% filter(gruppoprova!= "Parere Tecnico") %>% 
  mutate(tipoprove = ifelse(gruppoprova=="Prova Chimica", "Prova Chimica", 
                      ifelse(gruppoprova== "Prova Sierologica", "Prova Sierologica", "Prova Diagnostica/Alimenti"))) %>%  
  select(-gruppoprova) %>% 
  group_by(dtreg, nconf, pc, settore ) %>% 
  pivot_wider(names_from = "tipoprove", values_from = "tipoprove") %>%  View()

  mutate(`Prova Chimica` = ifelse(`Prova Chimica`!= "NULL", 2.46, 0), 
         `Prova Diagnostica/Alimenti` = ifelse(`Prova Diagnostica/Alimenti` != "NULL", 0.72, 0),
         `Prova Sierologica` = ifelse(`Prova Sierologica` != "NULL", 0.20, 0)) %>%  
  rowwise() %>% 
  mutate(valore= sum(`Prova Chimica` ,`Prova Diagnostica/Alimenti`, `Prova Sierologica`), 
         valore = 0.07*(valore)+valore) %>%
  ungroup() %>%  
  group_by(dtreg, pc) %>% 
  summarise(n.conf = n(), 
    valore = sum(valore)) %>% 
  mutate(Anno = year(dtreg)) %>%  
  group_by(Anno) %>% 
  summarise(n.conf = sum(n.conf), 
            valore = sum(valore)) %>% 
  tibble(Dipartimento = "Direzione sanitaria", Reparto = "GESTIONE CENTRALIZZATA DELLE RICHIESTE", 
         Laboratorio = "	GESTIONE CENTRALIZZATA DELLE RICHIESTE")  %>% View()
  saveRDS(here("data", "processed", "GCR.rds"))
  




