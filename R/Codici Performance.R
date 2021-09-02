library(tidyverse)
library(readr)
library(readxl)
library(here)
library(lubridate)
library(DBI)
library(odbc)


#DATI PERFORMANCE####
con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "CED-IIS2", 
                      Database = "ObiettiviStrategiciV2018", Port = 1433)

query <- "SELECT  
Avanzamento,  
Valore, 
Anno,
TipoObiettivo, 
Periodo, 
MacroArea,Obiettivo, 
Azione,
Indicatore, 
StrutturaAssegnataria

FROM ObiettiviStrategiciV2018.dbo.v_EstrazioneObiettivi
WHERE Anno > 2020"

perf <- con %>% tbl(sql(query)) %>% as_tibble() 

##Dati strutture

strutture <- read_xlsx("strutture.xlsx")

dip <- c("DIPARTIMENTO AREA TERRITORIALE EMILIA ROMAGNA", "DIPARTIMENTO SICUREZZA ALIMENTARE", 
               "DIPARTIMENTO TUTELA SALUTE ANIMALE", "DIPARTIMENTO AREA TERRITORIALE LOMBARDIA", 
               "DIPARTIMENTO AMMINISTRATIVO", "CONTROLLO DI GESTIONE")


strutture %>% select(Dipartimento, Reparto) %>% 
  unique()

##ricordificare le strutture

dt <- perf %>% 
  filter(!StrutturaAssegnataria %in% dip & TipoObiettivo == "Operativo" ) %>% 
  mutate(Struttura = recode(StrutturaAssegnataria, 
                            "S.T. PIACENZA E PARMA" = "SEDE TERRITORIALE DI PIACENZA - PARMA", 
                            "REP. CHIM. DEGLI ALIMENTI E MANGIMI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI", 
                            "REP. CHIMICO ALIMENTI BOLOGNA" = "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)", 
                            "REP. PRODUZIONE PRIMARIA" = "REPARTO PRODUZIONE PRIMARIA", 
                            "S.T. BOLOGNA, FERRARA E MODENA" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA", 
                            "S.T. REGGIO EMILIA" = "SEDE TERRITORIALE DI REGGIO EMILIA", 
                            "REP. VIROLOGIA" = "REPARTO VIROLOGIA", 
                            "REP. VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE", 
                            "S.T. BERGAMO, SONDRIO E BINAGO" = "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO", 
                            "S.T. BRESCIA" = "SEDE TERRITORIALE DI BRESCIA", 
                            "S.T. CREMONA, MANTOVA" = "SEDE TERRITORIALE DI CREMONA - MANTOVA", 
                            "S.T. FORLI' E RAVENNA" = "SEDE TERRITORIALE DI FORLÌ - RAVENNA", 
                            "S.T. LODI E MILANO" = "SEDE TERRITORIALE DI LODI - MILANO", 
                            "S.T. PAVIA" = "SEDE TERRITORIALE DI PAVIA", 
                            "U.O. PROVV. ECONOMATO E VENDITE" = "UO PROVVEDITORATO ECONOMATO E VENDITE", 
                            "SERVIZIO ASSICURAZIONE QUALITA" = "SERVIZIO ASSICURAZIONE QUALITA'",
                            "U.O. AFFARI GENERALI E LEGALI" = "U.O. AFFARI GENERALI E LEGALI", 
                            "U.O. TECNICO PATRIMONIALE" = "UO TECNICO PATRIMONIALE", 
                            "U.O. GESTIONE RISORSE UMANE E SVILUPPO COMPETENZE" = "U.O. GESTIONE RISORSE UMANE E SVILUPPO COMPETENZE", 
                            "U.O. GESTIONE SERVIZI CONTABILI" = "U.O. GESTIONE SERVIZI CONTABILI", 
                            "PROGRAMMAZIONE DEI SERVIZI TECNICI E CONTROLLO DI GESTIONE" = "Programmazione dei servizi tecnici e controllo di gestione",
                            "FORMAZIONE" =  "FORMAZIONE E BIBLIOTECA", 
                            "SISTEMI INFORMATIVI" = "Programmazione dei servizi tecnici e controllo di gestione", 
                            "SEGRETERIA DIREZIONALE" = "DIREZIONE GENERALE", 
                            "GESTIONE CENTRALIZZATA DELLE RICHIESTE DELL'UTENZA" = "GESTIONE CENTRALIZZATA DELLE RICHIESTE")
          
  )
  
dt <- dt %>% rename( Reparto = Struttura ) %>%  
  left_join(
    
    (strutture %>% select(Dipartimento, Reparto) %>% 
       unique())
    
    
    ,  by = c("Reparto")) %>% 
  filter(Periodo == 2 & Avanzamento != 0 )
  

##Analisi 
# dt %>% 
#   group_by(Dipartimento, Reparto, MacroArea) %>% 
#   summarise(MediaAvanzamento = median(Avanzamento, na.rm=T)) %>% 
#   filter(Dipartimento == "Direzione Sanitaria") %>% View()
#   
#   
#   dt %>% filter(Dipartimento == "Dipartimento area territoriale Lombardia") %>% 
#   group_by( Reparto, MacroArea) %>% 
#   summarise(media = mean(Avanzamento, na.rm = T)) %>% 
#   pivot_wider(names_from = "Reparto", values_from = "media") %>% View()
  
library(formattable)
Area <-  dt %>%  
    mutate(MacroArea = factor(MacroArea)) %>% 
    group_by(Dipartimento,  MacroArea) %>% 
    summarise(mediana =  round(median(Avanzamento, na.rm = T),2)) %>% 
  mutate(mediana = percent(mediana)) %>% 
    pivot_wider(names_from = "Dipartimento", values_from = "mediana") %>% 
    select("MacroArea","Direzione Generale", "Direzione Sanitaria", "Dipartimento tutela e salute animale", 
           "Dipartimento sicurezza alimentare","Dipartimento area territoriale Lombardia",
           "Dipartimento area territoriale Emilia Romagna",
           "Dipartimento amministrativo") %>% 
    arrange(MacroArea) %>% 
    mutate(MacroArea = as.character(MacroArea)) %>% 
    mutate(MacroArea = gsub("\\d+", "", MacroArea), 
           MacroArea = gsub("\"", "", MacroArea))  
Area <- sapply(Area, as.character)

Area[is.na(Area)] <- ""   

Area %>% data.frame() %>%
  
kbl() %>% 
    kable_styling() %>% 
  kable_paper(bootstrap_options = "striped", full_width = F)
    


Obiettivi <-  dt %>%  
  mutate(Obiettivo = factor(Obiettivo)) %>% 
  group_by(Dipartimento,  Obiettivo) %>% 
  summarise(mediana = 100*round(median(Avanzamento, na.rm = T),2)) %>% 
  pivot_wider(names_from = "Dipartimento", values_from = "mediana") %>% 
  select("Obiettivo","Direzione Generale", "Direzione Sanitaria", "Dipartimento tutela e salute animale", 
         "Dipartimento sicurezza alimentare","Dipartimento area territoriale Lombardia",
         "Dipartimento area territoriale Emilia Romagna",
         "Dipartimento amministrativo") %>% 
  arrange(Obiettivo) %>% 
  mutate(Obiettivo = as.character(Obiettivo)) %>% 
  mutate(Obiettivo = gsub("\\d+", "", Obiettivo), 
         Obiettivo = gsub("\\.", "", Obiettivo), 
         Obiettivo = gsub("\\)", "", Obiettivo),
         Obiettivo = gsub("\"", "", Obiettivo))
Obiettivi <- sapply(Obiettivi, as.character)

Obiettivi[is.na(Obiettivi)] <- ""   

Obiettivi %>% 
  kbl() %>% 
  kable_styling()



Indicatori <-   dt %>%  
  mutate(Indicatore = factor(Indicatore)) %>% 
  group_by(Dipartimento,  Indicatore) %>% 
  summarise(mediana = 100*round(median(Avanzamento, na.rm = T),2)) %>% 
  pivot_wider(names_from = "Dipartimento", values_from = "mediana") %>% 
  select("Indicatore","Direzione Generale", "Direzione Sanitaria", "Dipartimento tutela e salute animale", 
         "Dipartimento sicurezza alimentare","Dipartimento area territoriale Lombardia",
         "Dipartimento area territoriale Emilia Romagna",
         "Dipartimento amministrativo") %>% 
  arrange(Indicatore) %>% 
  mutate(Indicatore = as.character(Indicatore)) %>% 
  mutate(Indicatore = gsub("\\d+", "", Indicatore), 
         Indicatore = gsub("\\.", "", Indicatore), 
         Indicatore = gsub("\\)", "", Indicatore),
         Indicatore = gsub("\"", "", Indicatore))
Indicatori <- sapply(Indicatori, as.character)

Indicatori[is.na(Indicatori)] <- ""   

Indicatori %>% 
  kbl() %>% 
  kable_styling()


dt %>% 
  group_by(Azione) %>% 
  unique() %>% 
  count() %>% 
  count() %>%  View()

  
  
  
  # summarise(min= min(Avanzamento, na.rm = T), 
  #                    mediana = median(Avanzamento, na.rm = T), 
  #                                     media = mean(Avanzamento, na.rm = T),
  #                                     max = max(Avanzamento, na.rm = T))
  # 