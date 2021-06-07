library("tidyverse")
library("here")
library("readxl")
library("DBI")
library("odbc")
 
#library(dbplyr)

#DATI ORE LAVORATE DA DBASE PERSONALE_COGE####
con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbtest02", 
                      Database = "DW_COGE_DEV", Port = 1433)

query <- "SELECT
  dbo.IZS_Livello0.Livello0,
  dbo.IZS_Dipartimenti.DIPARTIMENTO,
  dbo.IZS_Reparti.REPARTO,
  dbo.IZS_CDC.CENTRO_DI_COSTO,
  dbo.Personale_V2020.Anno,
  dbo.Personale_V2020.Matricola,
  dbo.Personale_V2020.Ore,
  dbo.Personale_V2020.SmartWorking,
  dbo.Personale_V2020.Dirigente,
  dbo.Personale_V2020.Contratto,
  dbo.Personale_V2020.Percentuale,
  dbo.Personale_V2020.Mese,
  dbo.Personale_V2020.FineRapporto
FROM
  dbo.Personale_V2020 INNER JOIN dbo.IZS_CDC ON (dbo.Personale_V2020.CDC=dbo.IZS_CDC.CODICE_CDC)
   INNER JOIN dbo.IZS_Reparti ON (dbo.IZS_CDC.CODICE_REPARTO=dbo.IZS_Reparti.CODICE_REPARTO)
   INNER JOIN dbo.IZS_Dipartimenti ON (dbo.IZS_Reparti.CODICE_DIPARTIMENTO=dbo.IZS_Dipartimenti.CODICE_DIPARTIMENTO)
   INNER JOIN dbo.IZS_Livello0 ON (dbo.IZS_Dipartimenti.Codice_Livello0=dbo.IZS_Livello0.CODICE_Livello0)
  
WHERE
  dbo.Personale_V2020.Anno  >=  2019
"
con %>% tbl(sql(query)) %>% as_tibble() %>% 
  saveRDS(., file = here("COGES", "data", "processed",  "orelavorate.rds"))


tabstr <- readRDS(file = here("COGES", "data", "processed",  "orelavorate.rds") )

strutture <- tabstr %>% 
  select("Dipartimento" = Livello0, "Reparto" = DIPARTIMENTO, 
         "Laboratorio" = REPARTO, CENTRO_DI_COSTO) %>% 
  unique()
  #saveRDS(., file = here("COGES", "data", "processed",  "strutture.rds"))



#DATI DA CONTROLLO DI GESTIONE####

##ANALISI e Ricavi####
analisi <- read_excel(sheet = "Report 1", here("COGES", "data", "raw",  "analisi1921.xls"))
# il file analisi1921.xls deriva da una query eseguita in business object in sai-manager##

analisi %>% rename("CENTRO_DI_COSTO" =`Centro Di Costo`) %>% 
  select(-Reparto) %>% 
  left_join(strutture, by = c("CENTRO_DI_COSTO")) %>% 
  saveRDS(., file = here("COGES", "data", "processed",  "analisi.rds"))
  


##COSTI####

costi <- read_excel(sheet = "Report 1", here("COGES", "data", "raw",  "costi1921.xls"))
# il file costi1921.xls deriva da una query eseguita in business object in sai-manager##

costi %>% rename("CENTRO_DI_COSTO" =`Centro Di Costo`) %>% 
  select(-Reparto) %>% 
  left_join(strutture, by = c("CENTRO_DI_COSTO")) %>% View()
  saveRDS(., file = here("COGES", "data", "processed",  "costi.rds"))



##VENDITA PRODOTTI####

VP <- read_excel(sheet = "Report 1", here("COGES", "data", "raw",  "VP1921.xls"))
  # il file VP1921.xls  deriva da una query eseguita in business object in sai-manager##
  
VP %>% rename("CENTRO_DI_COSTO" =`Centro Di Costo`) %>% 
  left_join(strutture, by = c("CENTRO_DI_COSTO")) %>% 
  saveRDS(., file = here("COGES", "data", "processed",  "vp.rds"))

##ATTIVITA' INTERNA####

AI <- read_excel(sheet = "Report 1", here("COGES", "data", "raw",  "AI1921.xls"))
  # il file AI1921.xls deriva da una query eseguita in business object in sai-manager##

AI %>% 
  rename("CENTRO_DI_COSTO" =`Centro Di Costo`) %>% 
  left_join(strutture, by = c("CENTRO_DI_COSTO")) %>% 
  saveRDS(., file = here("COGES", "data", "processed",  "ai.rds"))




#DATI DA PROGETTI DI RICERCA####


#DATI DA PUBBLICAZIONI####


#DATI DA DBASE PERFORMANCE (OBIETTIVI, INDICATORI, TARGET, RISULTATO, FTEQ PROGRAMMATI)####











# attività2019 <- read_excel(sheet = "reparti", here("programmazione", "NUOVA VERSIONE", "data", "raw", "attivita2019.xlsx"))
# attività2020 <- read_excel(sheet = "Foglio1", here("programmazione", "NUOVA VERSIONE", "data", "raw", "attivita2020.xlsx"))



#####Dati attività interna e vendita prodotti by dip/rep###
# attività2020 %>% 
#   group_by(Reparto) %>% 
#   summarise(
#             VP = sum(`vendita prodotti`, na.rm = TRUE),
#             AI = sum(`attività interna`, na.rm = TRUE)) %>% 
#   mutate(Anno = rep(2020, nrow(.))) %>% 
#   
#   bind_rows( 
# 
# attività2019 %>% 
#   group_by(Reparto) %>% 
#   summarise(  
#             VP = sum(`vendita prodotti`, na.rm = TRUE),
#             AI = sum(`attività interna`, na.rm = TRUE)) %>% 
#   mutate(Anno = rep(2019, nrow(.)))
# 
#   ) %>% 
#   #filter(Reparto != "ANALISI DEL RISCHIO ED EPIDEMIOLOGIA GENOMICA") %>% 
#   mutate(Reparto = recode(Reparto, "VIROLOGIA" = "REPARTO VIROLOGIA",
#                           "VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE",
#                           "TECNOLOGIE BIOLOGICHE APPLICATE" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE", 
#                           "PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO",
#                           "CONTROLLO ALIMENTI" = "REPARTO CONTROLLO ALIMENTI",
#                           "PRODUZIONE PRIMARIA" = "REPARTO PRODUZIONE PRIMARIA",
#                           "CHIMICO DEGLI ALIMENTI E MANGIMI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI",
#                           "CHIMICO DEGLI ALIMENTI (BOLOGNA)" = "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)",
#                           "BERGAMO - BINAGO - SONDRIO" = "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO",
#                           "CREMONA - MANTOVA" = "SEDE TERRITORIALE DI CREMONA - MANTOVA",
#                           "PAVIA" = "SEDE TERRITORIALE DI PAVIA",
#                           "LODI - MILANO" = "SEDE TERRITORIALE DI LODI - MILANO",
#                           "BRESCIA" = "SEDE TERRITORIALE DI BRESCIA", 
#                           "BOLOGNA - MODENA - FERRARA" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA",
#                           "FORLI' - RAVENNA" = "SEDE TERRITORIALE DI FORLÌ - RAVENNA",
#                           "PIACENZA - PARMA" = "SEDE TERRITORIALE DI PIACENZA - PARMA",
#                           "REGGIO EMILIA" = "SEDE TERRITORIALE DI REGGIO EMILIA"), 
#          Dipartimento = recode (Reparto, "REPARTO VIROLOGIA" = "Dipartimento Tutela e  Salute Animale", 
#                                 "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE" = "Dipartimento Tutela e  Salute Animale",
#                                 "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO" = "Dipartimento Tutela e  Salute Animale",
#                                 "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "Dipartimento Tutela e  Salute Animale", 
#                                 "REPARTO PRODUZIONE PRIMARIA" = "Dipartimento Sicurezza Alimentare", 
#                                 "REPARTO CONTROLLO ALIMENTI" = "Dipartimento Sicurezza Alimentare",
#                                 "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI" = "Dipartimento Sicurezza Alimentare", 
#                                 "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)" = "Dipartimento Sicurezza Alimentare", 
#                                 "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO" = "Area Territoriale Lombardia",
#                                 "SEDE TERRITORIALE DI BRESCIA" = "Area Territoriale Lombardia",
#                                 "SEDE TERRITORIALE DI PAVIA" = "Area Territoriale Lombardia",
#                                 "SEDE TERRITORIALE DI CREMONA - MANTOVA" = "Area Territoriale Lombardia", 
#                                 "SEDE TERRITORIALE DI LODI - MILANO" = "Area Territoriale Lombardia", 
#                                 "SEDE TERRITORIALE DI FORLÌ - RAVENNA" = "Area Territoriale Emilia Romagna", 
#                                 "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA" = "Area Territoriale Emilia Romagna", 
#                                 "SEDE TERRITORIALE DI PIACENZA - PARMA" = "Area Territoriale Emilia Romagna", 
#                                 "SEDE TERRITORIALE DI REGGIO EMILIA" = "Area Territoriale Emilia Romagna", 
#                                 "ANALISI DEL RISCHIO ED EPIDEMIOLOGIA GENOMICA" = "Direzione Sanitaria")) %>% 
#   saveRDS(., file = here("programmazione", "NUOVA VERSIONE",  "shinyapp", "vpai.rds"))
# 
# 
# #####Dati esami e ricavi by dip/rep/lab###
# attività2020 %>% 
#   group_by(Reparto, Laboratorio) %>% 
#   summarise(
#     N.esami = sum(n.esami, na.rm = TRUE),
#     Ricavi = sum(valore, na.rm = TRUE)) %>% 
#   ungroup() %>%  
#   mutate(Anno = rep(2020,nrow(.) )) %>% 
#   
#   bind_rows( 
#     
#     attività2019 %>% 
#       group_by(Reparto, Laboratorio) %>% 
#       summarise(  
#         N.esami = sum(n.esami, na.rm = TRUE),
#         Ricavi = sum(valore, na.rm = TRUE))  %>% 
#       ungroup() %>% 
#       mutate(Anno = rep(2019, nrow(.)))  
#   ) %>% 
#   mutate(Reparto = recode(Reparto, "VIROLOGIA" = "REPARTO VIROLOGIA", 
#                           "TECNOLOGIE BIOLOGICHE APPLICATE"= "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE", 
#                           "PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO", 
#                           "VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE",
#                           "PRODUZIONE PRIMARIA" = "REPARTO PRODUZIONE PRIMARIA", 
#                           "CHIMICO DEGLI ALIMENTI E MANGIMI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI", 
#                           "CHIMICO DEGLI ALIMENTI (BOLOGNA)" = "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)", 
#                           "CONTROLLO ALIMENTI" = "REPARTO CONTROLLO ALIMENTI", 
#                           "BERGAMO - BINAGO - SONDRIO" = "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO", 
#                           "BRESCIA" = "SEDE TERRITORIALE DI BRESCIA", 
#                           "PAVIA" = "SEDE TERRITORIALE DI PAVIA", 
#                           "CREMONA - MANTOVA" =  "SEDE TERRITORIALE DI CREMONA - MANTOVA", 
#                           "LODI - MILANO" = "SEDE TERRITORIALE DI LODI - MILANO", 
#                           "FORLI' - RAVENNA" = "SEDE TERRITORIALE DI FORLÌ - RAVENNA", 
#                           "BOLOGNA - MODENA - FERRARA" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA", 
#                           "PIACENZA - PARMA" = "SEDE TERRITORIALE DI PIACENZA - PARMA", 
#                           "REGGIO EMILIA" = "SEDE TERRITORIALE DI REGGIO EMILIA" )) %>% 
#   #filter(Reparto != "ANALISI DEL RISCHIO ED EPIDEMIOLOGIA GENOMICA") %>% 
#   mutate(Laboratorio = recode(Laboratorio,"Bologna" = "SEDE TERRITORIALE DI BOLOGNA", 
#                               "Modena" = "SEDE TERRITORIALE DI MODENA", 
#                               "Ferrara" = "SEDE TERRITORIALE DI FERRARA", 
#                               "Bergamo" = "SEDE TERRITORIALE DI BERGAMO", 
#                               "Binago" = "SEDE TERRITORIALE DI BINAGO", 
#                               "Sondrio" = "SEDE TERRITORIALE DI SONDRIO", 
#                               "Brescia" = "SEDE TERRITORIALE DI BRESCIA", 
#                               "Chimico degli Alimenti (Bologna)" = "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)", 
#                               "Chimica Applicata alle Tecnologie Alimentari" = "LABORATORIO CHIMICA APPLICATA ALLE TECNOLOGIE ALIMENTARI",
#                               "Contaminanti Ambientali" = "LABORATORIO CONTAMINANTI AMBIENTALI", 
#                               "Mangimi e Tossicologia" = "LABORATORIO MANGIMI E TOSSICOLOGIA", 
#                               "Residui" = "LABORATORIO RESIDUI", 
#                               "Controllo Alimenti" = "REPARTO CONTROLLO ALIMENTI", 
#                               "Cremona" = "SEDE TERRITORIALE DI CREMONA", 
#                               "Mantova" = "SEDE TERRITORIALE DI MANTOVA", 
#                               "Forlì" = "SEDE TERRITORIALE DI FORLÌ", 
#                               "Ravenna" = "SEDE TERRITORIALE DI RAVENNA", 
#                               "Lodi" = "SEDE TERRITORIALE DI LODI", 
#                               "Milano" = "SEDE TERRITORIALE DI MILANO", 
#                               "Pavia" = "SEDE TERRITORIALE DI PAVIA", 
#                               "Parma" = "SEDE TERRITORIALE DI PARMA", 
#                               "Piacenza" = "SEDE TERRITORIALE DI PIACENZA", 
#                               "Reggio Emilia" = "SEDE TERRITORIALE DI REGGIO EMILIA", 
#                               "Benessere Animale, Biochimica Clinica, Immunologia Veterinaria e Stabulari" = "LABORATORIO BENESSERE ANIMALE, BIOCHIMICA CLINICA, IMMUNOLOGIA VETERINARIA E STABULARI", 
#                               "Controllo di Prodotti Biologici, Farmaceutici e Convalida dei Processi Produttivi" = "LABORATORIO DI CONTROLLO DI PRODOTTI BIOLOGICI, FARMACEUTICI E CONVALIDA DI PROCESSI PRODUTTIVI", 
#                               "Produzione Terreni" = "LABORATORIO PRODUZIONE TERRENI", 
#                               "Produzione Vaccini e Reagenti" = "LABORATORIO PRODUZIONE VACCINI E REAGENTI", 
#                               "Produzione Primaria" = "REPARTO PRODUZIONE PRIMARIA", 
#                               "Analisi Genomiche, Diagnostica Molecolare, OGM" = "LABORATORIO ANALISI GENOMICHE, LABORATORIO DIAGNOSTICA MOLECOLARE, OGM", 
#                               "Batteriologia Specializzata" = "LABORATORIO BATTERIOLOGIA SPECIALIZZATA", 
#                               "Colture Cellulari, Biobanca" = "LABORATORIO COLTURE CELLULARI, BIOBANCA", 
#                               "Proteomica" = "LABORATORIO DI PROTEOMICA E DIAGNOSTICA TSE", 
#                               "Virologia" = "LABORATORIO DI VIROLOGIA E SIEROLOGIA SPECIALIZZATA, MICROSCOPIA ELETTRONICA", 
#                               "Virus Vescicolari e Produzioni Biotecnologiche" = "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE"), 
#          Dipartimento = recode (Reparto, "REPARTO VIROLOGIA" = "Dipartimento Tutela e  Salute Animale", 
#                                 "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE" = "Dipartimento Tutela e  Salute Animale",
#                                 "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO" = "Dipartimento Tutela e  Salute Animale",
#                                 "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "Dipartimento Tutela e  Salute Animale", 
#                                 "REPARTO PRODUZIONE PRIMARIA" = "Dipartimento Sicurezza Alimentare", 
#                                 "REPARTO CONTROLLO ALIMENTI" = "Dipartimento Sicurezza Alimentare",
#                                 "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI" = "Dipartimento Sicurezza Alimentare", 
#                                 "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)" = "Dipartimento Sicurezza Alimentare", 
#                                 "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO" = "Area Territoriale Lombardia",
#                                 "SEDE TERRITORIALE DI BRESCIA" = "Area Territoriale Lombardia",
#                                 "SEDE TERRITORIALE DI PAVIA" = "Area Territoriale Lombardia",
#                                 "SEDE TERRITORIALE DI CREMONA - MANTOVA" = "Area Territoriale Lombardia", 
#                                 "SEDE TERRITORIALE DI LODI - MILANO" = "Area Territoriale Lombardia", 
#                                 "SEDE TERRITORIALE DI FORLÌ - RAVENNA" = "Area Territoriale Emilia Romagna", 
#                                 "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA" = "Area Territoriale Emilia Romagna", 
#                                 "SEDE TERRITORIALE DI PIACENZA - PARMA" = "Area Territoriale Emilia Romagna", 
#                                 "SEDE TERRITORIALE DI REGGIO EMILIA" = "Area Territoriale Emilia Romagna", 
#                                 "ANALISI DEL RISCHIO ED EPIDEMIOLOGIA GENOMICA" = "Direzione Sanitaria")) %>% 
#   saveRDS(., file = here("programmazione", "NUOVA VERSIONE",  "shinyapp", "esamiricavi.rds"))
# 
# 
# 
# 
# 


#####Dati costi by dip/rep###

####DATI ORE LAVORATE MATRICOLE BY DIP/REP###

# anag <-read_excel(sheet = "anagrafe", here("programmazione", "NUOVA VERSIONE", "data", "raw", "anagrafe.xlsx"))
# 
# anag <- mutate(REPARTO = recode(dbo_AD_Anagrafe_GRU.REPARTO,
#                         "SEDE TERRITORIALE BERGAMO" = "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO",
#                         "SEDE TERRITORIALE DI BINAGO" = "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO",
#                         "SEDE TERRITORIALE SONDRIO" = "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO",
#                         "SEDE TERRITORIALE DI CREMONA" = "SEDE TERRITORIALE DI CREMONA - MANTOVA",
#                         "SEDE TERRITORIALE DI MANTOVA" = "SEDE TERRITORIALE DI CREMONA - MANTOVA",
#                         "SEDE TERRITORIALE DI LODI" = "SEDE TERRITORIALE DI LODI - MILANO",
#                         "SEDE TERRITORIALE DI MILANO" = "SEDE TERRITORIALE DI LODI - MILANO",
#                         "LAB. DI ISTOLOGIA (MI)" = "SEDE TERRITORIALE DI LODI - MILANO",
#                         "SEDE TERRITORIALE DI BOLOGNA" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA",
#                         "SEDE TERRITORIALE DI MODENA" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA",
#                         "SEDE TERRITORIALE DI FERRARA" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA",
#                         "SEDE TERRITORIALE DI FORLI'" = "SEDE TERRITORIALE DI FORLÌ - RAVENNA",
#                         "SEDE TERRITORIALE DI RAVENNA" = "SEDE TERRITORIALE DI FORLÌ - RAVENNA",
#                         "SEDE TERRITORIALE DI PIACENZA" = "SEDE TERRITORIALE DI PIACENZA - PARMA",
#                         "SEDE TERRITORIALE DI PARMA" = "SEDE TERRITORIALE DI PIACENZA - PARMA",
#                         "LAB. CHIM. APPLICATA ALLE TECNOLOGIE ALIMENTARI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI",
#                         "LAB. BATTERIOLOGIA SPECIALIZZATA" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE",
#                         "LABORATORIO ANALISI GENOMICHE, LABORATORIO DIAGNOSTICA MOLECOLARE, OGM" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE",
#                         "REP.TECNOLOGIE BIOLOGICHE APPLICATE" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE",
#                         "LAB. DIAGNOSTICA MOLECOLARE E OGM" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE",
#                         "REP. VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE",
#                         "LAB. CONTAMINANTI AMBIENTALI (BRESCIA)" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI",
#                         "LAB. PRODUZIONE TERRENI" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO",
#                         "SERVIZIO PREPARAZIONE TERRENI E REAGENTI" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO",
#                         "REP. PRODUZIONE E CONTR. MAT. BIOLOGICO" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO",
#                         "LABORATORIO BENESSERE ANIMALE, BIOCHIMICA CLINICA, IMMUNOLOGIA VETERINARIA E STABULARI" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO",
#                         "LABORATORIO CONTAMINANTI AMBIENTALI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI",
#                         "REP. CHIMICA DEGLI ALIMENTI E MANGIMI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI",
#                         "REP. PRODUZIONE PRIMARIA" = "REPARTO PRODUZIONE PRIMARIA",
#                         "LAB. CONTAMINANTI AMBIENTALI (BO)" = "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)",
#                         "REP. CHIMICO DEGLI ALIMENTI (BOLOGNA)" = "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)",
#                         "REP.CONTROLLO DEGLI ALIMENTI" = "REPARTO CONTROLLO ALIMENTI",
#                         "LAB. VIROLOGIA SIEROLOGIA SPEC. E MICROS. ELETT." = "REPARTO VIROLOGIA",
#                         "REP. PROD. VACCINI E REAGENTI" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO",
#                         "LAB. PROTEOMICA E DIAGNOSTICA TSE" = "REPARTO VIROLOGIA",
#                         "LABORATORIO COLTURE CELLULARI, BIOBANCA" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE",
#                         "REP. SUBSTRATI CELLULARI E IMMUNOL.CELL." = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE",
#                         "REPARTO SUBSTRATI CELLULARI" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE",
#                         "FORMAZIONE" = "FORMAZIONE BIBLIOTECA COMUNICAZIONE",
#                         "U.O. GESTIONE ECONOMICO FINANZIARIA" = "U.O. GESTIONE SERVIZI CONTABILI",
#                         "U.O. ECONOMICO FINANZIARIA" = "U.O. GESTIONE SERVIZI CONTABILI",
#                         "U.O. SERVIZI GENERALI" = "U.O. TECNICO PATRIMONIALE",
#                         "U.O. GESTIONE DEL PERSONALE" = "U.O. GESTIONE RISORSE UMANE E SVILUPPO COMPETENZE",
#                         "DIREZIONE GENERALE SANITARIA AMMIN.VA" = "Direzione Generale",
#                         "CONTROLLO DI GESTIONE" = "Ufficio Controllo di Gestione e Performance",
#                         "U.O. PROGETTI DI RICERCA" = "U.O. AFFARI GENERALI E LEGALI",
#                         "SERVIZIO ASSICURAZIONE QUALITA' (2)" = "SERVIZIO ASSICURAZIONE QUALITA'",
#                         "FORM.SIS.DOC.C.R.N.FORM.SAN.PUBB.VET." = "FORMAZIONE BIBLIOTECA COMUNICAZIONE"
# ),
# Dipartimento = recode (REPARTO, "REPARTO VIROLOGIA" = "Dipartimento Tutela e  Salute Animale",
#                        "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE" = "Dipartimento Tutela e  Salute Animale",
#                        "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO" = "Dipartimento Tutela e  Salute Animale",
#                        "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "Dipartimento Tutela e  Salute Animale",
#                        "REPARTO PRODUZIONE PRIMARIA" = "Dipartimento Sicurezza Alimentare",
#                        "REPARTO CONTROLLO ALIMENTI" = "Dipartimento Sicurezza Alimentare",
#                        "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI" = "Dipartimento Sicurezza Alimentare",
#                        "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)" = "Dipartimento Sicurezza Alimentare",
#                        "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO" = "Area Territoriale Lombardia",
#                        "SEDE TERRITORIALE DI BRESCIA" = "Area Territoriale Lombardia",
#                        "SEDE TERRITORIALE DI PAVIA" = "Area Territoriale Lombardia",
#                        "SEDE TERRITORIALE DI CREMONA - MANTOVA" = "Area Territoriale Lombardia",
#                        "SEDE TERRITORIALE DI LODI - MILANO" = "Area Territoriale Lombardia",
#                        "SEDE TERRITORIALE DI FORLÌ - RAVENNA" = "Area Territoriale Emilia Romagna",
#                        "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA" = "Area Territoriale Emilia Romagna",
#                        "SEDE TERRITORIALE DI PIACENZA - PARMA" = "Area Territoriale Emilia Romagna",
#                        "SEDE TERRITORIALE DI REGGIO EMILIA" = "Area Territoriale Emilia Romagna",
#                        "ANALISI DEL RISCHIO E EPIDEMILOGIA GENOMICA" = "Direzione Sanitaria",
#                        "Ufficio Controllo di Gestione e Performance" = "Direzione Generale",
#                        "DIREZIONE AMMINISTRATIVA" = "Direzione Ammninistrativa",
#                        "DIREZIONE GENERALE" = "Direzione Generale",
#                        "DIREZIONE SANITARIA" = "Direzione Sanitaria",
#                        "FORMAZIONE BIBLIOTECA COMUNICAZIONE" = "Direzione Sanitaria",
#                        "GARE CONTRATTI PER ACQUISTO DI BENI E SERVIZI, MAGAZZINO E VENDITE, UFFICIO SERVIZI" = "Direzione Ammninistrativa",
#                        "GESTIONE CENTRALIZZATA DELLE RICHIESTE DELL'UTENZA" = "Direzione Sanitaria",
#                        "PROGETTAZIONE E DIREZIONE LAVORI MANUTENZIONI" = "Direzione Ammninistrativa",
#                        "PROGETTI DI RICERCA" = "Direzione Generale",
#                        "SERVIZIO ASSICURAZIONE QUALITA'" = "Direzione Generale",
#                        "SISTEMI INFORMATIVI" = "Direzione Generale",
#                        "SORVEGLIANZA EPIDEMIOLOGICA" = "Direzione Sanitaria",
#                        "U.O. AFFARI GENERALI E LEGALI" = "Dipartimento Amministrativo",
#                        "U.O. GESTIONE RISORSE UMANE E SVILUPPO COMPETENZE" = "Dipartimento Amministrativo",
#                        "U.O. GESTIONE SERVIZI CONTABILI" = "Dipartimento Amministrativo",
#                        "U.O. TECNICO PATRIMONIALE" =  "Dipartimento Amministrativo",
#                        "U.O. PROVVEDITORATO ECONOMATO E VENDITE" = "Dipartimento Amministrativo",
#                        
# )
# 
# )%>%
#   mutate(Dipartimento = toupper(Dipartimento) ) %>% 
#   select(Dipartimento, REPARTO, CENTRO_DI_COSTO, Dirigente,
#          Matricola, Nome, Cognome, InizioRapporto, FineRapporto) %>% View()

# %>% 
#   filter(!is.na(Dipartimento)) %>% 
####DATI PROGETTI DI RICERCA BY DIP/REP###

####DATI PUBBLICAZIONI BY DIP/REP###

####DATI DA PERFORMANCES DA SCHEDA BUDGET###
  