library(tidyverse)
library(readr)
library(readxl)
library(here)
library(lubridate)
library(DBI)
library(odbc)


#DATI ORE LAVORATE DA DBASE PERSONALE_COGE####
con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbtest02", 
                      Database = "DW_COGE_DEV", Port = 1433)
queryOre <- "SELECT
  dbo.IZS_Livello0.Livello0,
  dbo.IZS_Dipartimenti.DIPARTIMENTO,
  dbo.IZS_Reparti.REPARTO,
  dbo.IZS_CDC.CENTRO_DI_COSTO,
  dbo.Personale_V2020.CDC,
  dbo.Personale_V2020.Anno,
  dbo.Personale_V2020.Matricola,
  dbo.Personale_V2020.Ore,
  dbo.Personale_V2020.SmartWorking,
  dbo.Personale_V2020.Dirigente,
  dbo.Personale_V2020.Contratto,
  dbo.Personale_V2020.Percentuale,
  dbo.Personale_V2020.Mese,
  dbo.Personale_V2020.FineRapporto,
  dbo.Personale_V2020.Nome,
  dbo.Personale_V2020.Cognome
FROM
  dbo.Personale_V2020 INNER JOIN dbo.IZS_CDC ON (dbo.Personale_V2020.CDC=dbo.IZS_CDC.CODICE_CDC)
   INNER JOIN dbo.IZS_Reparti ON (dbo.IZS_CDC.CODICE_REPARTO=dbo.IZS_Reparti.CODICE_REPARTO)
   INNER JOIN dbo.IZS_Dipartimenti ON (dbo.IZS_Reparti.CODICE_DIPARTIMENTO=dbo.IZS_Dipartimenti.CODICE_DIPARTIMENTO)
   INNER JOIN dbo.IZS_Livello0 ON (dbo.IZS_Dipartimenti.Codice_Livello0=dbo.IZS_Livello0.CODICE_Livello0)
  
WHERE
  dbo.Personale_V2020.Anno  >=  2019"
#ore <- con %>% tbl(sql(query)) %>% as_tibble()  


#DATI DA COGE----
# ccx <- read_delim(here("data", "raw", "coge1921.txt"), 
               # "\t", escape_double = FALSE, locale = locale(decimal_mark = ",",
                                                              # grouping_mark = "."), trim_ws = TRUE)

# names(cc)[5:7] <- c("Dipartimento", "Reparto", "Laboratorio")
# 
# names(cc)[20:21] <- c("Centro di Costo", "CodCC")


# Dati da Controllo di Gestione per Dashboard Performance e APP Centri di Costo
queryCoge <- "SELECT IZS_ANNI.ANNO, IZS_TRIMESTRI.TRIMESTRE, IZS_MESI.MESE, IZS_MESI.Descrizione, 
                  IZS_Livello0.Livello0 AS Dipartimento, 
                  IZS_Dipartimenti.DIPARTIMENTO As Reparto, 
                  IZS_Reparti.REPARTO AS Laboratorio, 
                  SUM(CDC_MOVIMENTI_BO.Reale) AS Fatturato, 
                  SUM(CDC_MOVIMENTI_BO.Nominale) AS Tariffario, 
                  SUM(CDC_MOVIMENTI_BO.Costo) AS Costo, 
                  SUM(CDC_MOVIMENTI_BO.Determinazioni) AS Determinazioni, 
                  SUM(CDC_MOVIMENTI_BO.Quantita) AS Numero, 
                  IZS_Categorie.Descrizione AS Categoria, 
                  IZS_Riclassificazione.Descrizione AS Classificazione, 
                  IZS_Riclassificazione.idClassificazione, 
                  Elenco_Tipi_Movimenti.Descrizione AS Costi, 
                  IZS_Classi.Descrizione AS Classe, 
                  IZS_Aree.Descrizione AS Area, 
                  IZS_CDC.CODICE_CDC AS CodiceCDC, 
                  IZS_CDC.CENTRO_DI_COSTO AS CDC
FROM              IZS_Categorie INNER JOIN
                  IZS_Classi ON IZS_Categorie.TipoCostoRicavo = IZS_Classi.TipoCostoRicavo AND IZS_Categorie.Codice = IZS_Classi.Codice_categoria INNER JOIN
                  IZS_Aree ON IZS_Classi.TipoCostoRicavo = IZS_Aree.TipoCostoRicavo AND IZS_Classi.Codice = IZS_Aree.Codice_classe INNER JOIN
                  CDC_MOVIMENTI_BO ON IZS_Aree.TipoCostoRicavo = CDC_MOVIMENTI_BO.TipoCostoRicavo AND IZS_Aree.Codice_classe = CDC_MOVIMENTI_BO.Classe AND IZS_Aree.Codice_area = CDC_MOVIMENTI_BO.Area INNER JOIN
                  IZS_Classificazioni ON IZS_Classificazioni.idClassificazione = CDC_MOVIMENTI_BO.IdClassificazione INNER JOIN
                  IZS_Riclassificazione ON IZS_Riclassificazione.idClassificazione = IZS_Classificazioni.idRiclassifica INNER JOIN
                  Elenco_Tipi_Movimenti ON CDC_MOVIMENTI_BO.TipoCostoRicavo = Elenco_Tipi_Movimenti.TipoMovimento INNER JOIN
                  IZS_CDC ON IZS_CDC.CODICE_CDC = CDC_MOVIMENTI_BO.CDC INNER JOIN
                  IZS_Reparti ON IZS_Reparti.CODICE_REPARTO = IZS_CDC.CODICE_REPARTO INNER JOIN
                  IZS_Dipartimenti ON IZS_Dipartimenti.CODICE_DIPARTIMENTO = IZS_Reparti.CODICE_DIPARTIMENTO INNER JOIN
                  IZS_Livello0 ON IZS_Livello0.CODICE_Livello0 = IZS_Dipartimenti.Codice_Livello0 INNER JOIN
                  IZS_ANNI ON IZS_ANNI.ANNO = CDC_MOVIMENTI_BO.ANNO INNER JOIN
                  IZS_MESI ON IZS_MESI.MESE = CDC_MOVIMENTI_BO.MESE INNER JOIN
                  IZS_TRIMESTRI ON IZS_TRIMESTRI.TRIMESTRE = CDC_MOVIMENTI_BO.TRIMESTRE
                  
WHERE  (IZS_ANNI.ANNO >= 2019)
GROUP BY IZS_ANNI.ANNO, IZS_TRIMESTRI.TRIMESTRE, IZS_MESI.MESE, IZS_MESI.Descrizione, IZS_Livello0.Livello0, IZS_Dipartimenti.DIPARTIMENTO, IZS_Reparti.REPARTO, IZS_Categorie.Descrizione, IZS_Riclassificazione.Descrizione, 
                  IZS_Riclassificazione.idClassificazione, Elenco_Tipi_Movimenti.Descrizione, IZS_Classi.Descrizione, IZS_Aree.Descrizione, IZS_CDC.CODICE_CDC, IZS_CDC.CENTRO_DI_COSTO"
cc <- con %>% tbl(sql(queryCoge)) %>% as_tibble() 







#PREPARAZIONE DATI PER DASHBOARD PERFORMANCES----

###TABELLE-----
T1 <- cc %>% #tabella con prestazioni (tariffato, fatturato) e costi
  group_by(ANNO, Dipartimento, Reparto, Laboratorio,  Categoria, Classe, Area, Classificazione) %>% 
  summarise(Prestazioni = sum(Determinazioni, na.rm = TRUE), 
            Tariffato = sum(Tariffario, na.rm=TRUE), 
            Fatturato = sum (Fatturato, na.rm = TRUE), 
            costi = sum(Costo, na.rm = TRUE)) %>% 
  group_by(ANNO, Dipartimento, Reparto, Laboratorio) %>% 
  summarise(TotPrestazioni = sum(Prestazioni), 
            TotCost = sum(costi), 
            TotTariff = sum(Tariffato)) 

T2 <- cc %>% filter(Classe== "Vendite prodotti") %>% ###vendita prodotti
  group_by(ANNO, Dipartimento, Reparto, Laboratorio,  Categoria, Classe, Area, Classificazione) %>% 
  mutate(Fatturato = ifelse(Fatturato == 0, Tariffario, Fatturato )) %>% 
  summarise(NVP = sum(Numero, na.rm = TRUE), 
            FattVP = sum(Fatturato, na.rm = TRUE)) %>% 
  group_by(ANNO, Dipartimento, Reparto, Laboratorio) %>% 
  summarise(TotNVP = sum(NVP), 
            TotFattVP = sum(FattVP))


T3 <- cc %>% filter(Classe== "Ricavi da produzione") %>% ###attività interna
  group_by(ANNO, Dipartimento, Reparto, Laboratorio,  Categoria, Classe, Area, Classificazione) %>% 
  summarise(NumAI = sum(Numero, na.rm = TRUE), 
            TarAI = sum(Tariffario, na.rm = TRUE)) %>% 
  group_by(ANNO, Dipartimento, Reparto, Laboratorio) %>% 
  summarise(TotNAI = sum(NumAI), 
            TAI = sum(TarAI)) 

ore <- con %>% tbl(sql(queryOre)) %>% as_tibble()  ### FTEq
names(ore)[1:6] <- c("Dipartimento", "Reparto", "Laboratorio", "CDC", "CodiceCC", "ANNO")
fte <- ore %>% 
  mutate(Dirigente = recode(Dirigente, N = "Comparto", S = "Dirigenza")) %>% 
  filter(Dipartimento != "Non applicabile") %>% 
  group_by(ANNO, Dipartimento, Reparto, Laboratorio, Dirigente) %>%   
  filter(!is.na(Dirigente) & !is.na(Ore)) %>% 
  summarise(hworked = sum(Ore, na.rm = T)) %>% 
  mutate(FTE = ifelse(Dirigente == "Comparto", hworked/(38*47.4), hworked/(36*47.4))) %>% 
  pivot_wider(names_from = "Dirigente", values_from = c("hworked", "FTE"))  %>% 
  select(-hworked_, -FTE_)  

##TABELLA GENERALE----
T1 %>% ##attività costi e fte
  left_join(T2, by=c("ANNO", "Dipartimento", "Reparto", "Laboratorio")) %>%  
  left_join(T3, by=c("ANNO", "Dipartimento", "Reparto", "Laboratorio")) %>% 
  left_join(fte,by=c("ANNO", "Dipartimento", "Reparto", "Laboratorio")) %>% 
  saveRDS(., file = here("data", "processed",  "TabellaGenerale.rds"))



##DATI GESTIONE CENTRALIZZATA DELLE RICHIESTE DELL'UTENZA----
conAcc <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbprod02.izsler.it", 
                      Database = "IZSLER", Port = 1433)



queryAcc <- ("SELECT
  {fn year(dbo.Conferimenti.Data_Accettazione)} AS Anno,
  dbo.Conferimenti.Nome_Stazione_Inserimento AS PC,
  dbo.Conferimenti.Numero AS Nconf,
  dbo_Anag_Reparti_ConfProp.Descrizione AS StrPropr,
  dbo_Anag_Reparti_ConfAcc.Descrizione AS StrAcc,
  dbo_Operatori_di_sistema_ConfMatr.Descr_Completa AS Operatore,
  dbo_Anag_Reparti_ConfProp.Locazione AS LocStrutt,
  dbo_Anag_Finalita_Confer.Descrizione AS Finalità,
  dbo.Anag_Registri.Descrizione AS Settore,
  dbo.Anag_TipoConf.Descrizione AS Pagamento,
  dbo.Conferimenti.Data_Inserimento AS dtreg,
  dbo.Anag_Specie.Descrizione AS Specie ,
  dbo.Anag_Materiali.Descrizione AS Materiale,
  dbo.Anag_Matrici.Descrizione AS Matrice,
  dbo.Anag_Supergruppo_Specie.Descrizione AS SupergruppoSpecie,
  dbo.Anag_Gruppo_Specie.Descrizione AS GruppoSpecie,
  dbo.Anag_Prove.Descrizione AS Prova,
  dbo.Anag_Tipo_Prel.Descrizione AS Tipoprel,
  dbo.RDP_Date_Emissione.Istanza_RDP AS Istanzardp,
  convert (SMALLDATETIME, dbo.Conferimenti.Data_Primo_RDP_Completo_Firmato) AS dtprimotrdp,
  dbo.RDP_Date_Emissione.Data_RDP AS dturp,
  Datename(weekday, dbo.Conferimenti.Data_Accettazione) AS Giornoacc,
  dbo.Conferimenti.NrCampioni
FROM
{ oj dbo.Anag_Reparti  dbo_Anag_Reparti_ConfProp INNER JOIN dbo.Laboratori_Reparto  dbo_Laboratori_Reparto_ConfProp ON ( dbo_Laboratori_Reparto_ConfProp.Reparto=dbo_Anag_Reparti_ConfProp.Codice )
   INNER JOIN dbo.Conferimenti ON ( dbo.Conferimenti.RepLab=dbo_Laboratori_Reparto_ConfProp.Chiave )
   LEFT OUTER JOIN dbo.Anag_Matrici ON ( dbo.Conferimenti.Matrice=dbo.Anag_Matrici.Codice )
   LEFT OUTER JOIN dbo.Esami_Aggregati ON ( dbo.Conferimenti.Anno=dbo.Esami_Aggregati.Anno_Conferimento and dbo.Conferimenti.Numero=dbo.Esami_Aggregati.Numero_Conferimento )
   LEFT OUTER JOIN dbo.Nomenclatore_MP ON ( dbo.Esami_Aggregati.Nomenclatore=dbo.Nomenclatore_MP.Codice )
   LEFT OUTER JOIN dbo.Nomenclatore_Settori ON ( dbo.Nomenclatore_MP.Nomenclatore_Settore=dbo.Nomenclatore_Settori.Codice )
   LEFT OUTER JOIN dbo.Nomenclatore ON ( dbo.Nomenclatore_Settori.Codice_Nomenclatore=dbo.Nomenclatore.Chiave )
   LEFT OUTER JOIN dbo.Anag_Prove ON ( dbo.Nomenclatore.Codice_Prova=dbo.Anag_Prove.Codice )
   INNER JOIN dbo.Anag_Tipo_Prel ON ( dbo.Conferimenti.Tipo_Prelievo=dbo.Anag_Tipo_Prel.Codice )
   INNER JOIN dbo.Anag_Registri ON ( dbo.Conferimenti.Registro=dbo.Anag_Registri.Codice )
   INNER JOIN dbo.Laboratori_Reparto  dbo_Laboratori_Reparto_ConfAcc ON ( dbo.Conferimenti.RepLab_Conferente=dbo_Laboratori_Reparto_ConfAcc.Chiave )
   INNER JOIN dbo.Anag_Reparti  dbo_Anag_Reparti_ConfAcc ON ( dbo_Laboratori_Reparto_ConfAcc.Reparto=dbo_Anag_Reparti_ConfAcc.Codice )
   INNER JOIN dbo.Anag_TipoConf ON ( dbo.Anag_TipoConf.Codice=dbo.Conferimenti.Tipo )
   LEFT OUTER JOIN dbo.Anag_Materiali ON ( dbo.Anag_Materiali.Codice=dbo.Conferimenti.Codice_Materiale )
   LEFT OUTER JOIN dbo.Anag_Specie ON ( dbo.Anag_Specie.Codice=dbo.Conferimenti.Codice_Specie )
   LEFT OUTER JOIN dbo.Anag_Gruppo_Specie ON ( dbo.Anag_Specie.Cod_Darc1=dbo.Anag_Gruppo_Specie.Codice )
   LEFT OUTER JOIN dbo.Anag_Supergruppo_Specie ON ( dbo.Anag_Gruppo_Specie.Cod_Supergruppo=dbo.Anag_Supergruppo_Specie.Codice )
   INNER JOIN dbo.Conferimenti_Finalita ON ( dbo.Conferimenti.Anno=dbo.Conferimenti_Finalita.Anno and dbo.Conferimenti.Numero=dbo.Conferimenti_Finalita.Numero )
   INNER JOIN dbo.Anag_Finalita  dbo_Anag_Finalita_Confer ON ( dbo.Conferimenti_Finalita.Finalita=dbo_Anag_Finalita_Confer.Codice )
   INNER JOIN dbo.Operatori_di_sistema  dbo_Operatori_di_sistema_ConfMatr ON ( dbo.Conferimenti.Matr_Ins=dbo_Operatori_di_sistema_ConfMatr.Ident_Operatore )
   LEFT OUTER JOIN dbo.RDP_Date_Emissione ON ( dbo.RDP_Date_Emissione.Anno=dbo.Conferimenti.Anno and dbo.RDP_Date_Emissione.Numero=dbo.Conferimenti.Numero )
  }
WHERE
  dbo.Esami_Aggregati.Esame_Altro_Ente = 0
  AND  dbo.Esami_Aggregati.Esame_Altro_Ente = 0
  AND  (
  {fn year(dbo.Conferimenti.Data_Accettazione)}  =  2021
  AND  dbo.Conferimenti.Nome_Stazione_Inserimento  IN  ('ACC-CENTR2', 'PC-47326', 'PC-40780','MP-ACC3', 'BS-ASS-N',
                                                        'PC-47327', 'CH-ACC4-N','CH-ACC2-N', 'MP-SIVARS7','PC-47499', 
                                                        'MP-SIVARS7-N')
  )
")



# AND ({ fn MONTH(Conferimenti.Data_Accettazione) } <= 6)

# AND(Conferimenti.Nome_Stazione_Inserimento IN ('ACC-CENTR2', 'PC-47326', 'PC-40780', 
                                           # 'MP-ACC3', 'BS-ASS-N', 'PC-47327', 'CH-ACC4-N', 'CH-ACC2-N', 'MP-SIVARS7', 'PC-47499', 'MP-SIVARS7-N'))
# (dbo_Anag_Finalita_Confer.Descrizione NOT IN ('Autocontrollo latte routine', 'Controlli di qualità interni', 'Emergenza COVID-19', 'Validazione metodiche')) AND




# 
# conf <- unique(factor(acc$Nconf))
# 
# pt <- acc %>% 
#   filter(Prova !="Parere Tecnico")
# 
# confpt <- unique(factor(pt$Nconf))
# 
# 
# xx <- acc %>% filter(!Nconf %in% confpt)
# 
# 
# z <- acc %>% 
#   filter(Prova == "Parere Tecnico") %>% 
#   group_by(Nconf) %>% 
#   count()
# 
# 
# 
# dz <- as.data.frame((duplicated(z$Nconf)))
# 
# z <- cbind(z,dz)

acc <- conAcc%>% tbl(sql(queryAcc)) %>% as_tibble() 

accV <- acc %>% 
  mutate(tipoprove = ifelse(Prova=="Prova Chimica", "Prova Chimica", 
                            ifelse(Prova== "Prova Sierologica", "Prova Sierologica", 
                                   ifelse(Prova == "Parere Tecnico", "Parere Tecnico", "Prova Diagnostica/Alimenti")))) %>%
  mutate(Valorizzazione = ifelse(tipoprove == "Prova Chimica", 3.70, 
                                 ifelse(tipoprove == "Prova Sierologica", 0.20,
                                        ifelse(tipoprove == "Prova Diagnostica/Alimenti", 0.72, 0))))%>% 
  group_by(Nconf) %>% 
  mutate(Valore = max(Valorizzazione) ) %>% 
  select(-Valorizzazione, -Finalità) %>% 
  distinct(Nconf, .keep_all = TRUE) %>% 
  mutate(Valore =  0.07*(Valore)+Valore ) %>% 
  group_by(dtreg, PC) %>% 
  summarise(n.conf = n(), 
            Valore = sum(Valore),
            ncamp = sum(NrCampioni, na.rm = TRUE)) %>% 
  mutate(Anno = year(dtreg)) %>%  
  group_by(Anno) %>% 
  summarise(n.conf = sum(n.conf), 
            Valore = sum(Valore)) %>% 
  tibble(Dipartimento = "Direzione sanitaria", Reparto = "GESTIONE CENTRALIZZATA DELLE RICHIESTE", 
         Laboratorio = "	GESTIONE CENTRALIZZATA DELLE RICHIESTE")  %>%  
saveRDS(here("data", "processed", "GCR.rds"))




##DATI DA PROGETTI DI RICERCA----

prj <- read_excel(sheet = "PRJ", here("data", "raw", "prj2021.xlsx"))

anag <- ore %>% 
  mutate(annoraplav = year(FineRapporto)) %>% 
  filter(annoraplav > 2018) %>% 
  distinct(Matricola, .keep_all = TRUE)

prj %>%
  left_join(anag, by = c("MatrRSUO" = "Matricola")) %>% 
  mutate(annoinizio = year(DataInizio), 
         annofine = year(DataFine)) %>%   
  saveRDS(., file = here( "data", "processed",  "prj.rds"))



##DATI DA PUBBLICAZIONI####

pubblicazioni <- read_excel(here("data", "raw", "pubblicazioni.xlsx"))
pubblicazioni$AU <- str_to_upper(pubblicazioni$AU)
pubblicazioni$AU <- gsub(",.*$", "", pubblicazioni$AU)
pubblicazioni %>% filter(OA >= 2019) %>%
  mutate(Cognome = recode(AU,
                          "COSCIANI_CUNICO" = "COSCIANI CUNICO",
  )) %>%
  left_join(anag, by = c("Cognome" = "Cognome")) %>%
  filter(Dirigente == "S") %>%  
  saveRDS(., file = here( "data", "processed",  "pub.rds"))




##DATI DA DBASE PERFORMANCE (OBIETTIVI, INDICATORI, TARGET, RISULTATO, FTEQ PROGRAMMATI)-----

con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "CED-IIS2",
                      Database = "ObiettiviStrategiciV2018", Port = 1433)

queryPERF <- "SELECT
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

perf <- con %>% tbl(sql(queryPERF)) %>% as_tibble()

strutture <- read_excel(here("data", "raw", "strutture.xlsx"))


dip <- c("DIPARTIMENTO AREA TERRITORIALE EMILIA ROMAGNA", "DIPARTIMENTO SICUREZZA ALIMENTARE",
         "DIPARTIMENTO TUTELA SALUTE ANIMALE", "DIPARTIMENTO AREA TERRITORIALE LOMBARDIA",
         "DIPARTIMENTO AMMINISTRATIVO", "CONTROLLO DI GESTIONE")


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

  saveRDS(here("data", "processed", "performance.RDS"))
 


##Programmazione 2021 FTE--
#source(here("R",  "FTEPROGRAMMATI.R"))


#PREPARAZIONE DATI PER APPLICATIVO COSTI-RICAVI----

cc %>% 
  mutate(ClassAnalisi = recode(idClassificazione, 
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
                               `-13` = "Non Ufficiale Gratuito" ,  .default = NA_character_),
         Pagamento = recode(idClassificazione, 
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
                            `-13` = "Gratuito" ,  .default = NA_character_), 
         Uff = recode (idClassificazione, 
                       `-1` = "Ufficiale", 
                       `-3` = "Ufficiale", 
                       `-8` = "Non Ufficiale", 
                       `-9` = "Non Ufficiale", 
                       `-4` = "Ufficiale", 
                       `-5` = "Ufficiale", 
                       `-7` = "Ufficiale", 
                       `-11` = "Ufficiale", 
                       `-6`  = "Non Ufficiale", 
                       `-10` = "Non Ufficiale", 
                       `-13` = "Non Ufficiale", .default = NA_character_), 
         
         Quarter = factor(paste("Q",TRIMESTRE)),
         TUff = ifelse(ClassAnalisi == "Ufficiale a Pagamento", Fatturato,
                       ifelse(ClassAnalisi == "Ufficiale Gratuito",  Tariffario, 0)),
         TNonUff = ifelse(ClassAnalisi == "Non Ufficiale a Pagamento", Fatturato,
                          ifelse(ClassAnalisi == "Non Ufficiale Gratuito", Tariffario, 0)),
         TGratuito = ifelse(Pagamento == "Gratuito", Tariffario,0), 
         TPagamento = ifelse(Pagamento == "Pagamento", Fatturato,0), 
         TVP = ifelse(Classe == "Vendite prodotti", Fatturato, 0), 
         TAI = ifelse(Classe == "Ricavi da produzione interna", Tariffario, 0), 
         AttUff = ifelse(Uff== "Ufficiale", Determinazioni, 0 ), 
         AttNUff = ifelse(Uff== "Non Ufficiale", Determinazioni, 0 ), 
         AttGrat = ifelse(Pagamento== "Gratuito", Determinazioni, 0 ), 
         AttPag = ifelse(Pagamento == "Pagamento", Determinazioni, 0), 
         VP = ifelse(Classe == "Vendite prodotti", Numero, 0), 
         AI = ifelse(Classe == "Ricavi da produzione interna", Numero, 0)) %>% 
  saveRDS(here("data", "processed", "CC.rds"))


