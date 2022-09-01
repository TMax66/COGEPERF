library(tidyverse)
library(here)
library(knitr)
library(kableExtra)
library(gt)

#SQL----- 
#esegui su SQLSERVER poi esporta in formato .txt e apri in R
con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbprod02.izsler.it",
                      Database = "IZSLER", Port = 1433)
#
#
# query <- "SELECT        Conferimenti.Numero, dbo_Anag_Reparti_ConfProp.Descrizione AS StrProp, dbo_Anag_Reparti_ConfAcc.Descrizione AS StrAcc, RDP_Date_Emissione.Istanza_RDP, Conferimenti.Data_Accettazione, 
#                          RDP_Date_Emissione.Data_RDP, Conferimenti.NrCampioni
# FROM            Anag_Reparti AS dbo_Anag_Reparti_ConfProp INNER JOIN
#                          Laboratori_Reparto AS dbo_Laboratori_Reparto_ConfProp ON dbo_Laboratori_Reparto_ConfProp.Reparto = dbo_Anag_Reparti_ConfProp.Codice INNER JOIN
#                          Conferimenti ON Conferimenti.RepLab = dbo_Laboratori_Reparto_ConfProp.Chiave INNER JOIN
#                          Anag_Tipo_Prel ON Conferimenti.Tipo_Prelievo = Anag_Tipo_Prel.Codice INNER JOIN
#                          Laboratori_Reparto AS dbo_Laboratori_Reparto_ConfAcc ON Conferimenti.RepLab_Conferente = dbo_Laboratori_Reparto_ConfAcc.Chiave INNER JOIN
#                          Anag_Reparti AS dbo_Anag_Reparti_ConfAcc ON dbo_Laboratori_Reparto_ConfAcc.Reparto = dbo_Anag_Reparti_ConfAcc.Codice LEFT OUTER JOIN
#                          RDP_Date_Emissione ON RDP_Date_Emissione.Anno = Conferimenti.Anno AND RDP_Date_Emissione.Numero = Conferimenti.Numero
# WHERE        ({ fn YEAR(Conferimenti.Data_Accettazione) } = 2022) AND (Anag_Tipo_Prel.Descrizione = 'Ufficiale')
# "





query <-"SELECT   dbo_Anag_Reparti_ConfProp.Descrizione  AS StrProp,
  dbo_Anag_Reparti_ConfAcc.Descrizione AS StrAcc,
  dbo.Conferimenti.Numero,
  dbo.RDP_Date_Emissione.Istanza_RDP,
  dbo.Conferimenti.Data_Accettazione,
  dbo.RDP_Date_Emissione.Data_RDP,
  dbo.Conferimenti.NrCampioni,
  dbo_Anag_Finalita_Confer.Descrizione AS finalita,
  dbo.Esami_Aggregati.Tot_Eseguiti

FROM            Anag_Reparti AS dbo_Anag_Reparti_ConfProp INNER JOIN
Laboratori_Reparto AS dbo_Laboratori_Reparto_ConfProp ON dbo_Laboratori_Reparto_ConfProp.Reparto = dbo_Anag_Reparti_ConfProp.Codice INNER JOIN
Conferimenti ON Conferimenti.RepLab = dbo_Laboratori_Reparto_ConfProp.Chiave LEFT OUTER JOIN
Esami_Aggregati ON Conferimenti.Anno = Esami_Aggregati.Anno_Conferimento AND Conferimenti.Numero = Esami_Aggregati.Numero_Conferimento INNER JOIN
Anag_Tipo_Prel ON Conferimenti.Tipo_Prelievo = Anag_Tipo_Prel.Codice INNER JOIN
Laboratori_Reparto AS dbo_Laboratori_Reparto_ConfAcc ON Conferimenti.RepLab_Conferente = dbo_Laboratori_Reparto_ConfAcc.Chiave INNER JOIN
Anag_Reparti AS dbo_Anag_Reparti_ConfAcc ON dbo_Laboratori_Reparto_ConfAcc.Reparto = dbo_Anag_Reparti_ConfAcc.Codice INNER JOIN
Conferimenti_Finalita ON Conferimenti.Anno = Conferimenti_Finalita.Anno AND Conferimenti.Numero = Conferimenti_Finalita.Numero INNER JOIN
Anag_Finalita AS dbo_Anag_Finalita_Confer ON Conferimenti_Finalita.Finalita = dbo_Anag_Finalita_Confer.Codice LEFT OUTER JOIN
RDP_Date_Emissione ON RDP_Date_Emissione.Anno = Conferimenti.Anno AND RDP_Date_Emissione.Numero = Conferimenti.Numero
WHERE        (Esami_Aggregati.Esame_Altro_Ente = 0) AND (Esami_Aggregati.Esame_Altro_Ente = 0) AND ({ fn YEAR(Conferimenti.Data_Accettazione) } = 2022) AND (Anag_Tipo_Prel.Descrizione = 'Ufficiale')"















rar22 <- con %>% tbl(sql(query)) %>% as_tibble()




#Importazione dati----
# rar21  <- read_delim("data/raw/datirar21.txt", 
#                                  "\t", escape_double = FALSE, col_names = FALSE, 
#                                  trim_ws = TRUE)

#names(rar22) <- c("nconf","stran", "strpropr", "nrdp", "dtreg", "dtref","")





rar22 <- rar22 %>%
  mutate(Struttura = recode(StrAcc,
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



## analisi----
library(openxlsx)
 
tabella <- rar22 %>%
  filter(Data_Accettazione <= "2022-09-01") %>% 
  group_by(StrAcc) %>% 
  summarise(n = n()) %>% 
  left_join(
    (rar22 %>% 
       filter(!is.na(Istanza_RDP) ,
              Data_Accettazione <= "2022-09-01") %>% 
    group_by(StrAcc) %>% 
      summarise(n = n())  
    ), by = "StrAcc") %>%  
  mutate(
    "incorso" = n.x-n.y, 
    "%refertati" = round(100*(n.y/n.x),2)) %>%  as_tibble() %>% 
  rename( "Struttura" = StrAcc, "N.conferimenti ufficiali" =   n.x,  "N.conferimenti refertati" = n.y ,
          "N.conferimenti con analisi in corso" = incorso , "% conferimenti refertati" = `%refertati`  ) %>% 
 write.xlsx(file = "controllo attività.xlsx")
  
  


write.table(tabella, file = "tabella.txt")


rar22 %>% distinct() %>% 
  filter(Data_Accettazione <= "2022-09-01") %>% 
  group_by(StrAcc) %>% 
  summarise(n = n()) %>% View()

