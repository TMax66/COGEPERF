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
# 
# 
# 
# 
# 
# query <-"SELECT   dbo_Anag_Reparti_ConfProp.Descrizione  AS StrProp,
#   dbo_Anag_Reparti_ConfAcc.Descrizione AS StrAcc,
#   dbo.Conferimenti.Numero,
#   dbo.RDP_Date_Emissione.Istanza_RDP,
#   dbo.Conferimenti.Data_Accettazione,
#   dbo.RDP_Date_Emissione.Data_RDP,
#   dbo.Conferimenti.NrCampioni,
#   dbo_Anag_Finalita_Confer.Descrizione AS finalita,
#   dbo.Esami_Aggregati.Tot_Eseguiti
# 
# FROM            Anag_Reparti AS dbo_Anag_Reparti_ConfProp INNER JOIN
# Laboratori_Reparto AS dbo_Laboratori_Reparto_ConfProp ON dbo_Laboratori_Reparto_ConfProp.Reparto = dbo_Anag_Reparti_ConfProp.Codice INNER JOIN
# Conferimenti ON Conferimenti.RepLab = dbo_Laboratori_Reparto_ConfProp.Chiave LEFT OUTER JOIN
# Esami_Aggregati ON Conferimenti.Anno = Esami_Aggregati.Anno_Conferimento AND Conferimenti.Numero = Esami_Aggregati.Numero_Conferimento INNER JOIN
# Anag_Tipo_Prel ON Conferimenti.Tipo_Prelievo = Anag_Tipo_Prel.Codice INNER JOIN
# Laboratori_Reparto AS dbo_Laboratori_Reparto_ConfAcc ON Conferimenti.RepLab_Conferente = dbo_Laboratori_Reparto_ConfAcc.Chiave INNER JOIN
# Anag_Reparti AS dbo_Anag_Reparti_ConfAcc ON dbo_Laboratori_Reparto_ConfAcc.Reparto = dbo_Anag_Reparti_ConfAcc.Codice INNER JOIN
# Conferimenti_Finalita ON Conferimenti.Anno = Conferimenti_Finalita.Anno AND Conferimenti.Numero = Conferimenti_Finalita.Numero INNER JOIN
# Anag_Finalita AS dbo_Anag_Finalita_Confer ON Conferimenti_Finalita.Finalita = dbo_Anag_Finalita_Confer.Codice LEFT OUTER JOIN
# RDP_Date_Emissione ON RDP_Date_Emissione.Anno = Conferimenti.Anno AND RDP_Date_Emissione.Numero = Conferimenti.Numero
# WHERE        (Esami_Aggregati.Esame_Altro_Ente = 0) AND (Esami_Aggregati.Esame_Altro_Ente = 0) AND ({ fn YEAR(Conferimenti.Data_Accettazione) } = 2022) AND (Anag_Tipo_Prel.Descrizione = 'Ufficiale')"
# 
# 
# 
# 
# 

query <- "SELECT   dbo_Anag_Reparti_ConfProp.Descrizione  AS StrProp,
  dbo_Anag_Reparti_ConfAcc.Descrizione AS StrAcc,
  dbo.Anag_Reparti.Descrizione As StrAnalisi,
  dbo.Conferimenti.Numero,
  dbo.RDP_Date_Emissione.Istanza_RDP,
  dbo.Conferimenti.Data_Accettazione,
  dbo.RDP_Date_Emissione.Data_RDP,
  dbo.Conferimenti.NrCampioni,
  dbo.Esami_Aggregati.Tot_Eseguiti,
  dbo.Conferimenti.Data,
  dbo.Anag_Finalita.Descrizione As finalita
FROM
{ oj dbo.Anag_Reparti  dbo_Anag_Reparti_ConfProp INNER JOIN dbo.Laboratori_Reparto  dbo_Laboratori_Reparto_ConfProp ON ( dbo_Laboratori_Reparto_ConfProp.Reparto=dbo_Anag_Reparti_ConfProp.Codice )
   INNER JOIN dbo.Conferimenti ON ( dbo.Conferimenti.RepLab=dbo_Laboratori_Reparto_ConfProp.Chiave )
   LEFT OUTER JOIN dbo.Esami_Aggregati ON ( dbo.Conferimenti.Anno=dbo.Esami_Aggregati.Anno_Conferimento and dbo.Conferimenti.Numero=dbo.Esami_Aggregati.Numero_Conferimento )
   LEFT OUTER JOIN dbo.Programmazione_Finalita ON ( dbo.Esami_Aggregati.Anno_Conferimento=dbo.Programmazione_Finalita.Anno_Conferimento and dbo.Esami_Aggregati.Numero_Conferimento=dbo.Programmazione_Finalita.Numero_Conferimento and dbo.Esami_Aggregati.Codice=dbo.Programmazione_Finalita.Codice )
   LEFT OUTER JOIN dbo.Anag_Finalita ON ( dbo.Programmazione_Finalita.Finalita=dbo.Anag_Finalita.Codice )
   LEFT OUTER JOIN dbo.Laboratori_Reparto ON ( dbo.Esami_Aggregati.RepLab_analisi=dbo.Laboratori_Reparto.Chiave )
   LEFT OUTER JOIN dbo.Anag_Reparti ON ( dbo.Laboratori_Reparto.Reparto=dbo.Anag_Reparti.Codice )
   INNER JOIN dbo.Laboratori_Reparto  dbo_Laboratori_Reparto_ConfAcc ON ( dbo.Conferimenti.RepLab_Conferente=dbo_Laboratori_Reparto_ConfAcc.Chiave )
   INNER JOIN dbo.Anag_Reparti  dbo_Anag_Reparti_ConfAcc ON ( dbo_Laboratori_Reparto_ConfAcc.Reparto=dbo_Anag_Reparti_ConfAcc.Codice )
   INNER JOIN dbo.Conferimenti_Finalita ON ( dbo.Conferimenti.Anno=dbo.Conferimenti_Finalita.Anno and dbo.Conferimenti.Numero=dbo.Conferimenti_Finalita.Numero )
   INNER JOIN dbo.Anag_Finalita  dbo_Anag_Finalita_Confer ON ( dbo.Conferimenti_Finalita.Finalita=dbo_Anag_Finalita_Confer.Codice )
   LEFT OUTER JOIN dbo.RDP_Date_Emissione ON ( dbo.RDP_Date_Emissione.Anno=dbo.Conferimenti.Anno and dbo.RDP_Date_Emissione.Numero=dbo.Conferimenti.Numero )
  }
WHERE
  dbo.Esami_Aggregati.Esame_Altro_Ente = 0
  AND  dbo.Esami_Aggregati.Esame_Altro_Ente = 0
  AND  (
  {fn year(dbo.Conferimenti.Data_Accettazione)}  =  2022
  AND  dbo_Anag_Finalita_Confer.Descrizione  IN  ('Monitoraggio fauna selvatica', 'Monitoraggio fauna selvatica Emilia Romagna', 'Monitoraggio fauna selvatica Lombardia', 'Piano Regionale Alimenti (Emilia Romagna)', 'Piano Regionale Alimenti (Lombardia)', 'Piano sorveglianza nazionale West Nile Disease', 'TSE - ricerca farine animali', 'TSE - ricerca farine animali Extrapiano', 'TSE - ricerca farine animali monitoraggio', 'TSE - ricerca farine animali sorveglianza', 'TSE - ricerca farine animali Sospetto', 'TSE - ricerca farine animali-importazione')
  )"










rar22 <- con %>% tbl(sql(query)) %>% as_tibble()




#Importazione dati----
# rar21  <- read_delim("data/raw/datirar21.txt", 
#                                  "\t", escape_double = FALSE, col_names = FALSE, 
#                                  trim_ws = TRUE)

#names(rar22) <- c("nconf","stran", "strpropr", "nrdp", "dtreg", "dtref","")





rar22 <- rar22 %>%
  mutate(Struttura = recode(Str,
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
 
tabella <- rar22 %>% distinct(Numero, .keep_all = TRUE) %>% 
  filter(Data  <= "2022-08-31") %>% 
  group_by(finalita,  StrAcc) %>% 
  summarise(n = n()) %>%  
  left_join(
    (rar22 %>% distinct(Numero, .keep_all = TRUE) %>% 
       filter(!is.na(Istanza_RDP) ,
              Data  <= "2022-08-31") %>% 
    group_by(finalita, StrAcc) %>% 
      summarise(n = n())     
    ), by = c("finalita", "StrAcc")) %>% 
  # filter(finalita %in% c("Monitoraggio fauna selvatica Emilia Romagna",
  #                        "Monitoraggio fauna selvatica Lombardia", 
  #                        "Piano Regionale Alimenti (Emilia Romagna)",
  #                        "Controllo alimenti",
  #                        "Piano sorveglianza nazionale West Nile Disease" , 
  #                        "TSE - ricerca farine animali monitoraggio", 
  #                        "TSE - ricerca farine animali sorveglianza")) %>% 
  mutate(
    "incorso" = n.x-n.y, 
    "%refertati" = round(100*(n.y/n.x),2)) %>%  as_tibble() %>% 
  rename( "Struttura" = StrAcc, "N.conferimenti" =   n.x,  "N.conferimenti refertati" = n.y ,
          "N.conferimenti con analisi in corso" = incorso , "% conferimenti refertati" = `%refertati`  ) %>%
  
  group_by(Struttura) %>% 
  summarise(N.conferimenti = sum(N.conferimenti), 
            `N.conferimenti refertati`  = sum(`N.conferimenti refertati`, na.rm = TRUE),
            `N.conferimenti con analisi in corso` = sum(`N.conferimenti con analisi in corso`, na.rm = TRUE), 
            `% conferimenti refertati` = round(100*(`N.conferimenti refertati`/N.conferimenti))
            ) %>%  View()
  
 write.xlsx(file = "controllo attività.xlsx")
  
 



 rar22 %>% distinct(Numero, finalita, .keep_all = TRUE) %>%  
   filter(Data  <= "2022-08-31") %>% 
   group_by(finalita,  StrAnalisi) %>% 
   summarise(ncamp = sum(NrCampioni, na.rm=TRUE), 
             nesami = sum(Tot_Eseguiti, na.rm = TRUE)) %>%  View()
   
 

 tabella <- rar22 %>% distinct(Numero, .keep_all = TRUE) %>% 
   filter(Data  <= "2022-08-15") %>% 
   group_by(finalita,  StrAcc) %>% 
   summarise(n = n()) %>%  
   left_join(
     (rar22 %>% distinct(Numero, .keep_all = TRUE) %>% 
        filter(!is.na(Istanza_RDP) ,
               Data  <= "2022-08-15") %>% 
        group_by(finalita, StrAcc) %>% 
        summarise(n = n())     
     ), by = c("finalita", "StrAcc")) %>% 
   # filter(finalita %in% c("Monitoraggio fauna selvatica Emilia Romagna",
   #                        "Monitoraggio fauna selvatica Lombardia", 
   #                        "Piano Regionale Alimenti (Emilia Romagna)",
   #                        "Controllo alimenti",
   #                        "Piano sorveglianza nazionale West Nile Disease" , 
   #                        "TSE - ricerca farine animali monitoraggio", 
   #                        "TSE - ricerca farine animali sorveglianza")) %>% 
   mutate(
     "incorso" = n.x-n.y, 
     "%refertati" = round(100*(n.y/n.x),2)) %>%  as_tibble() %>% 
   rename( "Struttura" = StrAcc, "N.conferimenti" =   n.x,  "N.conferimenti refertati" = n.y ,
           "N.conferimenti con analisi in corso" = incorso , "% conferimenti refertati" = `%refertati`  ) %>%
   
   group_by(finalita,Struttura) %>% 
   summarise(N.conferimenti = sum(N.conferimenti), 
             `N.conferimenti refertati`  = sum(`N.conferimenti refertati`, na.rm = TRUE),
             `N.conferimenti con analisi in corso` = sum(`N.conferimenti con analisi in corso`, na.rm = TRUE), 
             `% conferimenti refertati` = round(100*(`N.conferimenti refertati`/N.conferimenti))
   ) %>%    
 
 write.xlsx(file = "controllo attività.xlsx")
 
 











 
 