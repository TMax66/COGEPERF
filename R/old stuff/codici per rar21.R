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
query <- "SELECT
dbo.Conferimenti.Numero,
dbo_Anag_Reparti_ConfProp.Descrizione As StrProp,
dbo_Anag_Reparti_ConfAcc.Descrizione As StrAcc,
dbo.RDP_Date_Emissione.Istanza_RDP,
dbo.Conferimenti.Data_Accettazione,
dbo.RDP_Date_Emissione.Data_RDP
FROM
{ oj dbo.Anag_Reparti  dbo_Anag_Reparti_ConfProp INNER JOIN dbo.Laboratori_Reparto  dbo_Laboratori_Reparto_ConfProp ON ( dbo_Laboratori_Reparto_ConfProp.Reparto=dbo_Anag_Reparti_ConfProp.Codice )
  INNER JOIN dbo.Conferimenti ON ( dbo.Conferimenti.RepLab=dbo_Laboratori_Reparto_ConfProp.Chiave )
  INNER JOIN dbo.Anag_Tipo_Prel ON ( dbo.Conferimenti.Tipo_Prelievo=dbo.Anag_Tipo_Prel.Codice )
  INNER JOIN dbo.Laboratori_Reparto  dbo_Laboratori_Reparto_ConfAcc ON ( dbo.Conferimenti.RepLab_Conferente=dbo_Laboratori_Reparto_ConfAcc.Chiave )
  INNER JOIN dbo.Anag_Reparti  dbo_Anag_Reparti_ConfAcc ON ( dbo_Laboratori_Reparto_ConfAcc.Reparto=dbo_Anag_Reparti_ConfAcc.Codice )
  LEFT OUTER JOIN dbo.RDP_Date_Emissione ON ( dbo.RDP_Date_Emissione.Anno=dbo.Conferimenti.Anno and dbo.RDP_Date_Emissione.Numero=dbo.Conferimenti.Numero )
}
WHERE
(
  {fn year(dbo.Conferimenti.Data_Accettazione)}  =  2021
  AND  dbo.Anag_Tipo_Prel.Descrizione  =  'Ufficiale'
)
"


rar21 <- con %>% tbl(sql(query)) %>% as_tibble()




#Importazione dati----
rar21  <- read_delim("data/raw/datirar21.txt", 
                                 "\t", escape_double = FALSE, col_names = FALSE, 
                                 trim_ws = TRUE)

names(rar21) <- c("nconf","stran", "strpropr", "nrdp", "dtreg", "dtref")





rar21 <- rar21 %>%
  mutate(Struttura = recode(stran,
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

 
tabella <- rar21 %>%
filter(dtreg <= "2021-12-02" ) %>%  
  group_by(stran) %>% 
  summarise(n = n()) %>% 
  left_join(
    
    (rar21 %>% 
       filter(dtreg <= "2021-12-02" ) %>%  
       filter(nrdp != "NULL") %>% 
    group_by(stran) %>% 
      summarise(n = n())  
    ), by = "stran") %>%  
  mutate(
    "incorso" = n.x-n.y, 
    "%refertati" = round(100*(n.y/n.x),2)) %>%  as_tibble() %>% 
  rename( "Struttura" = stran, "N.conferimenti ufficiali" =   n.x,  "N.conferimenti refertati" = n.y ,
          "N.conferimenti con analisi in corso" = incorso , "% conferimenti refertati" = `%refertati`  ) 
  


write.table(tabella, file = "tabella.txt")

# print(xtable(tabella), type = "html", file = "tabella.html"
# )      
      