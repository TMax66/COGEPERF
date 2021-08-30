library(tidyverse)
library(here)

#SQL----- 
#esegui su SQLSERVER poi esporta in formato .txt e apri in R
"SELECT
dbo_Anag_Reparti_ConfProp.Descrizione,
dbo_Anag_Reparti_ConfAcc.Descrizione,
dbo.Conferimenti.Numero,
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
)"

#Importazione dati----
rar21  <- read_delim("data/raw/datirar21.txt", 
                                 "\t", escape_double = FALSE, col_names = FALSE, 
                                 trim_ws = TRUE)

names(rar21) <- c("stran", "strpropr", "nconf", "nrdp", "dtreg", "dtref")

## analisi----
rar21 %>% 
  group_by(stran) %>% 
  summarise(n = n()) %>% 
  left_join(
    
    (rar21 %>% 
       filter(nrdp != "NULL") %>% 
    group_by(stran) %>% 
      summarise(n = n())  
    ), by = "stran") %>%  
  mutate(
    "incorso" = n.x-n.y, 
    "%refertati" = round(100*(n.y/n.x),2)) %>%  as_tibble() %>% 
  rename( "Struttura" = stran, "N.conferimenti ufficiali" =   n.x,  "N.conferimenti refertati" = n.y ,
          "N.conferimenti con analisi in corso" = incorso , "% conferimenti refertati" = `%refertati`  ) %>% View()

  