library(shiny)
library(tidyverse)
library(readxl)
library(lubridate)
library(shinythemes)
library(DT)
library(readr)
library(here)
library(DBI)
library(odbc)
library(rpivotTable)
library(shinyjs)
library(shinycssloaders)


conAcc <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbprod02.izsler.it", 
                         Database = "IZSLER", Port = 1433)

queryAcc <- ("SELECT { fn YEAR(Conferimenti.Data_Accettazione) } AS Anno, Conferimenti.Nome_Stazione_Inserimento AS PC, Conferimenti.Numero AS Nconf, dbo_Anag_Reparti_ConfProp.Descrizione AS StrPropr, dbo_Anag_Reparti_ConfAcc.Descrizione AS StrAcc, 
                  dbo_Operatori_di_sistema_ConfMatr.Descr_Completa AS Operatore, dbo_Anag_Reparti_ConfAcc.Locazione AS Locstrutt, dbo_Anag_Finalita_Confer.Descrizione AS Finalità, Anag_Registri.Descrizione AS Settore, 
				  Anag_TipoConf.Descrizione AS Pagamento, 
                  Conferimenti.Data_Accettazione AS dtreg, Anag_Gruppo_Prove.Descrizione AS Prova, Anag_Tipo_Prel.Descrizione AS Tipoprel, Esami_Aggregati.Istanza AS Istanzardp, CONVERT(SMALLDATETIME, Conferimenti.Data_Primo_RDP_Completo_Firmato) AS dtprimotrdp, 
                  RDP_Date_Emissione.Data_RDP AS dturdp, DATENAME(weekday, Conferimenti.Data_Accettazione) AS Giornoacc, Conferimenti.NrCampioni
FROM     Anag_Reparti AS dbo_Anag_Reparti_ConfProp INNER JOIN
                  Laboratori_Reparto AS dbo_Laboratori_Reparto_ConfProp ON dbo_Laboratori_Reparto_ConfProp.Reparto = dbo_Anag_Reparti_ConfProp.Codice INNER JOIN
                  Conferimenti ON Conferimenti.RepLab = dbo_Laboratori_Reparto_ConfProp.Chiave LEFT OUTER JOIN
                  Esami_Aggregati ON Conferimenti.Anno = Esami_Aggregati.Anno_Conferimento AND Conferimenti.Numero = Esami_Aggregati.Numero_Conferimento LEFT OUTER JOIN
                  Nomenclatore_MP ON Esami_Aggregati.Nomenclatore = Nomenclatore_MP.Codice LEFT OUTER JOIN
                  Nomenclatore_Settori ON Nomenclatore_MP.Nomenclatore_Settore = Nomenclatore_Settori.Codice LEFT OUTER JOIN
                  Nomenclatore ON Nomenclatore_Settori.Codice_Nomenclatore = Nomenclatore.Chiave LEFT OUTER JOIN
                  Anag_Gruppo_Prove ON Nomenclatore.Codice_Gruppo = Anag_Gruppo_Prove.Codice INNER JOIN
                  Anag_Tipo_Prel ON Conferimenti.Tipo_Prelievo = Anag_Tipo_Prel.Codice INNER JOIN
                  Anag_Registri ON Conferimenti.Registro = Anag_Registri.Codice INNER JOIN
                  Laboratori_Reparto AS dbo_Laboratori_Reparto_ConfAcc ON Conferimenti.RepLab_Conferente = dbo_Laboratori_Reparto_ConfAcc.Chiave INNER JOIN
                  Anag_Reparti AS dbo_Anag_Reparti_ConfAcc ON dbo_Laboratori_Reparto_ConfAcc.Reparto = dbo_Anag_Reparti_ConfAcc.Codice INNER JOIN
                  Anag_TipoConf ON Anag_TipoConf.Codice = Conferimenti.Tipo INNER JOIN
                  Conferimenti_Finalita ON Conferimenti.Anno = Conferimenti_Finalita.Anno AND Conferimenti.Numero = Conferimenti_Finalita.Numero INNER JOIN
                  Anag_Finalita AS dbo_Anag_Finalita_Confer ON Conferimenti_Finalita.Finalita = dbo_Anag_Finalita_Confer.Codice INNER JOIN
                  Operatori_di_sistema AS dbo_Operatori_di_sistema_ConfMatr ON Conferimenti.Matr_Ins = dbo_Operatori_di_sistema_ConfMatr.Ident_Operatore LEFT OUTER JOIN
                  RDP_Date_Emissione ON RDP_Date_Emissione.Anno = Conferimenti.Anno AND RDP_Date_Emissione.Numero = Conferimenti.Numero
WHERE  (Esami_Aggregati.Esame_Altro_Ente = 0) AND ({ fn YEAR(Conferimenti.Data_Accettazione) } = 2021)  AND 
				  (dbo_Operatori_di_sistema_ConfMatr.Descr_Completa IN ('Avisani Dominga', 'Muhammad Ibraheem', 'Zanoni Dr.ssa Mariagrazia', 
				  'Savoldini Laura', 'Merigo Silvia', 'Marmaglio Giordano', 'Baldin Silvia', 'Barbeno Claudio', 'Boccacci Giuliana', 
				  'Bettinzoli Luana', 'Bonometti Laura Camilla'))")

acc <- conAcc%>% tbl(sql(queryAcc)) %>% as_tibble() 
acc[,"Finalità"] <- sapply(acc[, "Finalità"], iconv, from = "latin1", to = "UTF-8", sub = "")
acc[,"StrAcc"] <- sapply(acc[, "StrAcc"], iconv, from = "latin1", to = "UTF-8", sub = "")
acc[,"StrPropr"] <- sapply(acc[, "StrPropr"], iconv, from = "latin1", to = "UTF-8", sub = "")



dt <- acc %>%
  group_by(Nconf) %>% 
  distinct(Nconf, .keep_all = TRUE) %>% 
  select(-Finalità, -Prova, -StrAcc)

   