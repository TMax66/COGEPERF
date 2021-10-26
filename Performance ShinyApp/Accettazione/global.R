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


#conAcc <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbprod02.izsler.it",
                       #Database = "IZSLER", Port = 1433)
# 
# queryAcc <- ("SELECT
#   {fn year(dbo.Conferimenti.Data_Accettazione)} AS Anno,
#   dbo.Conferimenti.Nome_Stazione_Inserimento AS PC,
#   dbo.Conferimenti.Numero AS Nconf,
#   dbo_Anag_Reparti_ConfProp.Descrizione AS StrPropr,
#   dbo_Anag_Reparti_ConfAcc.Descrizione AS StrAcc,
#   dbo_Operatori_di_sistema_ConfMatr.Descr_Completa AS Operatore,
#   dbo_Anag_Reparti_ConfProp.Locazione AS LocStrutt,
#   dbo_Anag_Finalita_Confer.Descrizione AS Finalità,
#   dbo.Anag_Registri.Descrizione AS Settore,
#   dbo.Anag_TipoConf.Descrizione AS Pagamento,
#   dbo.Conferimenti.Data_Inserimento AS dtreg,
#   dbo.Anag_Specie.Descrizione AS Specie ,
#   dbo.Anag_Materiali.Descrizione AS Materiale,
#   dbo.Anag_Matrici.Descrizione AS Matrice,
#   dbo.Anag_Supergruppo_Specie.Descrizione AS SupergruppoSpecie,
#   dbo.Anag_Gruppo_Specie.Descrizione AS GruppoSpecie,
#   dbo.Anag_Prove.Descrizione AS Prova,
#   dbo.Anag_Tipo_Prel.Descrizione AS Tipoprel,
#   dbo.RDP_Date_Emissione.Istanza_RDP AS Istanzardp,
#   convert (SMALLDATETIME, dbo.Conferimenti.Data_Primo_RDP_Completo_Firmato) AS dtprimotrdp,
#   dbo.RDP_Date_Emissione.Data_RDP AS dturp,
#   Datename(weekday, dbo.Conferimenti.Data_Accettazione) AS Giornoacc,
#   dbo.Conferimenti.NrCampioni
# FROM
# { oj dbo.Anag_Reparti  dbo_Anag_Reparti_ConfProp INNER JOIN dbo.Laboratori_Reparto  dbo_Laboratori_Reparto_ConfProp ON ( dbo_Laboratori_Reparto_ConfProp.Reparto=dbo_Anag_Reparti_ConfProp.Codice )
#    INNER JOIN dbo.Conferimenti ON ( dbo.Conferimenti.RepLab=dbo_Laboratori_Reparto_ConfProp.Chiave )
#    LEFT OUTER JOIN dbo.Anag_Matrici ON ( dbo.Conferimenti.Matrice=dbo.Anag_Matrici.Codice )
#    LEFT OUTER JOIN dbo.Esami_Aggregati ON ( dbo.Conferimenti.Anno=dbo.Esami_Aggregati.Anno_Conferimento and dbo.Conferimenti.Numero=dbo.Esami_Aggregati.Numero_Conferimento )
#    LEFT OUTER JOIN dbo.Nomenclatore_MP ON ( dbo.Esami_Aggregati.Nomenclatore=dbo.Nomenclatore_MP.Codice )
#    LEFT OUTER JOIN dbo.Nomenclatore_Settori ON ( dbo.Nomenclatore_MP.Nomenclatore_Settore=dbo.Nomenclatore_Settori.Codice )
#    LEFT OUTER JOIN dbo.Nomenclatore ON ( dbo.Nomenclatore_Settori.Codice_Nomenclatore=dbo.Nomenclatore.Chiave )
#    LEFT OUTER JOIN dbo.Anag_Prove ON ( dbo.Nomenclatore.Codice_Prova=dbo.Anag_Prove.Codice )
#    INNER JOIN dbo.Anag_Tipo_Prel ON ( dbo.Conferimenti.Tipo_Prelievo=dbo.Anag_Tipo_Prel.Codice )
#    INNER JOIN dbo.Anag_Registri ON ( dbo.Conferimenti.Registro=dbo.Anag_Registri.Codice )
#    INNER JOIN dbo.Laboratori_Reparto  dbo_Laboratori_Reparto_ConfAcc ON ( dbo.Conferimenti.RepLab_Conferente=dbo_Laboratori_Reparto_ConfAcc.Chiave )
#    INNER JOIN dbo.Anag_Reparti  dbo_Anag_Reparti_ConfAcc ON ( dbo_Laboratori_Reparto_ConfAcc.Reparto=dbo_Anag_Reparti_ConfAcc.Codice )
#    INNER JOIN dbo.Anag_TipoConf ON ( dbo.Anag_TipoConf.Codice=dbo.Conferimenti.Tipo )
#    LEFT OUTER JOIN dbo.Anag_Materiali ON ( dbo.Anag_Materiali.Codice=dbo.Conferimenti.Codice_Materiale )
#    LEFT OUTER JOIN dbo.Anag_Specie ON ( dbo.Anag_Specie.Codice=dbo.Conferimenti.Codice_Specie )
#    LEFT OUTER JOIN dbo.Anag_Gruppo_Specie ON ( dbo.Anag_Specie.Cod_Darc1=dbo.Anag_Gruppo_Specie.Codice )
#    LEFT OUTER JOIN dbo.Anag_Supergruppo_Specie ON ( dbo.Anag_Gruppo_Specie.Cod_Supergruppo=dbo.Anag_Supergruppo_Specie.Codice )
#    INNER JOIN dbo.Conferimenti_Finalita ON ( dbo.Conferimenti.Anno=dbo.Conferimenti_Finalita.Anno and dbo.Conferimenti.Numero=dbo.Conferimenti_Finalita.Numero )
#    INNER JOIN dbo.Anag_Finalita  dbo_Anag_Finalita_Confer ON ( dbo.Conferimenti_Finalita.Finalita=dbo_Anag_Finalita_Confer.Codice )
#    INNER JOIN dbo.Operatori_di_sistema  dbo_Operatori_di_sistema_ConfMatr ON ( dbo.Conferimenti.Matr_Ins=dbo_Operatori_di_sistema_ConfMatr.Ident_Operatore )
#    LEFT OUTER JOIN dbo.RDP_Date_Emissione ON ( dbo.RDP_Date_Emissione.Anno=dbo.Conferimenti.Anno and dbo.RDP_Date_Emissione.Numero=dbo.Conferimenti.Numero )
#   }
# 
# WHERE  (Esami_Aggregati.Esame_Altro_Ente = 0) AND ({ fn YEAR(Conferimenti.Data_Accettazione) } = 2021)  AND
# 				  (dbo_Operatori_di_sistema_ConfMatr.Descr_Completa IN ('Avisani Dominga', 'Muhammad Ibraheem', 'Zanoni Dr.ssa Mariagrazia',
# 				  'Savoldini Laura', 'Merigo Silvia', 'Marmaglio Giordano', 'Baldin Silvia', 'Barbeno Claudio', 'Boccacci Giuliana',
# 				  'Bettinzoli Luana', 'Bonometti Laura Camilla'))")

# acc <- conAcc%>% tbl(sql(queryAcc)) %>% as_tibble()
# acc[,"Finalità"] <- sapply(acc[, "Finalità"], iconv, from = "latin1", to = "UTF-8", sub = "")
# acc[,"StrAcc"] <- sapply(acc[, "StrAcc"], iconv, from = "latin1", to = "UTF-8", sub = "")
# acc[,"StrPropr"] <- sapply(acc[, "StrPropr"], iconv, from = "latin1", to = "UTF-8", sub = "")
#
#
#
# dt <- acc %>%
#   group_by(Nconf) %>%
#   distinct(Nconf, .keep_all = TRUE) %>%
#   select(-Finalità, -Prova, -StrAcc) %>% 
#   saveRDS(here("Performance ShinyApp","Accettazione","dt.rds"))
# 
 

#dt <- readRDS(file = here("Performance ShinyApp","Accettazione","dt.rds"))
dt <- readRDS("dt.rds")


