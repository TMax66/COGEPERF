











#importazione accettazione----- da query esportata da SQLServer ( vedi sotto)

library(readr)
acc <- read_delim("C:/Users/vito.tranquillo/Desktop/prova.txt", 
           "\t", escape_double = FALSE, col_names = FALSE, 
           trim_ws = TRUE)


names(acc) <- c("nconf", "stracc", "strpropr", "utente")



















# query <- "SELECT
#   dbo.Conferimenti.Numero,
#   dbo_Anag_Reparti_ConfAcc.Descrizione,
#   dbo_Anag_Reparti_ConfProp.Descrizione,
#   dbo_Operatori_di_sistema_ConfMatr.Descr_Completa,
#   dbo.Anag_Gruppo_Prove.Descrizione,
#   dbo.Conferimenti.Data_Prelievo,
#   dbo.Conferimenti.Data,
#   dbo.Conferimenti.Data_Inserimento,
#   dbo_Giorni_Lavorativi_DataConf.Nr_del_giorno
# FROM
# { oj dbo.Anag_Reparti  dbo_Anag_Reparti_ConfAcc INNER JOIN dbo.Laboratori_Reparto  dbo_Laboratori_Reparto_ConfAcc ON ( dbo_Laboratori_Reparto_ConfAcc.Reparto=dbo_Anag_Reparti_ConfAcc.Codice )
#    INNER JOIN dbo.Conferimenti ON ( dbo.Conferimenti.RepLab_Conferente=dbo_Laboratori_Reparto_ConfAcc.Chiave )
#    LEFT OUTER JOIN dbo.Esami_Aggregati ON ( dbo.Conferimenti.Anno=dbo.Esami_Aggregati.Anno_Conferimento and dbo.Conferimenti.Numero=dbo.Esami_Aggregati.Numero_Conferimento )
#    LEFT OUTER JOIN dbo.Nomenclatore_MP ON ( dbo.Esami_Aggregati.Nomenclatore=dbo.Nomenclatore_MP.Codice )
#    LEFT OUTER JOIN dbo.Nomenclatore_Settori ON ( dbo.Nomenclatore_MP.Nomenclatore_Settore=dbo.Nomenclatore_Settori.Codice )
#    LEFT OUTER JOIN dbo.Nomenclatore ON ( dbo.Nomenclatore_Settori.Codice_Nomenclatore=dbo.Nomenclatore.Chiave )
#    LEFT OUTER JOIN dbo.Anag_Gruppo_Prove ON ( dbo.Nomenclatore.Codice_Gruppo=dbo.Anag_Gruppo_Prove.Codice )
#    INNER JOIN dbo.Laboratori_Reparto  dbo_Laboratori_Reparto_ConfProp ON ( dbo.Conferimenti.RepLab=dbo_Laboratori_Reparto_ConfProp.Chiave )
#    INNER JOIN dbo.Anag_Reparti  dbo_Anag_Reparti_ConfProp ON ( dbo_Laboratori_Reparto_ConfProp.Reparto=dbo_Anag_Reparti_ConfProp.Codice )
#    INNER JOIN dbo.Operatori_di_sistema  dbo_Operatori_di_sistema_ConfMatr ON ( dbo.Conferimenti.Matr_Ins=dbo_Operatori_di_sistema_ConfMatr.Ident_Operatore )
#    INNER JOIN dbo.Giorni_Lavorativi  dbo_Giorni_Lavorativi_DataConf ON ( dbo_Giorni_Lavorativi_DataConf.Giorno=dbo.Conferimenti.Data )
#   }
# WHERE
#   dbo.Esami_Aggregati.Esame_Altro_Ente = 0
#   AND  (
#   {fn year(dbo.Conferimenti.Data_Accettazione)}  =  2021
#   )
# "