# query <- "SELECT DISTINCT
# dbo.Conferimenti.Data As dtconf,
# dbo.Conferimenti.Data_Inserimento As dtreg,
# dbo.Anag_Registri.Descrizione As settore,
# dbo_Anag_Reparti_ConfProp.Descrizione As strpropr,
# dbo_Anag_Reparti_ConfAcc.Descrizione As stracc,
# dbo.Anag_Reparti.Descrizione As str_analisi,
# dbo_Anag_Finalita_Confer.Descrizione As finalita,
# dbo.Anag_TipoConf.Descrizione As tipoconf,
# dbo.Anag_Tipo_Prel.Descrizione As tipoprel,
# dbo.Anag_Materiali.Descrizione As Materiali,
# dbo.Anag_Matrici.Descrizione As Matrici,
# dbo.Anag_Specie.Descrizione As Specie,
# dbo.Conferimenti.NrCampioni,
# convert (SMALLDATETIME, dbo.Conferimenti.Data_Primo_RDP_Completo_Firmato) As dtPrdp,
# dbo.Esami_Aggregati.Tot_Eseguiti,
# dbo.Anag_Gruppo_Prove.Descrizione As gruppo_prove,
#  dbo.Anag_Reparti.Descrizione As stresami,
#   dbo.Conferimenti.Numero As nconf
# FROM
# { oj dbo.Anag_Registri INNER JOIN dbo.Conferimenti ON ( dbo.Conferimenti.Registro=dbo.Anag_Registri.Codice )
#    LEFT OUTER JOIN dbo.Anag_Matrici ON ( dbo.Conferimenti.Matrice=dbo.Anag_Matrici.Codice )
#    LEFT OUTER JOIN dbo.Esami_Aggregati ON ( dbo.Conferimenti.Anno=dbo.Esami_Aggregati.Anno_Conferimento and dbo.Conferimenti.Numero=dbo.Esami_Aggregati.Numero_Conferimento )
#    LEFT OUTER JOIN dbo.Nomenclatore_MP ON ( dbo.Esami_Aggregati.Nomenclatore=dbo.Nomenclatore_MP.Codice )
#    LEFT OUTER JOIN dbo.Nomenclatore_Settori ON ( dbo.Nomenclatore_MP.Nomenclatore_Settore=dbo.Nomenclatore_Settori.Codice )
#    LEFT OUTER JOIN dbo.Nomenclatore ON ( dbo.Nomenclatore_Settori.Codice_Nomenclatore=dbo.Nomenclatore.Chiave )
#    LEFT OUTER JOIN dbo.Anag_Gruppo_Prove ON ( dbo.Nomenclatore.Codice_Gruppo=dbo.Anag_Gruppo_Prove.Codice )
#    LEFT OUTER JOIN dbo.Laboratori_Reparto ON ( dbo.Esami_Aggregati.RepLab_analisi=dbo.Laboratori_Reparto.Chiave )
#    LEFT OUTER JOIN dbo.Anag_Reparti ON ( dbo.Laboratori_Reparto.Reparto=dbo.Anag_Reparti.Codice )
#    INNER JOIN dbo.Anag_Tipo_Prel ON ( dbo.Conferimenti.Tipo_Prelievo=dbo.Anag_Tipo_Prel.Codice )
#    INNER JOIN dbo.Laboratori_Reparto  dbo_Laboratori_Reparto_ConfProp ON ( dbo.Conferimenti.RepLab=dbo_Laboratori_Reparto_ConfProp.Chiave )
#    INNER JOIN dbo.Anag_Reparti  dbo_Anag_Reparti_ConfProp ON ( dbo_Laboratori_Reparto_ConfProp.Reparto=dbo_Anag_Reparti_ConfProp.Codice )
#    INNER JOIN dbo.Laboratori_Reparto  dbo_Laboratori_Reparto_ConfAcc ON ( dbo.Conferimenti.RepLab_Conferente=dbo_Laboratori_Reparto_ConfAcc.Chiave )
#    INNER JOIN dbo.Anag_Reparti  dbo_Anag_Reparti_ConfAcc ON ( dbo_Laboratori_Reparto_ConfAcc.Reparto=dbo_Anag_Reparti_ConfAcc.Codice )
#    INNER JOIN dbo.Anag_TipoConf ON ( dbo.Anag_TipoConf.Codice=dbo.Conferimenti.Tipo )
#    LEFT OUTER JOIN dbo.Anag_Materiali ON ( dbo.Anag_Materiali.Codice=dbo.Conferimenti.Codice_Materiale )
#    LEFT OUTER JOIN dbo.Anag_Specie ON ( dbo.Anag_Specie.Codice=dbo.Conferimenti.Codice_Specie )
#    INNER JOIN dbo.Conferimenti_Finalita ON ( dbo.Conferimenti.Anno=dbo.Conferimenti_Finalita.Anno and dbo.Conferimenti.Numero=dbo.Conferimenti_Finalita.Numero )
#    INNER JOIN dbo.Anag_Finalita  dbo_Anag_Finalita_Confer ON ( dbo.Conferimenti_Finalita.Finalita=dbo_Anag_Finalita_Confer.Codice )
#   }
# WHERE
#   dbo.Esami_Aggregati.Esame_Altro_Ente = 0
#   AND  dbo.Esami_Aggregati.Esame_Altro_Ente = 0
#   AND  (
#   {fn year(dbo.Conferimenti.Data)}  >=  2019
#   )
# "


query <- "SELECT
dbo.Conferimenti.Numero,
dbo.Conferimenti.NrCampioni,
dbo.Conferimenti.Data_Accettazione,
dbo_Anag_Reparti_ConfAcc.Descrizione As repacc,
{fn year(dbo.Conferimenti.Data_Accettazione)} As dtconf,
dbo.Anag_Registri.Descrizione As settore,
dbo.Anag_Tipo_Prel.Descrizione As tipoprel,
dbo_Anag_Finalita_Confer.Descrizione As finalita,
dbo.Anag_TipoConf.Descrizione As tipoconf
FROM
dbo.Conferimenti,
dbo.Anag_Reparti  dbo_Anag_Reparti_ConfAcc,
dbo.Anag_Registri,
dbo.Anag_Tipo_Prel,
dbo.Anag_Finalita  dbo_Anag_Finalita_Confer,
dbo.Anag_TipoConf,
dbo.Laboratori_Reparto  dbo_Laboratori_Reparto_ConfAcc,
dbo.Conferimenti_Finalita
WHERE
( dbo.Conferimenti.Tipo_Prelievo=dbo.Anag_Tipo_Prel.Codice  )
AND  ( dbo.Conferimenti.Registro=dbo.Anag_Registri.Codice  )
AND  ( dbo.Conferimenti.RepLab_Conferente=dbo_Laboratori_Reparto_ConfAcc.Chiave  )
AND  ( dbo_Laboratori_Reparto_ConfAcc.Reparto=dbo_Anag_Reparti_ConfAcc.Codice  )
AND  ( dbo.Anag_TipoConf.Codice=dbo.Conferimenti.Tipo  )
AND  ( dbo.Conferimenti.Anno=dbo.Conferimenti_Finalita.Anno and dbo.Conferimenti.Numero=dbo.Conferimenti_Finalita.Numero  )
AND  ( dbo.Conferimenti_Finalita.Finalita=dbo_Anag_Finalita_Confer.Codice  )
AND  (
  {fn year(dbo.Conferimenti.Data_Accettazione)}  >=  2019
)"