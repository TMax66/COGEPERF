#Query----




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
dbo.Personale_V2020.Cognome,
dbo.Personale_V2020.Ruolo
FROM
dbo.Personale_V2020 INNER JOIN dbo.IZS_CDC ON (dbo.Personale_V2020.CDC=dbo.IZS_CDC.CODICE_CDC)
INNER JOIN dbo.IZS_Reparti ON (dbo.IZS_CDC.CODICE_REPARTO=dbo.IZS_Reparti.CODICE_REPARTO)
INNER JOIN dbo.IZS_Dipartimenti ON (dbo.IZS_Reparti.CODICE_DIPARTIMENTO=dbo.IZS_Dipartimenti.CODICE_DIPARTIMENTO)
INNER JOIN dbo.IZS_Livello0 ON (dbo.IZS_Dipartimenti.Codice_Livello0=dbo.IZS_Livello0.CODICE_Livello0)

WHERE

dbo.Personale_V2020.Anno  >=  2019"




#### query  Controllo di Gestione per Dashboard Performance e APP Centri di Costo----
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


#### query dati per accettazione centralizzata----

queryAcc <- ("SELECT DISTINCT 
                         Conferimenti.Nome_Stazione_Inserimento, Conferimenti.Matr_Ins, Conferimenti.Anno, Conferimenti.Numero AS Conferimento, Conferimenti.NrCampioni, Anag_TipoConf.Descrizione, 
                         Anag_Settori_Attività_Form.Descrizione AS settoreintervento, Anag_Settori_Attività_Form.Codice, Anag_Reparti.Descrizione AS repanalisi, 
                         Anag_Reparti.Codice AS strutturaaccettante_codice, Conferimenti.Consegnato_Portineria, CONVERT(VARCHAR(10), Conferimenti.Data_Accettazione, 111) AS Data_Registrazione, 
                         CONVERT(VARCHAR(10), Conferimenti.Data, 111) AS Data_Ricezione, CONVERT(VARCHAR(10), Conferimenti.Data_Prelievo, 111) AS Data_Prelievo, 
                         Anag_Tipo_Prel.Descrizione AS tipodiprelievo, Anag_Finalita.Descrizione AS FinalitaConferimento, dbo.Conferimenti.Temperatura_Campione_Sentinella_SiNo AS Possibile_Rilevare_T, 
                         Conferimenti.Temperatura_Campione_Sentinella AS T, dbo.Conferimenti.Temperatura_Campione_Sentinella_Fuori_Range AS Fuori_Range, dbo.Conferimenti.Stato_Campione, 
                         Anag_Referenti2.Ragione_Sociale AS Conferente, Anag_Referenti.Ragione_Sociale AS denominazazienda, Anag_Referenti.Indirizzo AS indirizzo, Anag_Comuni.Descrizione AS comune, 
                         Anag_Comuni.Provincia AS provincia, Conferimenti.Allevix_Proprietario AS codiceazienda, Anag_Referenti.Numero_Codice_Allevix AS Sottocodice, Conferimenti.Riferimenti AS Verbale, 
                         Anag_Referenti1.Ragione_Sociale AS Veterinario, Anag_Referenti3.Ragione_Sociale AS Dest_fattura, Anag_Referenti4.Ragione_Sociale AS Dest_Rapporto_Prova, 
                         Anag_Specie.Descrizione AS specie, Anag_Materiali.Descrizione AS Materiale, Anag_Matrici.Descrizione AS Matrice, Conferimenti.Smaltimento_Carcasse, 
                         Anag_Comuni.Regione AS regione, Anag_Asl.Descrizione AS ASL, Anag_Asl.CodiceMinistero, Anag_Asl.Cod_ISTAT AS CodiceASL, Conferimenti.Conferimento_Chiuso, 
                         Anag_Prove.Codice AS CodiceProva, Anag_Prove.Descrizione AS Prova, Anag_Gruppo_Prove.Descrizione AS gProva, Anag_Laboratori.Descrizione AS labanalisi, 
                         Laboratori_Reparto.Chiave, Anag_Laboratori_1.Descrizione AS labacc, Anag_Reparti_1.Descrizione AS strutturaaccettante, Anag_Reparti_1.Locazione
FROM         Anag_Esiti RIGHT OUTER JOIN
                         Risultati_Analisi RIGHT OUTER JOIN
                         Anag_Tecniche RIGHT OUTER JOIN
                         Anag_Prove RIGHT OUTER JOIN
                         Anag_Laboratori RIGHT OUTER JOIN
                         Anag_Settori_Attività_Form RIGHT OUTER JOIN
                         Anag_Finalita RIGHT OUTER JOIN
                         Anag_Comuni RIGHT OUTER JOIN
                         Programmazione INNER JOIN
                         Laboratori_Reparto AS Laboratori_Reparto_1 ON Programmazione.RepLab_analisi = Laboratori_Reparto_1.Chiave RIGHT OUTER JOIN
                         Laboratori_Reparto INNER JOIN
                         Conferimenti ON Laboratori_Reparto.Chiave = Conferimenti.RepLab_Conferente LEFT OUTER JOIN
                         Anag_Laboratori AS Anag_Laboratori_1 ON Laboratori_Reparto.Laboratorio = Anag_Laboratori_1.Codice ON Programmazione.Anno_Conferimento = Conferimenti.Anno AND 
                         Programmazione.Numero_Conferimento = Conferimenti.Numero LEFT OUTER JOIN
                         Anag_Reparti AS Anag_Reparti_1 ON Laboratori_Reparto.Reparto = Anag_Reparti_1.Codice LEFT OUTER JOIN
                         Anag_Referenti AS Anag_Referenti3 ON Conferimenti.Dest_Fattura = Anag_Referenti3.Codice LEFT OUTER JOIN
                         Anag_Referenti AS Anag_Referenti2 ON Conferimenti.Conferente = Anag_Referenti2.Codice LEFT OUTER JOIN
                         Anag_Referenti ON Conferimenti.Proprietario = Anag_Referenti.Codice LEFT OUTER JOIN
                         Anag_Referenti AS Anag_Referenti1 ON Conferimenti.Veterinario = Anag_Referenti1.Codice LEFT OUTER JOIN
                         Anag_Referenti AS Anag_Referenti4 ON Conferimenti.Dest_Rapporto_Prova = Anag_Referenti4.Codice ON Anag_Comuni.Codice = Anag_Referenti.Comune LEFT OUTER JOIN
                         Conferimenti_Finalita ON Conferimenti.Anno = Conferimenti_Finalita.Anno AND Conferimenti.Numero = Conferimenti_Finalita.Numero LEFT OUTER JOIN
                         Anag_Specie ON Conferimenti.Codice_Specie = Anag_Specie.Codice LEFT OUTER JOIN
                         Anag_TipoConf ON Conferimenti.Tipo = Anag_TipoConf.Codice LEFT OUTER JOIN
                         Anag_Materiali ON Conferimenti.Codice_Materiale = Anag_Materiali.Codice LEFT OUTER JOIN
                         Anag_Asl ON Conferimenti.Cod_Asl = Anag_Asl.codice LEFT OUTER JOIN
                         Anag_Matrici ON Conferimenti.Matrice = Anag_Matrici.Codice ON Anag_Finalita.Codice = Conferimenti_Finalita.Finalita LEFT OUTER JOIN
                         Anag_Tipo_Prel ON Conferimenti.Tipo_Prelievo = Anag_Tipo_Prel.Codice ON Anag_Settori_Attività_Form.Codice = Conferimenti.Registro LEFT OUTER JOIN
                         Anag_Reparti ON Laboratori_Reparto_1.Reparto = Anag_Reparti.Codice ON Anag_Laboratori.Codice = Laboratori_Reparto_1.Laboratorio LEFT OUTER JOIN
                         Nomenclatore_MP ON Programmazione.Nomenclatore = Nomenclatore_MP.Codice LEFT OUTER JOIN
                         Nomenclatore_Settori ON Nomenclatore_MP.Nomenclatore_Settore = Nomenclatore_Settori.Codice LEFT OUTER JOIN
                         Anag_Gruppo_Prove INNER JOIN
                         Nomenclatore ON Anag_Gruppo_Prove.Codice = Nomenclatore.Codice_Gruppo ON Nomenclatore_Settori.Codice_Nomenclatore = Nomenclatore.Chiave ON 
                         Anag_Prove.Codice = Nomenclatore.Codice_Prova ON Anag_Tecniche.Codice = Nomenclatore.Codice_Tecnica ON 
                         Risultati_Analisi.Anno_Conferimento = Programmazione.Anno_Conferimento AND Risultati_Analisi.Numero_Conferimento = Programmazione.Numero_Conferimento AND 
                         Risultati_Analisi.Codice = Programmazione.Codice ON Anag_Esiti.Codice = Risultati_Analisi.Esito LEFT OUTER JOIN
                         Nomenclatore_Range ON Risultati_Analisi.Range = Nomenclatore_Range.Codice
WHERE     (Conferimenti.Anno >= 2021) AND (Conferimenti.Conferimento_Chiuso = 1) AND (Conferimenti.Nome_Stazione_Inserimento = 'ACC-CENTR2' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'PC-47326' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'PC-40780' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'MP-ACC3' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'BS-ASS-N' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'PC-47327' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'CH-ACC4-N' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'CH-ACC2-N' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'PC-49676' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'PC-47499' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'PC-49694' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'OEVR-8-PORT' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'PC-50901' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'NB-50619' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'NB-50696' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'MP-SIVARS7-N' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'PC-49702' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'PC-49703' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'PC-47929' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'PC-51010' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'PC-50957' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'PC-46945')")









#### query dati performance 2021

# queryPERF <- "SELECT
# Avanzamento,
# Valore,
# Target, 
# Anno,
# TipoObiettivo,
# Periodo,
# MacroArea,Obiettivo,
# Azione,
# Indicatore,
# StrutturaAssegnataria
# 
# FROM ObiettiviStrategiciV2018.dbo.v_EstrazioneObiettivi
# WHERE Anno > 2020"


#### query dati schede budget e performance dal 2022----

# Query<-function(fixed="SELECT COLUMN_NAME, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME=", tabella="'nometab'"){
#   paste(fixed, tabella)
# }
# 
# q<-Query(tabella = "'vSchedaBudget'")
# 
# myfun <- function(con, q, tabella)
# {
# 
#   column.types <- dbGetQuery(con, q)
# 
#   ct <- column.types %>%
#     mutate(cml = case_when(
#       is.na(CHARACTER_MAXIMUM_LENGTH) ~ 10,
#       CHARACTER_MAXIMUM_LENGTH == -1 ~ 100000,
#       TRUE ~ as.double(CHARACTER_MAXIMUM_LENGTH)
#     )
#     ) %>%
#     arrange(cml) %>%
#     pull(COLUMN_NAME)
#   fields <- paste(ct, collapse=", ")
#   query <- paste("SELECT", fields, paste("FROM", tabella))
#   return(query)
# }
# 
# query <- myfun(con=conSB, q=q, tabella = "vSchedaBudget")
# 
