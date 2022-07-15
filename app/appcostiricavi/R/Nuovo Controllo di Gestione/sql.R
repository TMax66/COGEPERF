dw_coge <- "SELECT
  dbo.IZS_Livello0.CODICE_Livello0,
  dbo.IZS_Livello0.Livello0,
  dbo.IZS_Dipartimenti.CODICE_DIPARTIMENTO,
  dbo.IZS_Dipartimenti.DIPARTIMENTO,
  dbo.IZS_Reparti.CODICE_REPARTO,
  dbo.IZS_Reparti.REPARTO,
  dbo.IZS_CDC.CODICE_CDC,
  dbo.IZS_CDC.CENTRO_DI_COSTO,
  dbo.Elenco_Tipi_Movimenti.Descrizione AS tipomov,
  dbo.Elenco_Origini.DESCRIZIONE AS origine,
  dbo.IZS_Riclassificazione.Descrizione AS riclassificazione,
  dbo.IZS_TIPI.Descrizione AS tipi,
  dbo.IZS_Categorie.Descrizione AS categoria,
  dbo.IZS_Classi.Descrizione AS classe,
  dbo.IZS_Aree.Descrizione AS area,
  dbo.IZS_ANNI.ANNO,
  dbo.IZS_TRIMESTRI.TRIMESTRE,
  dbo.IZS_TRIMESTRI.Descrizione AS trim,
  dbo.IZS_MESI.Descrizione AS mesi,
  dbo.CDC_MOVIMENTI_BO.Settore,
  dbo.IZS_Riclassificazione.Gruppo,
  dbo.IZS_Riclassificazione.Ufficialita,
  dbo.IZS_PRODOTTI.Descrizione AS prodotti,
  dbo.CDC_MOVIMENTI_BO.SOTTOCONTO,
  dbo.CDC_MOVIMENTI_BO.Dati,
  dbo.Booleani.Descrizione AS boo,
  dbo.CDC_MOVIMENTI_BO.Commessa,
  dbo.IZS_TIPI_DOCUMENTI.Descrizione AS tipidoc,
  dbo.CDC_MOVIMENTI_BO.RagioneSociale AS ragsociale,
  sum(dbo.CDC_MOVIMENTI_BO.Costo) As costo,
  sum(dbo.CDC_MOVIMENTI_BO.Nominale) AS tariffario,
  sum(dbo.CDC_MOVIMENTI_BO.Reale) AS fatturato,
  sum(dbo.CDC_MOVIMENTI_BO.Quantita) AS numero,
  sum(dbo.CDC_MOVIMENTI_BO.Determinazioni)AS determinazioni
FROM
  dbo.Elenco_Origini INNER JOIN dbo.CDC_MOVIMENTI_BO ON (dbo.CDC_MOVIMENTI_BO.ORIGINE=dbo.Elenco_Origini.ID)
   RIGHT OUTER JOIN dbo.IZS_PRODOTTI ON (dbo.CDC_MOVIMENTI_BO.Codice=dbo.IZS_PRODOTTI.CodSAI)
   INNER JOIN dbo.IZS_Aree ON (dbo.IZS_Aree.TipoCostoRicavo=dbo.CDC_MOVIMENTI_BO.TipoCostoRicavo and dbo.IZS_Aree.Codice_classe=dbo.CDC_MOVIMENTI_BO.Classe and dbo.IZS_Aree.Codice_area=dbo.CDC_MOVIMENTI_BO.Area)
   INNER JOIN dbo.IZS_Classi ON (dbo.IZS_Classi.TipoCostoRicavo=dbo.IZS_Aree.TipoCostoRicavo and dbo.IZS_Classi.Codice=dbo.IZS_Aree.Codice_classe)
   INNER JOIN dbo.IZS_Categorie ON (dbo.IZS_Categorie.TipoCostoRicavo=dbo.IZS_Classi.TipoCostoRicavo and dbo.IZS_Categorie.Codice=dbo.IZS_Classi.Codice_categoria)
   INNER JOIN dbo.IZS_Classificazioni ON (dbo.IZS_Classificazioni.idClassificazione=dbo.CDC_MOVIMENTI_BO.IdClassificazione)
   INNER JOIN dbo.IZS_Riclassificazione ON (dbo.IZS_Riclassificazione.idClassificazione=dbo.IZS_Classificazioni.idRiclassifica)
   INNER JOIN dbo.Elenco_Tipi_Movimenti ON (dbo.CDC_MOVIMENTI_BO.TipoCostoRicavo=dbo.Elenco_Tipi_Movimenti.TipoMovimento)
   INNER JOIN dbo.Booleani ON (dbo.CDC_MOVIMENTI_BO.Progetto=dbo.Booleani.ID_SQL)
   INNER JOIN dbo.IZS_TIPI ON (dbo.CDC_MOVIMENTI_BO.Tipo=dbo.IZS_TIPI.Tipo)
   RIGHT OUTER JOIN dbo.IZS_TIPI_DOCUMENTI ON (dbo.CDC_MOVIMENTI_BO.Documento_Tipo=dbo.IZS_TIPI_DOCUMENTI.TipoDocumento)
   INNER JOIN dbo.IZS_CDC ON (dbo.IZS_CDC.CODICE_CDC=dbo.CDC_MOVIMENTI_BO.CDC)
   INNER JOIN dbo.IZS_Reparti ON (dbo.IZS_Reparti.CODICE_REPARTO=dbo.IZS_CDC.CODICE_REPARTO)
   INNER JOIN dbo.IZS_Dipartimenti ON (dbo.IZS_Dipartimenti.CODICE_DIPARTIMENTO=dbo.IZS_Reparti.CODICE_DIPARTIMENTO)
   INNER JOIN dbo.IZS_Livello0 ON (dbo.IZS_Livello0.CODICE_Livello0=dbo.IZS_Dipartimenti.Codice_Livello0)
   INNER JOIN dbo.IZS_ANNI ON (dbo.IZS_ANNI.ANNO=dbo.CDC_MOVIMENTI_BO.ANNO)
   INNER JOIN dbo.IZS_MESI ON (dbo.IZS_MESI.MESE=dbo.CDC_MOVIMENTI_BO.MESE)
   INNER JOIN dbo.IZS_TRIMESTRI ON (dbo.IZS_TRIMESTRI.TRIMESTRE=dbo.CDC_MOVIMENTI_BO.TRIMESTRE)
  
WHERE
  dbo.IZS_ANNI.ANNO  >=  2019
GROUP BY
  dbo.IZS_Livello0.CODICE_Livello0, 
  dbo.IZS_Livello0.Livello0, 
  dbo.IZS_Dipartimenti.CODICE_DIPARTIMENTO, 
  dbo.IZS_Dipartimenti.DIPARTIMENTO, 
  dbo.IZS_Reparti.CODICE_REPARTO, 
  dbo.IZS_Reparti.REPARTO, 
  dbo.IZS_CDC.CODICE_CDC, 
  dbo.IZS_CDC.CENTRO_DI_COSTO, 
  dbo.Elenco_Tipi_Movimenti.Descrizione, 
  dbo.Elenco_Origini.DESCRIZIONE, 
  dbo.IZS_Riclassificazione.Descrizione, 
  dbo.IZS_TIPI.Descrizione, 
  dbo.IZS_Categorie.Descrizione, 
  dbo.IZS_Classi.Descrizione, 
  dbo.IZS_Aree.Descrizione, 
  dbo.IZS_ANNI.ANNO, 
  dbo.IZS_TRIMESTRI.TRIMESTRE, 
  dbo.IZS_TRIMESTRI.Descrizione, 
  dbo.IZS_MESI.Descrizione, 
  dbo.CDC_MOVIMENTI_BO.Settore, 
  dbo.IZS_Riclassificazione.Gruppo, 
  dbo.IZS_Riclassificazione.Ufficialita, 
  dbo.IZS_PRODOTTI.Descrizione, 
  dbo.CDC_MOVIMENTI_BO.SOTTOCONTO, 
  dbo.CDC_MOVIMENTI_BO.Dati, 
  dbo.Booleani.Descrizione, 
  dbo.CDC_MOVIMENTI_BO.Commessa, 
  dbo.IZS_TIPI_DOCUMENTI.Descrizione, 
  dbo.CDC_MOVIMENTI_BO.RagioneSociale


"