SELECT
dbo.IZS_ANNI.ANNO,
dbo.IZS_TRIMESTRI.TRIMESTRE,
dbo.IZS_MESI.MESE,
dbo.IZS_MESI.Descrizione,
dbo.IZS_Livello0.Livello0,
dbo.IZS_Dipartimenti.DIPARTIMENTO,
dbo.IZS_Reparti.REPARTO,
sum(dbo.CDC_MOVIMENTI_BO.Reale),
sum(dbo.CDC_MOVIMENTI_BO.Nominale),
sum(dbo.CDC_MOVIMENTI_BO.Costo),
sum(dbo.CDC_MOVIMENTI_BO.Determinazioni),
sum(dbo.CDC_MOVIMENTI_BO.Quantita),
dbo.IZS_Categorie.Descrizione,
dbo.IZS_Riclassificazione.Descrizione,
dbo.IZS_Riclassificazione.idClassificazione,
dbo.Elenco_Tipi_Movimenti.Descrizione,
dbo.IZS_Classi.Descrizione,
dbo.IZS_Aree.Descrizione,
dbo.IZS_CDC.CODICE_CDC,
dbo.IZS_CDC.CENTRO_DI_COSTO,
dbo.Personale_Anagrafe_V2020.Tempo
FROM
dbo.IZS_Categorie INNER JOIN dbo.IZS_Classi ON (dbo.IZS_Categorie.TipoCostoRicavo=dbo.IZS_Classi.TipoCostoRicavo and dbo.IZS_Categorie.Codice=dbo.IZS_Classi.Codice_categoria)
INNER JOIN dbo.IZS_Aree ON (dbo.IZS_Classi.TipoCostoRicavo=dbo.IZS_Aree.TipoCostoRicavo and dbo.IZS_Classi.Codice=dbo.IZS_Aree.Codice_classe)
INNER JOIN dbo.CDC_MOVIMENTI_BO ON (dbo.IZS_Aree.TipoCostoRicavo=dbo.CDC_MOVIMENTI_BO.TipoCostoRicavo and dbo.IZS_Aree.Codice_classe=dbo.CDC_MOVIMENTI_BO.Classe and dbo.IZS_Aree.Codice_area=dbo.CDC_MOVIMENTI_BO.Area)
INNER JOIN dbo.IZS_Classificazioni ON (dbo.IZS_Classificazioni.idClassificazione=dbo.CDC_MOVIMENTI_BO.IdClassificazione)
INNER JOIN dbo.IZS_Riclassificazione ON (dbo.IZS_Riclassificazione.idClassificazione=dbo.IZS_Classificazioni.idRiclassifica)
INNER JOIN dbo.Elenco_Tipi_Movimenti ON (dbo.CDC_MOVIMENTI_BO.TipoCostoRicavo=dbo.Elenco_Tipi_Movimenti.TipoMovimento)
INNER JOIN dbo.IZS_CDC ON (dbo.IZS_CDC.CODICE_CDC=dbo.CDC_MOVIMENTI_BO.CDC)
INNER JOIN dbo.IZS_Reparti ON (dbo.IZS_Reparti.CODICE_REPARTO=dbo.IZS_CDC.CODICE_REPARTO)
INNER JOIN dbo.IZS_Dipartimenti ON (dbo.IZS_Dipartimenti.CODICE_DIPARTIMENTO=dbo.IZS_Reparti.CODICE_DIPARTIMENTO)
INNER JOIN dbo.IZS_Livello0 ON (dbo.IZS_Livello0.CODICE_Livello0=dbo.IZS_Dipartimenti.Codice_Livello0)
INNER JOIN dbo.IZS_ANNI ON (dbo.IZS_ANNI.ANNO=dbo.CDC_MOVIMENTI_BO.ANNO)
INNER JOIN dbo.IZS_MESI ON (dbo.IZS_MESI.MESE=dbo.CDC_MOVIMENTI_BO.MESE)
INNER JOIN dbo.IZS_TRIMESTRI ON (dbo.IZS_TRIMESTRI.TRIMESTRE=dbo.CDC_MOVIMENTI_BO.TRIMESTRE)
RIGHT OUTER JOIN dbo.Personale_Anagrafe_V2020 ON (dbo.CDC_MOVIMENTI_BO.Matricola=dbo.Personale_Anagrafe_V2020.Matricola)

WHERE
dbo.IZS_ANNI.ANNO  >=  2019
GROUP BY
dbo.IZS_ANNI.ANNO, 
dbo.IZS_TRIMESTRI.TRIMESTRE, 
dbo.IZS_MESI.MESE, 
dbo.IZS_MESI.Descrizione, 
dbo.IZS_Livello0.Livello0, 
dbo.IZS_Dipartimenti.DIPARTIMENTO, 
dbo.IZS_Reparti.REPARTO, 
dbo.IZS_Categorie.Descrizione, 
dbo.IZS_Riclassificazione.Descrizione, 
dbo.IZS_Riclassificazione.idClassificazione, 
dbo.Elenco_Tipi_Movimenti.Descrizione, 
dbo.IZS_Classi.Descrizione, 
dbo.IZS_Aree.Descrizione, 
dbo.IZS_CDC.CODICE_CDC, 
dbo.IZS_CDC.CENTRO_DI_COSTO, 
dbo.Personale_Anagrafe_V2020.Tempo


