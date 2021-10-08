A <- dtanalisi %>% 
  filter(CDC == "SEDE TERRITORIALE DI BERGAMO" & Categoria == "Personale") %>% 
  group_by(ANNO) %>% 
    summarise( costiP = sum(Costo))


cc %>% 
  filter(CDC == "SEDE TERRITORIALE DI BERGAMO" & Categoria == "Personale") %>% 
  group_by(ANNO, Categoria ) %>% 
  summarise( costiP = sum(Costo), 
             n = sum(Numero))

























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
cc <- con %>% tbl(sql(queryCoge)) %>% as_tibble() 
