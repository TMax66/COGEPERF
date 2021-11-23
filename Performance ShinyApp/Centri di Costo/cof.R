con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbtest02", 
                      Database = "DW_COGE_DEV", Port = 1433)



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
                  IZS_CDC.CENTRO_DI_COSTO AS CDC, 
dbo.Personale_Anagrafe_V2020.Tempo AS Tempo
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
dbo.Personale_Anagrafe_V2020.Tempo"





cc <- con %>% tbl(sql(queryCoge)) %>% as_tibble() 

cc %>% 
  mutate(ClassAnalisi = recode(idClassificazione, 
                               `-1` = "Ufficiale a Pagamento", 
                               `-3` = "Ufficiale a Pagamento", 
                               `-8` = "Non Ufficiale a Pagamento", 
                               `-9` = "Non Ufficiale a Pagamento", 
                               `-4` = "Ufficiale Gratuito", 
                               `-5` = "Ufficiale Gratuito", 
                               `-7` = "Ufficiale Gratuito", 
                               `-11` = "Ufficiale Gratuito", 
                               `-6`  = "Non Ufficiale Gratuito", 
                               `-10` = "Non Ufficiale Gratuito", 
                               `-13` = "Non Ufficiale Gratuito" ,  .default = NA_character_),
         Pagamento = recode(idClassificazione, 
                            `-1` = "Pagamento", 
                            `-3` = "Pagamento", 
                            `-8` = "Pagamento", 
                            `-9` = "Pagamento", 
                            `-4` = "Gratuito", 
                            `-5` = "Gratuito", 
                            `-7` = "Gratuito", 
                            `-11` = "Gratuito", 
                            `-6`  = "Gratuito", 
                            `-10` = "Gratuito", 
                            `-13` = "Gratuito" ,  .default = NA_character_), 
         Uff = recode (idClassificazione, 
                       `-1` = "Ufficiale", 
                       `-3` = "Ufficiale", 
                       `-8` = "Non Ufficiale", 
                       `-9` = "Non Ufficiale", 
                       `-4` = "Ufficiale", 
                       `-5` = "Ufficiale", 
                       `-7` = "Ufficiale", 
                       `-11` = "Ufficiale", 
                       `-6`  = "Non Ufficiale", 
                       `-10` = "Non Ufficiale", 
                       `-13` = "Non Ufficiale", .default = NA_character_), 
         
         Quarter = factor(paste("Q",TRIMESTRE)),
         TUff = ifelse(ClassAnalisi == "Ufficiale a Pagamento", Fatturato,
                       ifelse(ClassAnalisi == "Ufficiale Gratuito",  Tariffario, 0)),
         TNonUff = ifelse(ClassAnalisi == "Non Ufficiale a Pagamento", Fatturato,
                          ifelse(ClassAnalisi == "Non Ufficiale Gratuito", Tariffario, 0)),
         TGratuito = ifelse(Pagamento == "Gratuito", Tariffario,0), 
         TPagamento = ifelse(Pagamento == "Pagamento", Fatturato,0), 
         TVP = ifelse(Classe == "Vendite prodotti", Fatturato, 0), 
         TAI = ifelse(Classe == "Ricavi da produzione interna", Tariffario, 0), 
         AttUff = ifelse(Uff== "Ufficiale", Determinazioni, 0 ), 
         AttNUff = ifelse(Uff== "Non Ufficiale", Determinazioni, 0 ), 
         AttGrat = ifelse(Pagamento== "Gratuito", Determinazioni, 0 ), 
         AttPag = ifelse(Pagamento == "Pagamento", Determinazioni, 0), 
         VP = ifelse(Classe == "Vendite prodotti", Numero, 0), 
         AI = ifelse(Classe == "Ricavi da produzione interna", Numero, 0)) %>% 
  saveRDS(here("Performance ShinyApp", "Centri di Costo", "CC.rds"))

































































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


