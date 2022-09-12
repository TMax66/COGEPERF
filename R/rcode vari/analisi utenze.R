library(odbc)
library(DBI)
library(openxlsx)
library(sparkline)
library(formattable)
con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbprod02",
                         Database = "DW_COGE", Port = 1433)


query <-
  "SELECT
  sum(dbo.CDC_MOVIMENTI_BO.Costo) As CostiUtenza,
  dbo.IZS_CDC.CODICE_CDC,
  dbo.IZS_CDC.CENTRO_DI_COSTO,
  dbo.IZS_Reparti.CODICE_REPARTO,
  dbo.IZS_Reparti.REPARTO,
  dbo.IZS_Dipartimenti.CODICE_DIPARTIMENTO,
  dbo.IZS_Dipartimenti.DIPARTIMENTO,
  dbo.IZS_Livello0.CODICE_Livello0,
  dbo.IZS_Livello0.Livello0,
  dbo.IZS_Classi.Codice,
  dbo.IZS_Classi.Descrizione As Classi,
  dbo.IZS_Aree.Codice_area,
  dbo.IZS_Aree.Descrizione As Aree,
  dbo.IZS_ANNI.ANNO,
  dbo.CDC_MOVIMENTI_BO.SOTTOCONTO,
  dbo.CDC_MOVIMENTI_BO.Dati,
  dbo.IZS_PRODOTTI.CodSAI,
  dbo.IZS_PRODOTTI.Descrizione As prodotti
FROM
  dbo.IZS_Classi INNER JOIN dbo.IZS_Aree ON (dbo.IZS_Classi.TipoCostoRicavo=dbo.IZS_Aree.TipoCostoRicavo and dbo.IZS_Classi.Codice=dbo.IZS_Aree.Codice_classe)
   INNER JOIN dbo.CDC_MOVIMENTI_BO ON (dbo.IZS_Aree.TipoCostoRicavo=dbo.CDC_MOVIMENTI_BO.TipoCostoRicavo and dbo.IZS_Aree.Codice_classe=dbo.CDC_MOVIMENTI_BO.Classe and dbo.IZS_Aree.Codice_area=dbo.CDC_MOVIMENTI_BO.Area)
   RIGHT OUTER JOIN dbo.IZS_PRODOTTI ON (dbo.CDC_MOVIMENTI_BO.Codice=dbo.IZS_PRODOTTI.CodSAI)
   INNER JOIN dbo.IZS_CDC ON (dbo.IZS_CDC.CODICE_CDC=dbo.CDC_MOVIMENTI_BO.CDC)
   INNER JOIN dbo.IZS_Reparti ON (dbo.IZS_Reparti.CODICE_REPARTO=dbo.IZS_CDC.CODICE_REPARTO)
   INNER JOIN dbo.IZS_Dipartimenti ON (dbo.IZS_Dipartimenti.CODICE_DIPARTIMENTO=dbo.IZS_Reparti.CODICE_DIPARTIMENTO)
   INNER JOIN dbo.IZS_Livello0 ON (dbo.IZS_Livello0.CODICE_Livello0=dbo.IZS_Dipartimenti.Codice_Livello0)
   INNER JOIN dbo.IZS_ANNI ON (dbo.IZS_ANNI.ANNO=dbo.CDC_MOVIMENTI_BO.ANNO)
  
WHERE
  (
   dbo.IZS_Classi.Descrizione  IN  ( 'Utenze'  )
   AND
   dbo.IZS_Aree.Descrizione  IN  ( 'acqua','gas','energia elettrica'  )
   AND
   dbo.IZS_ANNI.ANNO  IN  ( 2019, 2020, 2021, 2022  )
  )
GROUP BY
  dbo.IZS_CDC.CODICE_CDC, 
  dbo.IZS_CDC.CENTRO_DI_COSTO, 
  dbo.IZS_Reparti.CODICE_REPARTO, 
  dbo.IZS_Reparti.REPARTO, 
  dbo.IZS_Dipartimenti.CODICE_DIPARTIMENTO, 
  dbo.IZS_Dipartimenti.DIPARTIMENTO, 
  dbo.IZS_Livello0.CODICE_Livello0, 
  dbo.IZS_Livello0.Livello0, 
  dbo.IZS_Classi.Codice, 
  dbo.IZS_Classi.Descrizione, 
  dbo.IZS_Aree.Codice_area, 
  dbo.IZS_Aree.Descrizione, 
  dbo.IZS_ANNI.ANNO, 
  dbo.CDC_MOVIMENTI_BO.SOTTOCONTO, 
  dbo.CDC_MOVIMENTI_BO.Dati, 
  dbo.IZS_PRODOTTI.CodSAI, 
  dbo.IZS_PRODOTTI.Descrizione
"

dt <- dbGetQuery(con, query)

saveRDS(dt, here("data", "processed", "utenze_energetiche.RDS"))

dt %>% filter(!str_detect(DIPARTIMENTO, "COSTI")) %>% 
  filter(!str_detect(DIPARTIMENTO, "CENTRO")) %>% 
  filter(Livello0 %in% c("Dipartimento area territoriale Emilia Romagna", 
                         "Dipartimento area territoriale Lombardia", 
                         "Dipartimento sicurezza alimentare",
                         "Dipartimento tutela e salute animale", 
                         "Dipartimento amministrativo")) %>% 
  group_by(ANNO, Livello0) %>%  #REPARTO, CENTRO_DI_COSTO,  ) %>% 
  filter(Aree == "gas") %>% 
  summarise("costo_utenze"= sum(CostiUtenza, na.rm = TRUE)) %>% 
  pivot_wider(names_from = "ANNO", values_from = "costo_utenze") %>% 
  
  left_join(
    (dt %>% filter(!str_detect(DIPARTIMENTO, "COSTI")) %>% 
      filter(!str_detect(DIPARTIMENTO, "CENTRO")) %>% 
      filter(Livello0 %in% c("Dipartimento area territoriale Emilia Romagna", 
                             "Dipartimento area territoriale Lombardia", 
                             "Dipartimento sicurezza alimentare",
                             "Dipartimento tutela e salute animale", 
                             "Dipartimento amministrativo")) %>% 
      group_by(ANNO, Livello0) %>%  #REPARTO, CENTRO_DI_COSTO,  ) %>% 
      filter(Aree == "gas") %>% 
      summarise("costo_utenze"= sum(CostiUtenza, na.rm = TRUE)) %>% ungroup() %>% 
      select(-ANNO) %>% 
      group_by(Livello0) %>% 
      summarise(trend = spk_chr(costo_utenze, type= "line", options =
                                  list(paging = FALSE))) 
     ))%>% 
      rename("Dipartimento" = Livello0) %>% 
      
      format_table()  %>%  
      htmltools::HTML() %>% 
      div() %>% 
      spk_add_deps()
      
    
    
    
    
 



 
  #write.xlsx(file = "gasdip.xlsx")
  













AC <- function(CC = input$CC ){ 
  dtanalisi %>%  
    dplyr::filter(Costi== "Ricavo") %>% 
    group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
    filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%  
    summarise(N_Det = sum(Determinazioni, na.rm = TRUE),
              N_Num = sum(Numero, na.rm = TRUE), 
              S_Tariffa = sum(Tariffario, na.rm = TRUE), 
              S_Fatturato = sum(Fatturato, na.rm = TRUE))  %>% 
    dplyr::filter(CDC == CC & Classe == "Prestazioni") %>% 
    group_by(ANNO, Quarter, Area) %>% 
    summarise(N = sum(N_Det, na.rm=TRUE)) %>% 
    mutate(YQ = paste(ANNO, "-", Quarter)) %>% ungroup() %>% 
    select(-ANNO, -Quarter) %>% 
    pivot_wider( names_from = YQ,  values_from = N, values_fill = 0) %>%   
    left_join(  
      
      (dtanalisi %>% 
         dplyr::filter(Costi == "Ricavo") %>% 
         group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC,ClassAnalisi, Classe, Area) %>% 
         filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%  
         summarise(N_Det = sum(Determinazioni, na.rm = TRUE),
                   N_Num = sum(Numero, na.rm = TRUE), 
                   S_Tariffa = sum(Tariffario, na.rm = TRUE), 
                   S_Fatturato = sum(Fatturato, na.rm = TRUE))  %>% 
         dplyr::filter(CDC == CC & Classe == "Prestazioni") %>% 
         group_by(ANNO, Quarter, Area) %>% 
         summarise(N = sum(N_Det, na.rm=TRUE)) %>% 
         mutate(YQ = paste(ANNO, "-", Quarter)) %>%
         select(-ANNO, -Quarter) %>% 
         group_by(Area) %>%
         summarise(trend = spk_chr(N, type= "line", options =
                                     list(paging = FALSE)))
      )) %>% rename("Prestazioni" = Area) %>% 
    
    format_table()  %>% 
    htmltools::HTML() %>% 
    div() %>% 
    spk_add_deps()
}













  

dt %>% filter(!str_detect(DIPARTIMENTO, "COSTI")) %>% 
  filter(!str_detect(DIPARTIMENTO, "CENTRO")) %>% 
  filter(Livello0 %in% c("Dipartimento area territoriale Emilia Romagna", 
                         "Dipartimento area territoriale Lombardia", 
                         "Dipartimento sicurezza alimentare",
                         "Dipartimento tutela e salute animale",
                         "Dipartimento amministrativo"
                         )) %>% 
  group_by(ANNO, Livello0, DIPARTIMENTO, REPARTO, CENTRO_DI_COSTO ) %>% 
  filter(Aree == "gas") %>% 
  summarise("costo_utenze"= sum(CostiUtenza, na.rm = TRUE)) %>% 
  pivot_wider(names_from = "ANNO", values_from = "costo_utenze") %>% 
  write.xlsx(file = "gas.xlsx")

  
  
  
  



