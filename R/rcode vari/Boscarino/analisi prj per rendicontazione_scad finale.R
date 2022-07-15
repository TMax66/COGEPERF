library(tidyverse)
library(readr)
library(readxl)
library(here)
library(lubridate)
library(DBI)
library(odbc)
library(DataEditR)
library(knitr)
library(kableExtra)


#PROGETTI----
con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbprod03.izsler.it", 
                       Database = "ProgettiAccordi")

DBI::dbListFields(con, "ProgettiAccordi")

# query <- "SELECT        
#           Codice, 
#           CodIDIzsler, 
#           Tipologia, 
#           Descrizione As Titolo, 
#           RespScientifico, 
#           MatrRespScientifico, 
#           Pubblicazioni, 
#           NumeroPubblicazioni, 
#           DataScadenzaRelazioneFinale, 
#           DataScadenzaRelazioneIntermedia, 
#           DataInvioRelazioneFinale, 
#           DataInvioRelazioneIntermedia, 
#           InviatoSollecitoRelazioneFinale, 
#           InviatoSollecitoRelazioneIntermedia, 
#           Prolungato, 
#           InviatoSollecitoRelazioneScadenza, 
#           ProrogatoCoronavirus, 
#           RelazioneIntermediaDaInviare, 
#           DataInizio, 
#           DataFine, 
#           Anno
# FROM      ProgettiAccordi"


Query<-function(fixed="SELECT COLUMN_NAME, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME=", tabella="'nometab'"){
  paste(fixed, tabella)
}

q<-Query(tabella = "'ProgettiAccordi'")

myfun <- function(con, q, tabella)
{   
  
  column.types <- dbGetQuery(con, q)
  
  ct <- column.types %>%
    mutate(cml = case_when(
      is.na(CHARACTER_MAXIMUM_LENGTH) ~ 10,
      CHARACTER_MAXIMUM_LENGTH == -1 ~ 100000,
      TRUE ~ as.double(CHARACTER_MAXIMUM_LENGTH)
    )
    ) %>%
    arrange(cml) %>%
    pull(COLUMN_NAME)
  fields <- paste(ct, collapse=", ")
  query <- paste("SELECT", fields, paste("FROM", tabella))
  return(query)
}

query <- myfun(con=con, q=q, tabella = "ProgettiAccordi")


prj <- con %>% tbl(sql(query)) %>% as_tibble() 


# # codifica TipologieProgetto
# DBI::dbListFields(con, "TipologieProgetto")
# query_tipologia <- "SELECT * FROM dbo.TipologieProgetto"
# df0 <- con %>% tbl(sql(query_tipologia)) %>% as_tibble()
# View(df0)


# estrazione dati per rendicontazione obiettivi

prj <- prj %>% 
  mutate(annoinizio = year(DataInizio), 
         annofine = year(DataFine), 
         annoIntermedia = year(DataScadenzaRelazioneIntermedia)) %>% 
  filter(annofine == 2022 |
           annoIntermedia == 2022) %>%
  select(Codice,CodIDIzsler, Tipologia, Descrizione, RespScientifico, MatrRespScientifico, DataInizio, DataFine, 
         Stato, StatoRelazioneIntermedia , DataScadenzaRelazioneFinale, DataScadenzaRelazioneIntermedia, Prolungato, 
         InviatoSollecitoRelazioneFinale, InviatoSollecitoRelazioneIntermedia, RelazioneIntermediaDaInviare) %>% 
  mutate(
    Stato = recode(Stato,
   `1` = "annullato", 
   `2` = "inviato", 
   `3` = "in attesa", 
   `4` = "in corso", 
   `5` = "terminato", 
   `6` = "terminato non richiesta relazione"), 
    StatoRelazioneIntermedia = recode(StatoRelazioneIntermedia, 
   `0` = "non impostato", 
   `1` = "non prevista", 
   `2` = "non inviata", 
   `3` = "inviata", 
   `4` = "approvata", 
   `5` = "non approvata"),
    Tipologia = recode(Tipologia,
   `0` = "Autofinanziato",
   `1` = "Finalizzato", 
   `2` = "Corrente", 
   `3` = "Altro tipo", 
   `4` = "Regionali", 
   `5` = "Europeo", 
   `6` = "CCM",
   `7` = "Istituzionale")
    ) %>%  
  filter(Stato == "in corso" &
         StatoRelazioneIntermedia != "non prevista", 
         DataScadenzaRelazioneIntermedia <= "2022-06-30")


prj <- prj %>% 
  mutate(annoinizio = format(as.Date(DataInizio, format="%d/%m/%Y"),"%Y"))



#ANAGRAFICA----
conOre <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbprod02",
                         Database = "DW_COGE", Port = 1433)

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


ore <- conOre %>% tbl(sql(queryOre)) %>% as_tibble()  ### FTEq
#ore <- readRDS("ore.rds")

names(ore)[1:6] <- c("Dipartimento", "Reparto", "Laboratorio", "CDC", "CodiceCC", "ANNO")

ore <- ore %>% 
  filter(Ruolo != "RICERCA SANITARIA E SUPPORTO RIC. SAN.")

anag <- ore %>% 
  mutate(annoraplav = year(FineRapporto)) %>% 
  filter(annoraplav > 2018)%>%
  mutate(Nome = gsub("\\s.*$", "", Nome)) %>% 
  distinct(Matricola, ANNO, .keep_all= TRUE)

anag <- anag[,c(1,2,6,7,15,16)]

#JOIN PRJ-ANAG----
prj_anag <- prj %>%
  left_join(anag, by = c("MatrRespScientifico" = "Matricola")) %>% 
  filter(annoinizio == ANNO)

#PROGETTI MANCANTI
na <- anti_join(prj, prj_anag)


prj_full <- prj_anag %>% 
  bind_rows(na)

View(prj_full)

# library(editData)
# editData(prj_full)
#prj_rel_intermedia <- DataEditR::data_edit(prj_full, viewer = "browser")
prj_rel_intermedia <- read.csv("prj_rel_intermedia_20220713-data.csv")
prj_rel_intermedia <- prj_rel_intermedia %>%  mutate(
    Tipologia = recode(Tipologia,
   `0` = "Autofinanziato",
   `1` = "Finalizzato", 
   `2` = "Corrente", 
   `3` = "Altro tipo", 
   `4` = "Regionali", 
   `5` = "Europeo", 
   `6` = "CCM",
   `7` = "Istituzionale")
    )
prj_rel_intermedia <- prj_rel_intermedia[,-1]
#View(prj_rel_intermedia)

prj_kbl_intermedia <- prj_rel_intermedia %>% 
  group_by(Reparto) %>% 
  summarise(
    total_prj = n_distinct(Codice),
    total_no_sollecito = sum(InviatoSollecitoRelazioneIntermedia=="FALSE"),
    perc_rel_inviate = paste0(round(total_no_sollecito/total_prj*100),"%"))


prj_kbl_intermedia %>%
kbl(caption = 'Rendicontazione obiettivo : blablabla', 
    col.names = c('Reparto',
                'Progetti in corso',
                'Relazioni intermedie<br/>presentate *',
                'Raggiungimento obiettivo'),
    align = "lccc",
    escape = FALSE) %>%
  kable_classic(full_width = F, html_font = "Cambria")%>%
  column_spec(2:4, width = "12em") %>% 
  footnote(general_title = "", general = "* entro la scadenza del 30 giugno 2022.")
  

prj_rel_intermedia %>% 
  mutate(`Resp. scientifico` = paste0(Nome," ",Cognome)) %>% 
  select(CodIDIzsler, `Resp. scientifico`, Reparto)%>%
  kbl(caption = 'Lista progetti per rendicontazione obiettivo con relazione intermedia da presentare entro il 30/06/2022') %>% 
  kable_classic(full_width = F, html_font = "Cambria")%>%
  save_kable(file = "progetti_relazioni intermedie.pdf")


# library(gt)
#   gt() %>% 
#     tab_style(style = list(
#     cell_text(align = "center")),
#     locations = cells_body(columns= c(total_prj,total_ok,perc_rel_inviate))) %>% 
#   cols_align(
#   align = c("center"),
#   columns = c(total_prj,total_ok,perc_rel_inviate)) %>% 
#   cols_width(`total_prj` ~ pct(10),
#              total_ok ~ pct(10),
#              `perc_rel_inviate` ~ pct(10)) %>% 
#   cols_label(
#     total_prj = html("Progetti in corso"),
#     total_ok = html("Relazioni intermedie da inviare *"),
#     perc_rel_inviate = html("Percentuale raggiungimento obiettivo")) %>% 
#    tab_source_note(source_note = "* entro la scadenza del 30 giugno 2022.")
