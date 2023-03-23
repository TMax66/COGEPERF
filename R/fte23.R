library(tidyverse)
library(readr)
library(readxl)
library(here)
library(lubridate)
library(DBI)
library(odbc)
library(gt)
library(janitor)
library(webshot)
library(here)



#preparazione dataset----
conSB <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "CED-IIS2",
                        Database = "ObiettiviStrategici2022", Port = 1433)

Query <- function(fixed="SELECT COLUMN_NAME, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME=", tabella="'nometab'"){
  paste(fixed, tabella)
}

q <- Query(tabella = "'vSchedaBudget'")

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

query <- myfun(con=conSB, q=q, tabella = "vSchedaBudget")

fte <- tbl(conSB, sql(query)) %>% 
  as_tibble()

# DATI----
df <- fte %>% 
  mutate(Periodo = replace_na(Periodo, 1)) %>% 
  filter(Anno == 2023, Periodo == 1) %>% 
  mutate(
    Pesatura = ifelse(Pesatura != "no", "SI", "NO"), 
    
    Valorizzato = ifelse(Valorizzato != "no", "SI", "NO"),
    
    AS = case_when(
      AreaStrategica == "GARANTIRE L'ATTIVITA' ISTITUZIONALE IN MODO EFFICACE ED APPROPRIATO (AS1)" ~ paste0("AS1 - GARANTIRE L'ATTIVITÀ ISTITUZIONALE IN MODO EFFICACE ED APPROPRIATO"),
      AreaStrategica == " POTENZIARE LE ATTIVITA' RELATIVE ALLA RICERCA NAZIONALE ED INTERNAZIONALE  (AS2)" ~ paste0("AS2 - POTENZIARE LE ATTIVITÀ RELATIVE ALLA RICERCA NAZIONALE ED INTERNAZIONALE"),
      AreaStrategica == "GARANTIRE L’EFFICIENZA DEI SISTEMI GESTIONALI ATTRAVERSO LA SOSTENIBILITÀ DEI PROCESSI IN UN’OTTICA MULTIDIMENSIONALE  (AS3)" ~ paste0("AS3 - GARANTIRE L'EFFICIENZA DEI SISTEMI GESTIONALI ATTRAVERSO LA SOSTENIBILITÀ DEI PROCESSI IN UN'OTTICA MULTIDIMENSIONALE"),
      AreaStrategica == "PROMUOVERE LA FORMAZIONE CONTINUA, IL DIALOGO CON GLI STAKEHOLDER E  LA VALORIZZAZIONE DELLE RISORSE UMANE ( AS4)" ~ paste0("AS4 - PROMUOVERE LA FORMAZIONE CONTINUA, IL DIALOGO CON GLI STAKEHOLDER E LA VALORIZZAZIONE DELLE RISORSE UMANE")
    ),
    
    TGT = case_when(
      Tipo_Risposta == "Percentuale" & Operatore == ">=" ~ paste0(intToUtf8(8805), Target, "%"),
      Tipo_Risposta == "Percentuale" & Operatore == "<=" ~ paste0(intToUtf8(8804), Target, "%"),
      Tipo_Risposta == "Percentuale" & Operatore == "=" ~ paste0(intToUtf8(61), Target, "%"),
      Tipo_Risposta == "Valore" & Operatore == ">=" ~ paste0(intToUtf8(8805), Target),
      Tipo_Risposta == "Valore" & Operatore == "<=" ~ paste0(intToUtf8(8804), Target),
      Tipo_Risposta == "Valore" & Operatore == "=" ~ paste0(intToUtf8(61), Target)
    ),
    
    OG = trimws(
      gsub("\\s+", " ",
           paste0(
             # "<b>",
             sub("\\).*", "", sub(".*\\(", "", ObiettivoGenerale)),
             # "</b>",
             " - ",
             gsub("\\s*\\([^\\)]+\\)","", ObiettivoGenerale))
      )),
    
    ObiettivoOperativo = trimws(
      gsub("\\s+", " ", ObiettivoOperativo)),
    
    Indicatore = trimws(
      gsub("\\s+", " ", Indicatore)),
    
    CriteriAttivita = case_when(
      is.na(CriteriAttivita) ~ "-",
      CriteriAttivita == "\r\ncalcolato per DIPARTIMENTO" ~ paste0("Calcolato per DIPARTIMENTO"),
      TRUE ~ str_replace_all(CriteriAttivita, "\r\n", "<br>"))
  )


#DIPARTIMENTO----
obiett <- df %>% 
  # select(Dipartimento, Struttura, OG,
  #        FTED, AttivitaRoutinaria_FTED, AttivitaValorizzataPerproduzioni_FTED, CentroReferenza_FTED,
  #        FTEC, AttivitaRoutinaria_FTEC, AttivitaValorizzataPerproduzioni_FTEC, CentroReferenza_FTEC) %>% 
  # filter(Dipartimento == "DIPARTIMENTO AREA TERRITORIALE LOMBARDIA") %>%
  group_by(Dipartimento) %>% 
  summarise(
    FTED_obiett = sum(FTED, na.rm = TRUE), 
    FTEC_obiett = sum(FTEC, na.rm = TRUE)
    )

attiv <- df %>% 
  # select(Dipartimento, Struttura,
  #        AttivitaRoutinaria_FTED, AttivitaValorizzataPerproduzioni_FTED, CentroReferenza_FTED,
  #        AttivitaRoutinaria_FTEC, AttivitaValorizzataPerproduzioni_FTEC, CentroReferenza_FTEC) %>% 
  # filter(Dipartimento == "DIPARTIMENTO AREA TERRITORIALE LOMBARDIA") %>%
  distinct(Struttura, .keep_all = T) %>% 
  group_by(Dipartimento) %>% 
  summarise(
    FTED_routine = sum(AttivitaRoutinaria_FTED, na.rm = TRUE), 
    FTEC_routine = sum(AttivitaRoutinaria_FTEC, na.rm = TRUE),
    FTED_valor = sum(AttivitaValorizzataPerproduzioni_FTED, na.rm = TRUE), 
    FTEC_valor = sum(AttivitaValorizzataPerproduzioni_FTEC, na.rm = TRUE),
    FTED_centriref = sum(CentroReferenza_FTED, na.rm = TRUE), 
    FTEC_centriref = sum(CentroReferenza_FTEC, na.rm = TRUE)
  )

tab <- obiett %>% 
  left_join(attiv, by = c("Dipartimento")) %>% 
  pivot_longer(cols = 2:9, names_to = "destinazione", values_to = "FTE") %>%
  mutate(
    tipo = case_when(
      str_detect(destinazione, "rout") ~ paste0("routinaria"),
      str_detect(destinazione, "valor") ~ paste0("valorizzata"),
      str_detect(destinazione, "centri") ~ paste0("centriref"),
      str_detect(destinazione, "obiet") ~ paste0("obiettivi")),
    area = case_when(
      str_detect(destinazione, "FTED") ~ paste0("dirigenza"),
      str_detect(destinazione, "FTEC") ~ paste0("comparto")
    )
  ) %>% 
  group_by(Dipartimento) %>% 
  mutate(FTEtot = sum(FTE)) %>% #FTE totali per dipartimento
  
  ungroup() %>% 
  arrange(Dipartimento, area) %>% 
  group_by(Dipartimento, area) %>% 
  mutate(FTEtot_area = sum(FTE), #FTE comparto/dirigenza per dipartimento
         # FTEperc = round(FTE/FTEtot*100, 2)) %>% 
         FTEperc = FTE/FTEtot*100,
         FTEperc_area = FTE/FTEtot_area*100) %>%
  ungroup()


##FTE DIPARTIMENTO % sul totale----
# tab %>%
#   select(Dipartimento, tipo, area, FTEperc) %>% 
#   mutate(tipo = factor(tipo, levels = c("obiettivi", "valorizzata", "routinaria", "centriref"))) %>% 
#   arrange(tipo, area) %>%
#   pivot_wider(names_from = c("tipo", "area"),
#               values_from = c("FTEperc")) %>% 
#   mutate(
#     obiettivi_tot = obiettivi_dirigenza + obiettivi_comparto, 
#     valorizzata_tot = valorizzata_dirigenza + valorizzata_comparto, 
#     routinaria_tot = routinaria_dirigenza + routinaria_comparto,
#     centriref_tot = centriref_dirigenza + centriref_comparto
#   ) %>% 
#   gt(
#     rowname_col = "Dipartimento"
#     # groupname_col = "Dipartimento"
#   ) %>% 
#   tab_header(
#     title = md("**% FTE programmati per le attività**")
#   ) %>%
#   tab_spanner(
#     label = html("Attività<br>Obiettivi"),
#     columns = c(obiettivi_dirigenza, obiettivi_comparto, obiettivi_tot)
#   ) %>%
#   tab_spanner(
#     label = html("Attività<br>Valorizzata"),
#     columns = c(valorizzata_dirigenza, valorizzata_comparto, valorizzata_tot)
#   ) %>%
#   tab_spanner(
#     label = html("Attività<br>Routinaria"),
#     columns = c(routinaria_dirigenza, routinaria_comparto, routinaria_tot)
#   ) %>%
#   tab_spanner(
#     label = html("Attività<br>Centri di Referenza"),
#     columns = c(centriref_dirigenza, centriref_comparto, centriref_tot)
#   ) %>%
#   cols_label(
#     obiettivi_comparto = "C",
#     obiettivi_dirigenza = "D",
#     routinaria_comparto = "C",
#     routinaria_dirigenza = "D",
#     valorizzata_comparto = "C",
#     valorizzata_dirigenza = "D",
#     centriref_comparto = "C",
#     centriref_dirigenza = "D",
#     obiettivi_tot = "T",
#     valorizzata_tot = "T",
#     routinaria_tot = "T",
#     centriref_tot = "T"
#   ) %>%
#   fmt_number(
#     columns = c(-Dipartimento),
#     decimals = 1,
#     use_seps = FALSE,
#     drop_trailing_zeros = T
#   ) %>% 
#   cols_width(
#     obiettivi_comparto ~ pct(6),
#     obiettivi_dirigenza ~ pct(6),
#     valorizzata_comparto ~ pct(6),
#     valorizzata_dirigenza ~ pct(6),
#     routinaria_comparto ~ pct(6),
#     routinaria_dirigenza ~ pct(6),
#     centriref_comparto ~ pct(6),
#     centriref_dirigenza ~ pct(6),
#     obiettivi_tot ~ pct(6),
#     valorizzata_tot ~ pct(6),
#     routinaria_tot ~ pct(6),
#     centriref_tot ~ pct(6)
#     
#   ) %>% 
#   cols_align(
#     align = "center",
#     columns = -c(Dipartimento)) %>%
#   opt_css(
#     css = "
#     .gt_col_heading {
#       vertical-align: middle !important;
#     }
#     "
#   ) %>% 
#   tab_style(
#     style = list(cell_fill(color = "#D3D3D3"),
#                  cell_text(weight = "bold")),
#     locations = cells_body(columns = ends_with("tot"))
#   )


##FTE DIP TOT----
tab %>%
  select(Dipartimento, tipo, area, FTEperc) %>% 
  mutate(tipo = factor(tipo, levels = c("valorizzata", "routinaria", "centriref", "obiettivi"))) %>% 
  arrange(tipo, area) %>%
  pivot_wider(names_from = c("tipo", "area"),
              values_from = c("FTEperc")) %>% 
  mutate(
    valorizzata_tot = valorizzata_dirigenza + valorizzata_comparto, 
    routinaria_tot = routinaria_dirigenza + routinaria_comparto,
    centriref_tot = centriref_dirigenza + centriref_comparto,
    obiettivi_tot = obiettivi_dirigenza + obiettivi_comparto
  ) %>% 
  gt(
    rowname_col = "Dipartimento"
    # groupname_col = "Dipartimento"
  ) %>% 
  tab_header(
    title = md("**% FTE programmati per le diverse attività - (Dirigenza + Comparto)**")
  ) %>%
  # tab_spanner(
  #   label = html("Attività<br>Valorizzata"),
  #   columns = c(valorizzata_tot)
  # ) %>%
  # tab_spanner(
  #   label = html("Attività<br>Routinaria"),
  #   columns = c(routinaria_tot)
  # ) %>%
  # tab_spanner(
  #   label = html("Attività<br>Centri Referenza"),
  #   columns = c(centriref_tot)
  # ) %>%
  # tab_spanner(
  #   label = html("Attività<br>Obiettivi"),
  #   columns = c(obiettivi_tot)
  # ) %>% 
  
  cols_label(
    # obiettivi_comparto = "C",
    # obiettivi_dirigenza = "D",
    # routinaria_comparto = "C",
    # routinaria_dirigenza = "D",
    # valorizzata_comparto = "C",
    # valorizzata_dirigenza = "D",
    # centriref_comparto = "C",
    # centriref_dirigenza = "D",

    valorizzata_tot = html("Attività<br>Valorizzata"),
    routinaria_tot = html("Attività<br>Routinaria"),
    centriref_tot =  html("Attività<br>Centri Referenza"),
    obiettivi_tot =  html("Attività<br>Obiettivi")
  ) %>%
  fmt_number(
    columns = c(-Dipartimento),
    decimals = 1,
    use_seps = FALSE,
    drop_trailing_zeros = T
  ) %>% 
  cols_hide(c(valorizzata_dirigenza, valorizzata_comparto, 
              routinaria_dirigenza, routinaria_comparto,
              centriref_dirigenza, centriref_comparto,
              obiettivi_dirigenza, obiettivi_comparto)) %>% 
  cols_width(
    # obiettivi_comparto ~ pct(6),
    # obiettivi_dirigenza ~ pct(6),
    # valorizzata_comparto ~ pct(6),
    # valorizzata_dirigenza ~ pct(6),
    # routinaria_comparto ~ pct(6),
    # routinaria_dirigenza ~ pct(6),
    # centriref_comparto ~ pct(6),
    # centriref_dirigenza ~ pct(6),
    Dipartimento ~ pct(20),
    obiettivi_tot ~ pct(10),
    valorizzata_tot ~ pct(10),
    routinaria_tot ~ pct(10),
    centriref_tot ~ pct(10)
    
  ) %>% 
  cols_align(
    align = "center",
    columns = -c(Dipartimento)) %>%
  opt_css(
    css = "
    .gt_col_heading {
      vertical-align: middle !important;
    }
    "
  ) 
  # tab_style(
  #   style = list(cell_fill(color = "#D3D3D3"),
  #                cell_text(weight = "bold")),
  #   locations = cells_body(columns = ends_with("tot"))
  # )

##FTE DIP DIRIG----
tab %>%
  select(Dipartimento, tipo, area, FTEperc_area) %>% 
  mutate(tipo = factor(tipo, levels = c("valorizzata", "routinaria", "centriref", "obiettivi"))) %>% 
  arrange(Dipartimento, tipo) %>%
  filter(area == "dirigenza") %>% 
  
  pivot_wider(names_from = c("tipo", "area"),
              values_from = c("FTEperc_area")) %>% 
  gt(
    rowname_col = "Dipartimento"
    # groupname_col = "Dipartimento"
  ) %>% 
  tab_header(
    title = md("**% FTE programmati per le diverse attività - DIRIGENZA**")
  ) %>%
  # tab_spanner(
  #   label = html("Attività<br>Valorizzata"),
  #   columns = c(valorizzata_dirigenza)
  # ) %>%
  # tab_spanner(
  #   label = html("Attività<br>Routinaria"),
  #   columns = c(routinaria_dirigenza)
  # ) %>%
  # tab_spanner(
  #   label = html("Attività<br>Centri Referenza"),
  #   columns = c(centriref_dirigenza)
  # ) %>%
  # tab_spanner(
  #   label = html("Attività<br>Obiettivi"),
  #   columns = c(obiettivi_dirigenza)
  # ) %>%
  # 
  cols_label(
    # obiettivi_comparto = "C",
    # obiettivi_dirigenza = "D",
    # routinaria_comparto = "C",
    # routinaria_dirigenza = "D",
    # valorizzata_comparto = "C",
    # valorizzata_dirigenza = "D",
    # centriref_comparto = "C",
    # centriref_dirigenza = "D",

    valorizzata_dirigenza = html("Attività<br>Valorizzata"),
    routinaria_dirigenza =  html("Attività<br>Routinaria"),
    centriref_dirigenza = html("Attività<br>Centri Referenza"),
    obiettivi_dirigenza = html("Attività<br>Obiettivi"),
  ) %>%
  fmt_number(
    columns = c(-Dipartimento),
    decimals = 1,
    use_seps = FALSE,
    drop_trailing_zeros = T
  ) %>% 
  cols_width(
    # obiettivi_comparto ~ pct(6),
    # obiettivi_dirigenza ~ pct(6),
    # valorizzata_comparto ~ pct(6),
    # valorizzata_dirigenza ~ pct(6),
    # routinaria_comparto ~ pct(6),
    # routinaria_dirigenza ~ pct(6),
    # centriref_comparto ~ pct(6),
    # centriref_dirigenza ~ pct(6),
    Dipartimento ~ pct(20),
    obiettivi_dirigenza ~ pct(10),
    valorizzata_dirigenza ~ pct(10),
    routinaria_dirigenza ~ pct(10),
    centriref_dirigenza ~ pct(10)
    
  ) %>% 
  cols_align(
    align = "center",
    columns = -c(Dipartimento)) %>%
  opt_css(
    css = "
    .gt_col_heading {
      vertical-align: middle !important;
    }
    "
  ) 
# tab_style(
#   style = list(cell_fill(color = "#D3D3D3"),
#                cell_text(weight = "bold")),
#   locations = cells_body(columns = ends_with("tot"))
# )


##FTE DIP COMPAR----
tab %>%
  select(Dipartimento, tipo, area, FTEperc_area) %>% 
  mutate(tipo = factor(tipo, levels = c("valorizzata", "routinaria", "centriref", "obiettivi"))) %>% 
  arrange(Dipartimento, tipo) %>%
  filter(area == "comparto") %>% 
  
  pivot_wider(names_from = c("tipo", "area"),
              values_from = c("FTEperc_area")) %>% 
  gt(
    rowname_col = "Dipartimento"
    # groupname_col = "Dipartimento"
  ) %>% 
  tab_header(
    title = md("**% FTE programmati per le diverse attività - COMPARTO**")
  ) %>%
  # tab_spanner(
  #   label = html("Attività<br>Valorizzata"),
  #   columns = c(valorizzata_comparto)
  # ) %>%
  # tab_spanner(
  #   label = html("Attività<br>Routinaria"),
  #   columns = c(routinaria_comparto)
  # ) %>%
  # tab_spanner(
  #   label = html("Attività<br>Centri Referenza"),
  #   columns = c(centriref_comparto)
  # ) %>%
  # tab_spanner(
  #   label = html("Attività<br>Obiettivi"),
  #   columns = c(obiettivi_comparto)
  # ) %>%
  
  cols_label(
    # obiettivi_comparto = "C",
    # obiettivi_dirigenza = "D",
    # routinaria_comparto = "C",
    # routinaria_dirigenza = "D",
    # valorizzata_comparto = "C",
    # valorizzata_dirigenza = "D",
    # centriref_comparto = "C",
    # centriref_dirigenza = "D",

    valorizzata_comparto = html("Attività<br>Valorizzata"),
    routinaria_comparto = html("Attività<br>Routinaria"),
    centriref_comparto = html("Attività<br>Centri Referenza"),
    obiettivi_comparto = html("Attività<br>Obiettivi")
  ) %>%
  fmt_number(
    columns = c(-Dipartimento),
    decimals = 1,
    use_seps = FALSE,
    drop_trailing_zeros = T
  ) %>% 
  cols_width(
    # obiettivi_comparto ~ pct(6),
    # obiettivi_dirigenza ~ pct(6),
    # valorizzata_comparto ~ pct(6),
    # valorizzata_dirigenza ~ pct(6),
    # routinaria_comparto ~ pct(6),
    # routinaria_dirigenza ~ pct(6),
    # centriref_comparto ~ pct(6),
    # centriref_dirigenza ~ pct(6),
    Dipartimento ~ pct(20),
    obiettivi_comparto ~ pct(10),
    valorizzata_comparto ~ pct(10),
    routinaria_comparto ~ pct(10),
    centriref_comparto ~ pct(10)
    
  ) %>% 
  cols_align(
    align = "center",
    columns = -c(Dipartimento)) %>%
  opt_css(
    css = "
    .gt_col_heading {
      vertical-align: middle !important;
    }
    "
  ) 

# tab_style(
#   style = list(cell_fill(color = "#D3D3D3"),
#                cell_text(weight = "bold")),
#   locations = cells_body(columns = ends_with("tot"))
# )



# #FTE REPARTO----

obiett <- df %>% 
  # select(Dipartimento, Struttura, OG,
  #        FTED, AttivitaRoutinaria_FTED, AttivitaValorizzataPerproduzioni_FTED, CentroReferenza_FTED,
  #        FTEC, AttivitaRoutinaria_FTEC, AttivitaValorizzataPerproduzioni_FTEC, CentroReferenza_FTEC) %>% 
  # filter(Dipartimento == "DIPARTIMENTO AREA TERRITORIALE LOMBARDIA") %>%
  group_by(Dipartimento, Struttura) %>% 
  summarise(
    FTED_obiett = sum(FTED, na.rm = TRUE), 
    FTEC_obiett = sum(FTEC, na.rm = TRUE)
  )

attiv <- df %>% 
  # select(Dipartimento, Struttura,
  #        AttivitaRoutinaria_FTED, AttivitaValorizzataPerproduzioni_FTED, CentroReferenza_FTED,
  #        AttivitaRoutinaria_FTEC, AttivitaValorizzataPerproduzioni_FTEC, CentroReferenza_FTEC) %>% 
  # filter(Dipartimento == "DIPARTIMENTO AREA TERRITORIALE LOMBARDIA") %>%
  distinct(Struttura, .keep_all = T) %>% 
  group_by(Dipartimento, Struttura) %>% 
  summarise(
    FTED_routine = sum(AttivitaRoutinaria_FTED, na.rm = TRUE), 
    FTEC_routine = sum(AttivitaRoutinaria_FTEC, na.rm = TRUE),
    FTED_valor = sum(AttivitaValorizzataPerproduzioni_FTED, na.rm = TRUE), 
    FTEC_valor = sum(AttivitaValorizzataPerproduzioni_FTEC, na.rm = TRUE),
    FTED_centriref = sum(CentroReferenza_FTED, na.rm = TRUE), 
    FTEC_centriref = sum(CentroReferenza_FTEC, na.rm = TRUE)
  )

tab <- obiett %>% 
  left_join(attiv, by = c("Dipartimento", "Struttura")) %>% 
  pivot_longer(cols = 3:10, names_to = "destinazione", values_to = "FTE") %>%  
  mutate(
    tipo = case_when(
      str_detect(destinazione, "rout") ~ paste0("routinaria"),
      str_detect(destinazione, "valor") ~ paste0("valorizzata"),
      str_detect(destinazione, "centri") ~ paste0("centriref"),
      str_detect(destinazione, "obiet") ~ paste0("obiettivi")),
    area = case_when(
      str_detect(destinazione, "FTED") ~ paste0("dirigenza"),
      str_detect(destinazione, "FTEC") ~ paste0("comparto")
    )
  ) %>% 
  group_by(Dipartimento, Struttura) %>% 
  mutate(FTEtot = sum(FTE)) %>% #FTE totali per dipartimento
  
  ungroup() %>% 
  arrange(Dipartimento,Struttura, area) %>% 
  group_by(Dipartimento, Struttura,  area) %>% 
  mutate(FTEtot_area = sum(FTE), #FTE comparto/dirigenza per dipartimento
         # FTEperc = round(FTE/FTEtot*100, 2)) %>% 
         FTEperc = FTE/FTEtot*100,
         FTEperc_area = FTE/FTEtot_area*100)
  



#FTE tot Strutture----

tab %>% ungroup() %>% 
  select(Dipartimento,Struttura, tipo, area, FTE) %>% 
  mutate(tipo = factor(tipo, levels = c("valorizzata", "routinaria", "centriref", "obiettivi"))) %>% 
  arrange(tipo, area) %>%
  pivot_wider(names_from = c("tipo", "area"),
              values_from = c("FTEperc")) %>%  
  mutate(
    valorizzata_tot = valorizzata_dirigenza + valorizzata_comparto, 
    routinaria_tot = routinaria_dirigenza + routinaria_comparto,
    centriref_tot = centriref_dirigenza + centriref_comparto,
    obiettivi_tot = obiettivi_dirigenza + obiettivi_comparto
  ) %>% 
  gt(
    rowname_col = "Struttura"
    # groupname_col = "Dipartimento"
  ) %>% 
  tab_header(
    title = md("**% FTE programmati per le diverse attività - (Dirigenza + Comparto)**")
  ) %>%
  # tab_spanner(
  #   label = html("Attività<br>Valorizzata"),
  #   columns = c(valorizzata_tot)
  # ) %>%
  # tab_spanner(
  #   label = html("Attività<br>Routinaria"),
  #   columns = c(routinaria_tot)
  # ) %>%
  # tab_spanner(
  #   label = html("Attività<br>Centri Referenza"),
  #   columns = c(centriref_tot)
# ) %>%
# tab_spanner(
#   label = html("Attività<br>Obiettivi"),
#   columns = c(obiettivi_tot)
# ) %>% 

cols_label(
  # obiettivi_comparto = "C",
  # obiettivi_dirigenza = "D",
  # routinaria_comparto = "C",
  # routinaria_dirigenza = "D",
  # valorizzata_comparto = "C",
  # valorizzata_dirigenza = "D",
  # centriref_comparto = "C",
  # centriref_dirigenza = "D",
  
  valorizzata_tot = html("Attività<br>Valorizzata"),
  routinaria_tot = html("Attività<br>Routinaria"),
  centriref_tot =  html("Attività<br>Centri Referenza"),
  obiettivi_tot =  html("Attività<br>Obiettivi")
) %>%
  fmt_number(
    columns = c(-Struttura),
    decimals = 1,
    use_seps = FALSE,
    drop_trailing_zeros = T
  ) %>% 
  cols_hide(c(valorizzata_dirigenza, valorizzata_comparto, 
              routinaria_dirigenza, routinaria_comparto,
              centriref_dirigenza, centriref_comparto,
              obiettivi_dirigenza, obiettivi_comparto)) %>% 
  cols_width(
    # obiettivi_comparto ~ pct(6),
    # obiettivi_dirigenza ~ pct(6),
    # valorizzata_comparto ~ pct(6),
    # valorizzata_dirigenza ~ pct(6),
    # routinaria_comparto ~ pct(6),
    # routinaria_dirigenza ~ pct(6),
    # centriref_comparto ~ pct(6),
    # centriref_dirigenza ~ pct(6),
    Struttura ~ pct(20),
    obiettivi_tot ~ pct(10),
    valorizzata_tot ~ pct(10),
    routinaria_tot ~ pct(10),
    centriref_tot ~ pct(10)
    
  ) %>% 
  cols_align(
    align = "center",
    columns = -c(Struttura)) %>%
  opt_css(
    css = "
    .gt_col_heading {
      vertical-align: middle !important;
    }
    "
  ) 


library(openxlsx)
##FTE Struttura DIRIG----
tab %>% ungroup() %>% 
  select(Dipartimento, Struttura, tipo, area, FTEperc_area) %>% 
  mutate(tipo = factor(tipo, levels = c("valorizzata", "routinaria", "centriref", "obiettivi"))) %>% 
  arrange(Struttura, tipo) %>%
  filter(area == "dirigenza") %>% 
  
  pivot_wider(names_from = c("tipo", "area"),
              values_from = c("FTEperc_area")) %>% write.xlsx(file = "fte23dir.xlsx")
  gt(
    rowname_col = "Struttura"
    # groupname_col = "Dipartimento"
  ) %>% 
  tab_header(
    title = md("**% FTE programmati per le diverse attività - DIRIGENZA**")
  ) %>%
  # tab_spanner(
  #   label = html("Attività<br>Valorizzata"),
  #   columns = c(valorizzata_dirigenza)
  # ) %>%
  # tab_spanner(
  #   label = html("Attività<br>Routinaria"),
  #   columns = c(routinaria_dirigenza)
  # ) %>%
  # tab_spanner(
  #   label = html("Attività<br>Centri Referenza"),
  #   columns = c(centriref_dirigenza)
# ) %>%
# tab_spanner(
#   label = html("Attività<br>Obiettivi"),
#   columns = c(obiettivi_dirigenza)
# ) %>%
# 
cols_label(
  # obiettivi_comparto = "C",
  # obiettivi_dirigenza = "D",
  # routinaria_comparto = "C",
  # routinaria_dirigenza = "D",
  # valorizzata_comparto = "C",
  # valorizzata_dirigenza = "D",
  # centriref_comparto = "C",
  # centriref_dirigenza = "D",
  
  valorizzata_dirigenza = html("Attività<br>Valorizzata"),
  routinaria_dirigenza =  html("Attività<br>Routinaria"),
  centriref_dirigenza = html("Attività<br>Centri Referenza"),
  obiettivi_dirigenza = html("Attività<br>Obiettivi"),
) %>%
  fmt_number(
    columns = c(-Struttura),
    decimals = 1,
    use_seps = FALSE,
    drop_trailing_zeros = T
  ) %>% 
  cols_width(
    # obiettivi_comparto ~ pct(6),
    # obiettivi_dirigenza ~ pct(6),
    # valorizzata_comparto ~ pct(6),
    # valorizzata_dirigenza ~ pct(6),
    # routinaria_comparto ~ pct(6),
    # routinaria_dirigenza ~ pct(6),
    # centriref_comparto ~ pct(6),
    # centriref_dirigenza ~ pct(6),
    Struttura ~ pct(20),
    obiettivi_dirigenza ~ pct(10),
    valorizzata_dirigenza ~ pct(10),
    routinaria_dirigenza ~ pct(10),
    centriref_dirigenza ~ pct(10)
    
  ) %>% 
  cols_align(
    align = "center",
    columns = -c(Struttura)) %>%
  opt_css(
    css = "
    .gt_col_heading {
      vertical-align: middle !important;
    }
    "
  ) 
# tab_style(
#   style = list(cell_fill(color = "#D3D3D3"),
#                cell_text(weight = "bold")),
#   locations = cells_body(columns = ends_with("tot"))
# )



  ##FTE Struttura Comparto----
  tab %>% ungroup() %>% 
    select(Dipartimento, Struttura, tipo, area, FTEperc_area) %>% 
    mutate(tipo = factor(tipo, levels = c("valorizzata", "routinaria", "centriref", "obiettivi"))) %>% 
    arrange(Struttura, tipo) %>%
    filter(area == "comparto") %>% 
    
    pivot_wider(names_from = c("tipo", "area"),
                values_from = c("FTEperc_area")) %>% write.xlsx(file = "fte23comp.xlsx")
  gt(
    rowname_col = "Struttura"
    # groupname_col = "Dipartimento"
  ) %>% 
    tab_header(
      title = md("**% FTE programmati per le diverse attività - DIRIGENZA**")
    ) %>%
    # tab_spanner(
    #   label = html("Attività<br>Valorizzata"),
    #   columns = c(valorizzata_dirigenza)
    # ) %>%
    # tab_spanner(
    #   label = html("Attività<br>Routinaria"),
    #   columns = c(routinaria_dirigenza)
    # ) %>%
    # tab_spanner(
    #   label = html("Attività<br>Centri Referenza"),
    #   columns = c(centriref_dirigenza)
  # ) %>%
  # tab_spanner(
  #   label = html("Attività<br>Obiettivi"),
  #   columns = c(obiettivi_dirigenza)
  # ) %>%
  # 
  cols_label(
    # obiettivi_comparto = "C",
    # obiettivi_dirigenza = "D",
    # routinaria_comparto = "C",
    # routinaria_dirigenza = "D",
    # valorizzata_comparto = "C",
    # valorizzata_dirigenza = "D",
    # centriref_comparto = "C",
    # centriref_dirigenza = "D",
    
    valorizzata_dirigenza = html("Attività<br>Valorizzata"),
    routinaria_dirigenza =  html("Attività<br>Routinaria"),
    centriref_dirigenza = html("Attività<br>Centri Referenza"),
    obiettivi_dirigenza = html("Attività<br>Obiettivi"),
  ) %>%
    fmt_number(
      columns = c(-Struttura),
      decimals = 1,
      use_seps = FALSE,
      drop_trailing_zeros = T
    ) %>% 
    cols_width(
      # obiettivi_comparto ~ pct(6),
      # obiettivi_dirigenza ~ pct(6),
      # valorizzata_comparto ~ pct(6),
      # valorizzata_dirigenza ~ pct(6),
      # routinaria_comparto ~ pct(6),
      # routinaria_dirigenza ~ pct(6),
      # centriref_comparto ~ pct(6),
      # centriref_dirigenza ~ pct(6),
      Struttura ~ pct(20),
      obiettivi_dirigenza ~ pct(10),
      valorizzata_dirigenza ~ pct(10),
      routinaria_dirigenza ~ pct(10),
      centriref_dirigenza ~ pct(10)
      
    ) %>% 
    cols_align(
      align = "center",
      columns = -c(Struttura)) %>%
    opt_css(
      css = "
    .gt_col_heading {
      vertical-align: middle !important;
    }
    "
    ) 
  # tab_style(
  #   style = list(cell_fill(color = "#D3D3D3"),
  #                cell_text(weight = "bold")),
  #   locations = cells_body(columns = ends_with("tot"))
  # )
  





### Distribuzione dei FTE

  obiett <- df %>% 
    # select(Dipartimento, Struttura, OG,
    #        FTED, AttivitaRoutinaria_FTED, AttivitaValorizzataPerproduzioni_FTED, CentroReferenza_FTED,
    #        FTEC, AttivitaRoutinaria_FTEC, AttivitaValorizzataPerproduzioni_FTEC, CentroReferenza_FTEC) %>% 
    # filter(Dipartimento == "DIPARTIMENTO AREA TERRITORIALE LOMBARDIA") %>%
    group_by(Dipartimento, Struttura) %>% 
    summarise(
      FTED_obiett = sum(FTED, na.rm = TRUE), 
      FTEC_obiett = sum(FTEC, na.rm = TRUE)
    )
  
  attiv <- df %>% 
    # select(Dipartimento, Struttura,
    #        AttivitaRoutinaria_FTED, AttivitaValorizzataPerproduzioni_FTED, CentroReferenza_FTED,
    #        AttivitaRoutinaria_FTEC, AttivitaValorizzataPerproduzioni_FTEC, CentroReferenza_FTEC) %>% 
    # filter(Dipartimento == "DIPARTIMENTO AREA TERRITORIALE LOMBARDIA") %>%
    distinct(Struttura, .keep_all = T) %>% 
    group_by(Dipartimento, Struttura) %>% 
    summarise(
      FTED_routine = sum(AttivitaRoutinaria_FTED, na.rm = TRUE), 
      FTEC_routine = sum(AttivitaRoutinaria_FTEC, na.rm = TRUE),
      FTED_valor = sum(AttivitaValorizzataPerproduzioni_FTED, na.rm = TRUE), 
      FTEC_valor = sum(AttivitaValorizzataPerproduzioni_FTEC, na.rm = TRUE),
      FTED_centriref = sum(CentroReferenza_FTED, na.rm = TRUE), 
      FTEC_centriref = sum(CentroReferenza_FTEC, na.rm = TRUE)
    )
  
  tab <- obiett %>% 
    left_join(attiv, by = c("Dipartimento", "Struttura")) %>% 
    pivot_longer(cols = 3:10, names_to = "destinazione", values_to = "FTE") %>%  
    mutate(
      tipo = case_when(
        str_detect(destinazione, "rout") ~ paste0("routinaria"),
        str_detect(destinazione, "valor") ~ paste0("valorizzata"),
        str_detect(destinazione, "centri") ~ paste0("centriref"),
        str_detect(destinazione, "obiet") ~ paste0("obiettivi")),
      area = case_when(
        str_detect(destinazione, "FTED") ~ paste0("dirigenza"),
        str_detect(destinazione, "FTEC") ~ paste0("comparto")
      )
    ) %>% 
    group_by(Dipartimento, Struttura) %>% 
    mutate(FTEtot = sum(FTE)) %>% #FTE totali per dipartimento
    
    ungroup() %>% 
    arrange(Dipartimento,Struttura, area) #%>% 
    #group_by(Dipartimento, Struttura,  area) %>% 
    # mutate(FTEtot_area = sum(FTE), #FTE comparto/dirigenza per dipartimento
    #        # FTEperc = round(FTE/FTEtot*100, 2)) %>% 
    #        FTEperc = FTE/FTEtot*100,
    #        FTEperc_area = FTE/FTEtot_area*100)
  
  
  
  
library(tidytext)
  
  tab %>% ungroup() %>% 
    select(Dipartimento, tipo, area, FTE) %>% 
    mutate(tipo = factor(tipo, levels = c("valorizzata", "routinaria", "centriref", "obiettivi")), 
           tipo = recode(tipo, 
                         "valorizzata" = "Attività Valorizzata", 
                         "routinaria" = "Attività Routinaria", 
                         "centriref" = "Attvità per CdR", 
                         "obiettivi" = "Attività per obiettivi")) %>% 
    
    
    
    group_by(Dipartimento, tipo) %>% 
    summarise(FTE = round(sum(FTE), 2)) %>%  
    # arrange(tipo, area) %>%
    # pivot_wider(names_from = c("tipo", "area"),
    #             values_from = c("FTE")) %>% 
    # filter(tipo %in% c("valorizzata", "obiettivi")) %>%  
    # pivot_wider(names_from = "tipo", values_from = "FTE") %>% 
    # 
   # mutate(Dipartimento = factor(Dipartimento, unique(Dipartimento))) %>% 
    
    
      ggplot()+
      
      aes(x = reorder_within(Dipartimento,  FTE, tipo),
          y =  FTE, 
          label = FTE)+
     geom_bar(stat = "identity", fill="steelblue", width = 0.5)+ geom_label(size = 3)+
    coord_flip()+
    facet_wrap(~tipo, scales = "free")+
    scale_x_discrete(labels = function(x) gsub("__.+$", "", x))+
    labs(title = "FTE allocati alle diverse attività per Dipartimento", x= "", y = "FTE")+
    theme_classic()+
    theme(axis.text.y = element_text(size = 8), 
          axis.ticks = element_blank(), 
          axis.line = element_blank(), 
          axis.text.x = element_text(size=8),
          title = element_text(size = 15))
    
    
  
  
  
  
  
  
  
    # gt(
    #   rowname_col = "Struttura"
    #   # groupname_col = "Dipartimento"
    # ) %>% 
    # tab_header(
    #   title = md("**% FTE programmati per le diverse attività - (Dirigenza + Comparto)**")
    # ) %>%
    # tab_spanner(
    #   label = html("Attività<br>Valorizzata"),
    #   columns = c(valorizzata_tot)
    # ) %>%
    # tab_spanner(
    #   label = html("Attività<br>Routinaria"),
    #   columns = c(routinaria_tot)
    # ) %>%
    # tab_spanner(
    #   label = html("Attività<br>Centri Referenza"),
    #   columns = c(centriref_tot)
  # ) %>%
  # tab_spanner(
  #   label = html("Attività<br>Obiettivi"),
  #   columns = c(obiettivi_tot)
  # ) %>% 
  
  # cols_label(
  #   # obiettivi_comparto = "C",
  #   # obiettivi_dirigenza = "D",
  #   # routinaria_comparto = "C",
  #   # routinaria_dirigenza = "D",
  #   # valorizzata_comparto = "C",
  #   # valorizzata_dirigenza = "D",
  #   # centriref_comparto = "C",
  #   # centriref_dirigenza = "D",
  #   
  #   valorizzata_tot = html("Attività<br>Valorizzata"),
  #   routinaria_tot = html("Attività<br>Routinaria"),
  #   centriref_tot =  html("Attività<br>Centri Referenza"),
  #   obiettivi_tot =  html("Attività<br>Obiettivi")
  # ) %>%
  #   fmt_number(
  #     columns = c(-Struttura),
  #     decimals = 1,
  #     use_seps = FALSE,
  #     drop_trailing_zeros = T
  #   ) %>% 
  #   cols_hide(c(valorizzata_dirigenza, valorizzata_comparto, 
  #               routinaria_dirigenza, routinaria_comparto,
  #               centriref_dirigenza, centriref_comparto,
  #               obiettivi_dirigenza, obiettivi_comparto)) %>% 
  #   cols_width(
  #     # obiettivi_comparto ~ pct(6),
  #     # obiettivi_dirigenza ~ pct(6),
  #     # valorizzata_comparto ~ pct(6),
  #     # valorizzata_dirigenza ~ pct(6),
  #     # routinaria_comparto ~ pct(6),
  #     # routinaria_dirigenza ~ pct(6),
  #     # centriref_comparto ~ pct(6),
  #     # centriref_dirigenza ~ pct(6),
  #     Struttura ~ pct(20),
  #     obiettivi_tot ~ pct(10),
  #     valorizzata_tot ~ pct(10),
  #     routinaria_tot ~ pct(10),
  #     centriref_tot ~ pct(10)
  #     
  #   ) %>% 
  #   cols_align(
  #     align = "center",
  #     columns = -c(Struttura)) %>%
  #   opt_css(
  #     css = "
  #   .gt_col_heading {
  #     vertical-align: middle !important;
  #   }
  #   "
  #   ) 
  # 

















