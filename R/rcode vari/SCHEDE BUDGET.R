library(tidyverse)
library(readr)
library(readxl)
library(here)
library(lubridate)
library(DBI)
library(odbc)


conSB <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "CED-IIS2",
                          Database = "ObiettiviStrategici2022", Port = 1433)

Query<-function(fixed="SELECT COLUMN_NAME, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME=", tabella="'nometab'"){
  paste(fixed, tabella)
}

q<-Query(tabella = "'vSchedaBudget'")



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


df<-fte %>% 
  mutate(Pesatura = ifelse(Pesatura != "no", "si", "no"), 
         Valorizzato = ifelse(Valorizzato != "no", "si", "no"))

fteDip <- df %>% 
  group_by(Dipartimento, AreaStrategica,  ObiettivoGenerale, ObiettivoOperativo, Indicatore) %>% 
  summarise(FTED = sum(FTED, na.rm = TRUE), 
            FTEC = sum(FTEC, na.rm = TRUE)) %>% View()
 