
Query<-function(fixed="SELECT COLUMN_NAME, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME=", tabella="'nometab'"){
  paste(fixed, tabella)
}

q<-Query(tabella = "'V_MappaStrategiciOperativi'")



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



query <- myfun(con=con, q=q, tabella = "V_MappaStrategiciOperativi")

mp <- tbl(con, sql(query)) %>% 
  as_tibble()


tbl(con, sql())
