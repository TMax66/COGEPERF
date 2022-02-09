library("janitor")
library("kintr")
library("kableExtra")
library("readxl")
library("here")
library("tidyverse")

pr <- read_excel(here("programmazione", "piramideR", "pr2020.xlsx"))

# pr %>% 
#   select(-14, -15) %>%
#   mutate("Stato" = ifelse(DataFine < as.Date("2020-01-01"), "Archiviato", "Attivo")) %>%
#   filter(Stato == "Attivo" & DataInizio <= as.Date("2020-12-31")) %>%
#   mutate("Statoanno" = ifelse(DataFine <=as.Date("2020-12-31"), "Concluso", "In corso")) %>%
#   mutate("Nuovo" = ifelse(DataInizio > as.Date("2019-12-31") & 
#                             DataInizio <= as.Date("2020-12-31"), "Nuovo", "Vecchio")) %>% 
#   filter(Nuovo == "Vecchio") %>% 
#   distinct(., CodIDIzler,.keep_all = TRUE) %>% 
#   group_by(Tipologia, Statoanno) %>% 
#   summarise(N.Progetti = n()) %>% 
#   pivot_wider(names_from = Statoanno, values_from = N.Progetti)  %>%  
#   
#   left_join (  
# (pr %>% 
#   select(-14, -15) %>%
#   mutate("Nuovo" = ifelse(DataInizio > as.Date("2019-12-31") & 
#                             DataInizio <= as.Date("2020-12-31"), "Nuovo", "Vecchio")) %>% 
#   filter(Nuovo == "Nuovo") %>% 
#   distinct(., CodIDIzler,.keep_all = TRUE) %>% 
#   group_by(Tipologia) %>% 
#   summarise(Nuovi = n()) ), by = c("Tipologia")
# ) %>% 
#   mutate(anno = 2015)
#%>% 
  # adorn_totals(where = "col") %>%   
  # 
  # adorn_totals(where = "row") %>% 
  # 
  

  #writexl::write_xlsx("progetti.xlsx")


  fun_prj <- function(anno, datafine, datainizio, datafine2, datainizio2, datainizio3)
  {
    pr %>% 
      select(-14, -15) %>%
      mutate("Stato" = ifelse(DataFine < as.Date(datafine), "Archiviato", "Attivo")) %>%
      filter(Stato == "Attivo" & DataInizio <= as.Date(datainizio)) %>%
      mutate("Statoanno" = ifelse(DataFine <=as.Date(datafine2), "Concluso", "In corso")) %>%
      mutate("Nuovo" = ifelse(DataInizio > as.Date(datainizio2) & 
                                DataInizio <= as.Date(datainizio3), "Nuovo", "Vecchio")) %>% 
      filter(Nuovo == "Vecchio") %>% 
      distinct(., CodIDIzler,.keep_all = TRUE) %>% 
      group_by(Tipologia, Statoanno) %>% 
      summarise(N.Progetti = n()) %>% 
      pivot_wider(names_from = Statoanno, values_from = N.Progetti)  %>%  
      
      left_join (  
        (pr %>% 
           select(-14, -15) %>%
           mutate("Nuovo" = ifelse(DataInizio > as.Date(datainizio2) & 
                                     DataInizio <= as.Date(datainizio3), "Nuovo", "Vecchio")) %>% 
           filter(Nuovo == "Nuovo") %>% 
           distinct(., CodIDIzler,.keep_all = TRUE) %>% 
           group_by(Tipologia) %>% 
           summarise(Nuovi = n()) ), by = c("Tipologia")
      ) %>% 
      # adorn_totals(where = "col") %>%   
      # 
      # adorn_totals(where = "row") %>% 
      
      mutate(anno = anno)
      
    
    
  }
  
  prj_2015<-fun_prj(anno = 2015, datafine = "2015-01-01", 
          datainizio = "2015-12-31", 
          datafine2 = "2015-12-31", 
          datainizio2 = "2014-12-31",
          datainizio3 = "2015-12-31")

  prj_2016<-fun_prj(anno = 2016, datafine = "2016-01-01", 
                    datainizio = "2016-12-31", 
                    datafine2 = "2016-12-31", 
                    datainizio2 = "2015-12-31",
                    datainizio3 = "2016-12-31")
  prj_2017<-fun_prj(anno = 2017, datafine = "2017-01-01", 
                    datainizio = "2017-12-31", 
                    datafine2 = "2017-12-31", 
                    datainizio2 = "2016-12-31",
                    datainizio3 = "2017-12-31")
  prj_2018<-fun_prj(anno = 2018, datafine = "2018-01-01", 
                    datainizio = "2018-12-31", 
                    datafine2 = "2018-12-31", 
                    datainizio2 = "2017-12-31",
                    datainizio3 = "2018-12-31")
  prj_2019<-fun_prj(anno = 2019, datafine = "2019-01-01", 
                    datainizio = "2019-12-31", 
                    datafine2 = "2019-12-31", 
                    datainizio2 = "2018-12-31",
                    datainizio3 = "2019-12-31")
  prj_2020<-fun_prj(anno = 2020, datafine = "2020-01-01", 
                    datainizio = "2020-12-31", 
                    datafine2 = "2020-12-31", 
                    datainizio2 = "2019-12-31",
                    datainizio3 = "2020-12-31")
  
  PRJ <- rbind(prj_2015, prj_2016, prj_2017, prj_2018, prj_2019, prj_2020)
  
  PRJ %>% 
    pivot_longer(cols = 2:4, names_to = "Stato") %>% 
    mutate(value = replace_na(value, 0)) %>%  
    mutate(value = as.double(value)) %>% 
    pivot_wider(names_from = anno, values_from = c("value"), values_fill = 0) %>% 
    # filter(Tipologia %in% c("Corrente", "Autofinanziato")) %>% 
    #group_split(Tipologia) %>% 
    kbl() %>% 
    kable_paper() %>% 
    column_spec(1, bold = T) %>% 
      collapse_rows(columns = 1:2, valign = "top") %>% 
     column_spec(8, color = "red")
 
    
    ggplot(aes(x=anno, y=value,group=Stato, color=Stato))+
    geom_line()+
    facet_wrap(~Tipologia)
    
    
  

  