cc <- read_delim(here("data", "raw", "coge1921.txt"), 
                 "\t", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                              grouping_mark = "."), trim_ws = TRUE)

names(cc)[5:9] <- c("Dipartimento", "Reparto", "Laboratorio", "Centro di Costo", "CodCC")


cc %>% 
  filter(Anno == 2019 & `Costo o Ricavo`== "Ricavo" & Determinazioni >0 ) %>%
  group_by(Dipartimento, Categoria, Classe, Area, Classificazione) %>% 
  summarise(Prestazioni = sum(Determinazioni, na.rm = TRUE)) %>% 
  group_by(Dipartimento) %>% 
  summarise(TotPrestazioni = sum(Prestazioni)) %>% 
  summarise(sum(TotPrestazioni))



ore <- con %>% tbl(sql(query)) %>% as_tibble()  
names(ore)[1:5] <- c("Dipartimento", "Reparto", "Laboratorio", "Centro di Costo", "CodCC")
fte <- ore %>% 
  mutate(Dirigente = recode(Dirigente, N = "Comparto", S = "Dirigenza")) %>%
  filter(Dipartimento != "Non applicabile") %>% 
  group_by(Anno, Dipartimento, Reparto, Laboratorio, Dirigente) %>%   
  filter(!is.na(Dirigente) & !is.na(Ore)) %>% 
  summarise(hworked = sum(Ore, na.rm = T)) %>% 
  mutate(FTE = ifelse(Dirigente == "Comparto", hworked/(38*47.4), hworked/(36*47.4))) %>% 
  pivot_wider(names_from = "Dirigente", values_from = c("hworked", "FTE"))  


cc %>% 























# analisi <- read_excel(sheet = "Report 1", here(  "data", "raw",  "analisi1921.xls"))
# 
# analisi %>% 
#   filter(Anno == 2019  & Determinazioni >0  ) %>%
#   group_by( Livello0,  Classe, Area, Classificazione) %>% 
#   summarise(Prestazioni = sum(Determinazioni, na.rm = TRUE)) %>%  
#   group_by(Livello0) %>%  
#   summarise(TotPrestazioni = sum(Prestazioni)) %>% 
#   summarise(sum(TotPrestazioni))
