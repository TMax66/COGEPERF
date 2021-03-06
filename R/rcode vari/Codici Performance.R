# library(tidyverse)
# library(readr)
# library(readxl)
# library(here)
# library(lubridate)
# library(DBI)
# library(odbc)
# library(knitr)
# library(kableExtra)
# library(formattable)
# library(fmsb)
# 
# #perf <- readRDS(here("data", "processed", "performance.RDS"))
# 
# con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "CED-IIS2",
#                       Database = "ObiettiviStrategiciV2018", Port = 1433)
# 
# queryPERF <- "SELECT
# Avanzamento,
# Valore,
# Anno,
# TipoObiettivo,
# Periodo,
# MacroArea,Obiettivo,
# Azione,
# Indicatore,
# StrutturaAssegnataria
# 
# FROM ObiettiviStrategiciV2018.dbo.v_EstrazioneObiettivi
# WHERE Anno > 2020"
# 
# perf <- con %>% tbl(sql(queryPERF)) %>% as_tibble()
# 
# strutture <- read_excel(here("data", "raw", "strutture.xlsx"))
# 
# 
# dip <- c("DIPARTIMENTO AREA TERRITORIALE EMILIA ROMAGNA", "DIPARTIMENTO SICUREZZA ALIMENTARE",
#          "DIPARTIMENTO TUTELA SALUTE ANIMALE", "DIPARTIMENTO AREA TERRITORIALE LOMBARDIA",
#          "DIPARTIMENTO AMMINISTRATIVO", "CONTROLLO DI GESTIONE")
# 
# 
# ##ricordificare le strutture
# 
# dt <- perf %>%
#   filter(!StrutturaAssegnataria %in% dip & TipoObiettivo == "Operativo" ) %>% 
#   mutate(Struttura = recode(StrutturaAssegnataria,
#                             "S.T. PIACENZA E PARMA" = "SEDE TERRITORIALE DI PIACENZA - PARMA",
#                             "REP. CHIM. DEGLI ALIMENTI E MANGIMI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI",
#                             "REP. CHIMICO ALIMENTI BOLOGNA" = "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)",
#                             "REP. PRODUZIONE PRIMARIA" = "REPARTO PRODUZIONE PRIMARIA",
#                             "S.T. BOLOGNA, FERRARA E MODENA" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA",
#                             "S.T. REGGIO EMILIA" = "SEDE TERRITORIALE DI REGGIO EMILIA",
#                             "REP. VIROLOGIA" = "REPARTO VIROLOGIA",
#                             "REP. VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE",
#                             "S.T. BERGAMO, SONDRIO E BINAGO" = "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO",
#                             "S.T. BRESCIA" = "SEDE TERRITORIALE DI BRESCIA",
#                             "S.T. CREMONA, MANTOVA" = "SEDE TERRITORIALE DI CREMONA - MANTOVA",
#                             "S.T. FORLI' E RAVENNA" = "SEDE TERRITORIALE DI FORLÌ - RAVENNA",
#                             "S.T. LODI E MILANO" = "SEDE TERRITORIALE DI LODI - MILANO",
#                             "S.T. PAVIA" = "SEDE TERRITORIALE DI PAVIA",
#                             "U.O. PROVV. ECONOMATO E VENDITE" = "UO PROVVEDITORATO ECONOMATO E VENDITE",
#                             "SERVIZIO ASSICURAZIONE QUALITA" = "SERVIZIO ASSICURAZIONE QUALITA'",
#                             "U.O. AFFARI GENERALI E LEGALI" = "U.O. AFFARI GENERALI E LEGALI",
#                             "U.O. TECNICO PATRIMONIALE" = "UO TECNICO PATRIMONIALE",
#                             "U.O. GESTIONE RISORSE UMANE E SVILUPPO COMPETENZE" = "U.O. GESTIONE RISORSE UMANE E SVILUPPO COMPETENZE",
#                             "U.O. GESTIONE SERVIZI CONTABILI" = "U.O. GESTIONE SERVIZI CONTABILI",
#                             "PROGRAMMAZIONE DEI SERVIZI TECNICI E CONTROLLO DI GESTIONE" = "Programmazione dei servizi tecnici e controllo di gestione",
#                             "FORMAZIONE" =  "FORMAZIONE E BIBLIOTECA",
#                             "SISTEMI INFORMATIVI" = "Programmazione dei servizi tecnici e controllo di gestione",
#                             "SEGRETERIA DIREZIONALE" = "DIREZIONE GENERALE",
#                             "GESTIONE CENTRALIZZATA DELLE RICHIESTE DELL'UTENZA" = "GESTIONE CENTRALIZZATA DELLE RICHIESTE")
#          
#   )
# 
# dt <- dt %>% rename( Reparto = Struttura ) %>%
#   left_join(
#     
#     (strutture %>% select(Dipartimento, Reparto) %>%
#        unique())
#     
#     
#     ,  by = c("Reparto"))  


pPerf <-  perf %>%
                    filter(Periodo == 4 & Avanzamento != 0 ) %>%
                    mutate(MacroArea = factor(MacroArea)) %>%
                    group_by(MacroArea) %>%
                    summarise(media = 100*round(mean(Avanzamento, na.rm = T),2),
                              n = n()) %>%
                    mutate(target = 100) %>%
                    mutate(MacroArea = as.character(MacroArea)) %>%
                    mutate(MacroArea = gsub("\\d+", "", MacroArea),
                           MacroArea = gsub("\"", "", MacroArea))
)





dt <- perf
library(gt)
 
AV2 %>% 
  filter(Periodo == 4 ) %>% 
  group_by(MacroArea) %>% 
  summarise(media = 100*round(mean(Avanzamento,na.rm  = T),2), 
            min = 100*min(Avanzamento))  %>% 
  ungroup %>% 
  add_row(MacroArea = 'Livello Sintetico di Ente', !!! colMeans(.[-1])) %>% 
  gt() %>%  
gtsave("LSE.pdf")
  
  
  
  library(tibble)
df2 %>% 
  ungroup %>% 
  add_row(year = 'mean', !!! colMeans(.[-1]))



AV2 <- dt %>% 
  filter(Periodo == 4) %>% 
  mutate(Avanzamento = ifelse(Avanzamento == 0, 1, Avanzamento) )






library(flexdashboard)

x <- gauge(74, min= 0, max = 100, symbol = '%',
      gaugeSectors(success = c(0,100),   colors = "steelblue"))

x %>%
  knit_print()





#Dipartimenti----

dt %>% 
  filter(Periodo == 2 & Avanzamento != 0 ) %>% 
  group_by(Dipartimento) %>% 
  summarise(media = 100*round(mean(Avanzamento,na.rm  = T),2)) %>% View()
  
# filter(Dipartimento == "Direzione Generale")
  
  

#Avanzamento per Area----
Area <-  perf %>%
    filter(Periodo == 2 & Avanzamento != 0 ) %>% 
  mutate(MacroArea = factor(MacroArea)) %>%  
  group_by(MacroArea) %>% 
  summarise(mediana =  round(median(Avanzamento, na.rm = T),2),
            media = round(mean(Avanzamento,na.rm  = T),2), 
            n = n()) %>% 
  mutate(mediana = percent(mediana), 
         mediana = as.character(mediana), 
         media = percent(media),
         media = as.character(media)) %>%   
  #pivot_wider(names_from = "Dipartimento", values_from = "mediana", values_fill = " ") %>%  
  arrange(MacroArea) %>% 
  mutate(MacroArea = as.character(MacroArea)) %>% 
  mutate(MacroArea = gsub("\\d+", "", MacroArea), 
         MacroArea = gsub("\"", "", MacroArea))  %>%  
  kbl( ) %>% 
  kable_styling() %>% 
  kable_paper(bootstrap_options = "striped", full_width = F)


#Polar plot avanzamento per Area----

plot_dt <- dt %>%  
    filter(Periodo == 2 & Avanzamento != 0 ) %>% 
  mutate(MacroArea = factor(MacroArea)) %>% 
  group_by(MacroArea) %>% 
  summarise(mediana =  100*round(median(Avanzamento, na.rm = T),2),
            media = 100*round(mean(Avanzamento, na.rm = T),2), 
            n = n()) %>% 
  mutate(target = 100) %>% 
  mutate(MacroArea = as.character(MacroArea)) %>% 
  mutate(MacroArea = gsub("\\d+", "", MacroArea), 
         MacroArea = gsub("\"", "", MacroArea))
  
#Polar plot avanzamento per Area/Dip---
plot_dt2 <- dt %>%  
  mutate(MacroArea = factor(MacroArea)) %>% 
  group_by(MacroArea, Dipartimento) %>% 
  summarise(mediana =  100*round(median(Avanzamento, na.rm = T),2),
            media = 100*round(mean(Avanzamento, na.rm = T),2), 
            n = n()) %>% 
  mutate(target = 100) %>% 
  mutate(MacroArea = as.character(MacroArea)) %>% 
  mutate(MacroArea = gsub("\\d+", "", MacroArea), 
         MacroArea = gsub("\"", "", MacroArea))





##Plot----
plt <- ggplot(plot_dt)+
  geom_hline(
    aes(yintercept = y),
    data.frame(y = c(0, 25, 50, 75, 90, 100)), 
    color = "lightgrey"
  )+
  geom_col(
    aes(x = reorder(str_wrap(MacroArea, 1), media), 
        y = media, 
        fill = media
        ), 
    position = "dodge2", 
    show.legend = TRUE, 
    alpha = .9
  )+
    
  geom_point(
    aes(
      x = reorder(str_wrap(MacroArea, 1), media),
      y = media
    ), 
    size = 3, color = "gray12"
  )+
    
  geom_segment(
    aes(
      x =  reorder(str_wrap(MacroArea, 1), media), 
      y = 0, 
      xend = reorder(str_wrap(MacroArea, 1), media), 
      yend = 100
    ), 
    linetype = "dashed",
    color = "gray12"
  )+
  coord_polar()+
  
  scale_y_continuous(
    limits = c(-20,110),
    expand = c(0, 0)
    
  ) +
  geom_text(
    aes(
      x = reorder(str_wrap(MacroArea, 1), media),
      y = media-10, 
      label = paste0(media, "%")), 
      color = "black", 
      size=5)+
  
  annotate(
    x = 0.5, 
    y = 30, 
    label = "25%", 
    geom = "text", 
    color = "red", 
    family = "Bell MT"
  )  +
  annotate(
    x = 0.5, 
    y = 55, 
    label = "50%", 
    geom = "text", 
    color = "red", 
    family = "Bell MT"
  )  +
  
  annotate(
    x = 0.5, 
    y = 80, 
    label = "75%", 
    geom = "text", 
    color = "red", 
    family = "Bell MT"
  )  +
  
  annotate(
    x = 0.5, 
    y = 110, 
    label = "100%", 
    geom = "text", 
    color = "red", 
    family = "Bell MT"
  )  +
  
 # scale_fill_gradientn(colours = gray.colors(7))+
  
  theme(
    # Remove axis ticks and text
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    # Use gray text for the region names
    axis.text.x = element_text(color = "gray12", size = 8),
    # Move the legend to the bottom
    legend.position = "blank",
  )+

  # Customize general theme
  theme(
    
    # Set default color and font family for the text
    text = element_text(color = "gray12", family = "Bell MT"),
    
    # Customize the text in the title, subtitle, and caption
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 14, hjust = 0.05),
    plot.caption = element_text(size = 10, hjust = .5),
    
    # Make the background white and remove extra grid lines
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank()
  )




##AreaDip-----
AreaDip <-  perf %>%  
  filter(Periodo == 4  ) %>% 
  mutate(MacroArea = factor(MacroArea)) %>% 
  group_by(Dipartimento,  MacroArea) %>% 
  summarise(media =  round(mean(Avanzamento, na.rm = T),2)) %>%  
  mutate(media = percent(media), 
         media = as.character(media)) %>%   
  pivot_wider(names_from = "Dipartimento", values_from = "media", values_fill = " ") %>%  
  select("MacroArea","Direzione Generale", "Direzione Sanitaria", "Dipartimento tutela e salute animale", 
         "Dipartimento sicurezza alimentare","Dipartimento area territoriale Lombardia",
         "Dipartimento area territoriale Emilia Romagna",
         "Dipartimento amministrativo") %>% 
  arrange(MacroArea) %>% 
  mutate(MacroArea = as.character(MacroArea)) %>% 
  mutate(MacroArea = gsub("\\d+", "", MacroArea), 
         MacroArea = gsub("\"", "", MacroArea))  %>% 
  rename("Macro Area" = "MacroArea") %>% 
  kbl( ) %>% 
  kable_styling() %>% 
  kable_paper(bootstrap_options = "striped", full_width = F)# %>% 
  save_kable(file = "tab1.png")



#plot aredip----

plt2 <- ggplot(plot_dt2)+
  geom_hline(
    aes(yintercept = y),
    data.frame(y = c(0, 25, 50, 75, 90, 100)),
    color = "lightgrey"
  )+
  geom_col(
    aes(x = reorder(str_wrap(MacroArea, 1), media),
        y = media,
        fill = media
    ),
    position = "dodge2",
    show.legend = TRUE,
    alpha = .9
  )+

  geom_point(
    aes(
      x = reorder(str_wrap(MacroArea, 1), media),
      y = media
    ),
    size = 3, color = "gray12"
  )+

  geom_segment(
    aes(
      x =  reorder(str_wrap(MacroArea, 1), media),
      y = 0,
      xend = reorder(str_wrap(MacroArea, 1), media),
      yend = 100
    ),
    linetype = "dashed",
    color = "gray12"
  )+
  coord_polar()+

  scale_y_continuous(
    limits = c(-20,110),
    expand = c(0, 0)

  ) +
  geom_text(
    aes(
      x = reorder(str_wrap(MacroArea, 1), media),
      y = media-10,
      label = paste0(media, "%")),
    color = "black",
    size=5)+

  annotate(
    x = 0.5,
    y = 30,
    label = "25%",
    geom = "text",
    color = "red",
    family = "Bell MT"
  )  +
  annotate(
    x = 0.5,
    y = 55,
    label = "50%",
    geom = "text",
    color = "red",
    family = "Bell MT"
  )  +

  annotate(
    x = 0.5,
    y = 80,
    label = "75%",
    geom = "text",
    color = "red",
    family = "Bell MT"
  )  +

  annotate(
    x = 0.5,
    y = 110,
    label = "100%",
    geom = "text",
    color = "red",
    family = "Bell MT"
  )  +

  scale_fill_gradientn(colours = gray.colors(7))+

  theme(
    # Remove axis ticks and text
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    # Use gray text for the region names
    axis.text.x = element_text(color = "gray12", size = 8),
    # Move the legend to the bottom
    legend.position = "blank",
  )+



  facet_wrap(~Dipartimento, ncol = 5)

plt2 <- plt2+
  labs(
    title = paste("\nGrado di raggiungimento obiettivi di performance:Valutazione Intermedia al 30/06/2021.\n"),
    caption = "U.O. Controllo di Gestione e Performances")+

  # Customize general theme
  theme(

    # Set default color and font family for the text
    text = element_text(color = "gray12", family = "Bell MT"),

    # Customize the text in the title, subtitle, and caption
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 14, hjust = 0.05),
    plot.caption = element_text(size = 10, hjust = .5),

    # Make the background white and remove extra grid lines
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank()
  )










###OBIETTIVI ----

Obiettivi <-  dt %>%  
  mutate(Obiettivo = factor(Obiettivo)) %>% 
  group_by(Dipartimento,  Obiettivo) %>% 
  summarise(media =  round(mean(Avanzamento, na.rm = T),2)) %>% 
  mutate(media = percent(media), 
         media = as.character(media)) %>%
  pivot_wider(names_from = "Dipartimento", values_from = "media", values_fill = " ") %>%  
  select("Obiettivo","Direzione Generale", "Direzione Sanitaria", "Dipartimento tutela e salute animale", 
         "Dipartimento sicurezza alimentare","Dipartimento area territoriale Lombardia",
         "Dipartimento area territoriale Emilia Romagna",
         "Dipartimento amministrativo") %>%  
  arrange(Obiettivo) %>% 
  mutate(Obiettivo = as.character(Obiettivo)) %>% 
  mutate(Obiettivo = gsub("\\d+", "", Obiettivo), 
         Obiettivo = gsub("\\.", "", Obiettivo), 
         Obiettivo = gsub("\\)", "", Obiettivo),
         Obiettivo = gsub("\"", "", Obiettivo)) %>% .[-5,] %>% 
  kbl() %>% 
  kable_styling() %>% 
  kable_paper(bootstrap_options = "striped", full_width = F) %>% 
  save_kable(file = "tab2.png")



##plot obiettivi

plot_ob <-  dt %>%  
  mutate(Obiettivo = factor(Obiettivo)) %>% 
  group_by(Obiettivo) %>% 
  summarise(media =  100*round(mean(Avanzamento, na.rm = T),2)) %>% 
  arrange(Obiettivo) %>% 
  mutate(Obiettivo = as.character(Obiettivo)) %>% 
  mutate(Obiettivo = gsub("\\d+", "", Obiettivo), 
         Obiettivo = gsub("\\.", "", Obiettivo), 
         Obiettivo = gsub("\\)", "", Obiettivo),
         Obiettivo = gsub("\"", "", Obiettivo)) %>% .[-5,]

plt <- ggplot(plot_ob)+
  aes(y = Obiettivo, x = media)+
  geom_point()
   

Indicatori <-   dt %>%  
  mutate(Indicatore = factor(Indicatore)) %>% 
  group_by(Dipartimento,  Indicatore) %>% 
  summarise(mediana =  round(median(Avanzamento, na.rm = T),2)) %>% 
  mutate(mediana = percent(mediana), 
         mediana = as.character(mediana)) %>%
  pivot_wider(names_from = "Dipartimento", values_from = "mediana", values_fill = " ") %>% 
  select("Indicatore","Direzione Generale", "Direzione Sanitaria", "Dipartimento tutela e salute animale", 
         "Dipartimento sicurezza alimentare","Dipartimento area territoriale Lombardia",
         "Dipartimento area territoriale Emilia Romagna",
         "Dipartimento amministrativo") %>% 
  arrange(Indicatore) %>% 
  mutate(Indicatore = as.character(Indicatore)) %>% 
  mutate(Indicatore = gsub("\\d+", "", Indicatore), 
         Indicatore = gsub("\\.", "", Indicatore), 
         Indicatore = gsub("\\)", "", Indicatore),
         Indicatore = gsub("\"", "", Indicatore)) %>% View()
kbl() %>% 
  kable_styling()


dt %>% 
  group_by(Azione) %>% 
  unique() %>% 
  count() %>% 
  count() %>%  View()

  
  
  
  # summarise(min= min(Avanzamento, na.rm = T), 
  #                    mediana = median(Avanzamento, na.rm = T), 
  #                                     media = mean(Avanzamento, na.rm = T),
  #                                     max = max(Avanzamento, na.rm = T))
  # 





##PlotAreaDip