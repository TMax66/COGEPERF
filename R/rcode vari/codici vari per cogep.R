library(timevis)
library(janitor)
library(reshape2)
library(hrbrthemes)
library(scales)
library(readxl)



timing <- read_excel(here("data", "raw",  "cronoprogrammapiao.xlsx"), 
                     col_types = c( "text", "date", 
                                    "date"))

timing <- timing %>% 
  mutate(event = factor(event, levels = c("Definizione cronoprogramma attività del PIAO",
                                          "Individuazione degli indicatori della dimensione interna ed esterna",
                                          "Redazione dei documenti della parte generale e della parte specifica del PIAO",
                                          "Individuazione degli obiettivi di performance 2023-2025",
                                          "Consegna della bozza del PIAO al CERVAP per valutazione",
                                          "Trasmissione al NVP e al CdA della Bozza del PIAO", 
                                          "Adozione del PIAO 2023-2025")))  




melt(timing,  measure.vars = c("start", "end")) %>% 
  mutate(event = fct_rev(event), 
         value = as.Date(value)) %>% 
  ggplot(aes(value, event,  color = event))+
  geom_line(size = 10, alpha = 0.8)+ 
  labs(title =  "CRONOPROGRAMMA DELLE ATTIVITA' LEGATE ALLA REALIZZAZIONE DEL PIAO 2023-2025", y="", x="")+
  theme_ipsum_rc()+
  theme(legend.position = "blank")+
  scale_x_date(labels = date_format("%d-%m-%Y"), breaks = "1 month")+
  scale_color_manual(values=c(rep("#487DA8", 16)))

