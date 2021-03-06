## full time equivalenti al 1 gennaio 2022####

library(here)
library(tidyverse)
library(readxl)
library(gt)
dati22 <- read_excel(here("data", "raw", "presenze2022.xlsx"))

CC <- readRDS(here("data", "processed", "CC.rds"))


dati22 %>% 
  mutate(peror = as.numeric(`Perc Orario`)) %>%  
    left_join(  
             (CC %>% 
              select(Dipartimento, Reparto, Laboratorio, CDC, CodiceCDC) %>% 
              unique() ) , by = c("CODICE_CDC" = "CodiceCDC")) %>%  
  
  mutate(Laboratorio = recode(Laboratorio, 
                             "LABORATORIO DI CONTROLLO DI PRODOTTI BIOLOGICI, FARMACEUTICI E CONVALIDA DI PROCESSI PRODUTTIVI" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO", 
                             "LABORATORIO PRODUZIONE TERRENI" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO", 
                             "LABORATORIO ANALISI GENOMICHE, LABORATORIO DIAGNOSTICA MOLECOLARE, OGM" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE", 
                             "LABORATORIO BATTERIOLOGIA SPECIALIZZATA" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE", 
                             "LABORATORIO DI PROTEOMICA E DIAGNOSTICA TSE" = "REPARTO VIROLOGIA", 
                             "LABORATORIO DI VIROLOGIA E SIEROLOGIA SPECIALIZZATA, MICROSCOPIA ELETTRONICA" = "REPARTO VIROLOGIA", 
                             "LABORATORIO CHIMICA APPLICATA ALLE TECNOLOGIE ALIMENTARI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI", 
                             "LABORATORIO CONTAMINANTI AMBIENTALI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI", 
                             "LABORATORIO MANGIMI E TOSSICOLOGIA" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI")) %>%  
  
  
  mutate(contr  = ifelse( Dirigente == "N", (36*peror)/100, (38*peror)/100),
         hcontr =  contr*47.4) %>% 
  
  select(Dipartimento, Reparto, Laboratorio, Dirigente, contr, hcontr) %>%  
  group_by(Dipartimento, Reparto, Laboratorio, Dirigente) %>% 
  
  summarise(hcontr = sum(hcontr)) %>% 
  mutate(FTE = ifelse(Dirigente == "S", hcontr/(38*47.4), hcontr/(36*47.4))) %>%  
  select(-hcontr) %>% 
 
  pivot_wider(names_from = Dirigente, values_from = FTE) %>%  
  rename( "FTED" = S, 
          "FTEC" = N)  %>% 
  ungroup() %>%   
  
  gt() %>% 
  gtsave("fte.rtf")
  

 
 
 







































ftepREP











pubsdip <-  pub %>% 
                      filter(OA == 2019 & Dipartimento == "DIPARTIMENTO AREA TERRITORIALE EMILIA ROMAGNA")



prdip <-  
  prj %>% 
    mutate("Stato" = ifelse(annofine < 2019, "Archiviato", "Attivo")) %>% 
    filter(Stato == "Attivo" & annoinizio <= 2019 & Dipartimento == "DIPARTIMENTO AREA TERRITORIALE EMILIA ROMAGNA")









tdiprep<- tabIZSLER %>% 
  rename( "Prestazioni" = TotPrestazioni, "Valorizzazione" = TotTariff, "VP" = TotFattVP, "AI" = TAI, 
          "COSTI" = TotCost, "FTED" = FTE_Dirigenza, "FTEC"= FTE_Comparto, Anno = ANNO) %>%
  filter(Anno == 2019 & Dipartimento == "DIPARTIMENTO AREA TERRITORIALE EMILIA ROMAGNA") %>% 
  group_by(Reparto) %>%
  summarise_at(c("Prestazioni", "Valorizzazione",  "VP", "AI", "FTED", "FTEC","COSTI"), sum, na.rm = T) %>%  
  mutate(RT = (Valorizzazione+VP+AI),
         FTET = round((FTED+FTEC),2)) %>%
  arrange(desc(Prestazioni)) %>% 
  #select(-FTED, -FTEC)) %>% 
  left_join(
    (pubsdip %>%
       filter(articoliif == "IF") %>%
       count(Reparto, NR) %>%
       group_by(Reparto) %>%  
       count(NR) %>%
       summarise("Pubblicazioni" = sum(n))), by = "Reparto") %>%    
  left_join(
    (prdip %>%
       group_by(Reparto) %>%
       summarise("Progetti di Ricerca"=nlevels(factor(Codice)))
    ), by = "Reparto") %>% 
  left_join(
    ftepREP, by = "Reparto"
  ) %>%
  mutate(RFTE = RT/(FTET*(FTp/100)))  


flextable(tdiprep, 
          
          col_keys = c("Reparto", "Prestazioni", "Valorizzazione", "VP", "AI", "RT",  "COSTI",
                       "FTED",  "FTEC",  "FTET",  "FTp",  "RFTE", "Pubblicazioni", "Progetti di Ricerca")
) %>%   
  theme_booktabs() %>% 
  color(i = 1, color = "blue", part = "header") %>%
  bold( part = "header") %>%
  fontsize(size=15) %>%
  fontsize(part = "header", size = 15) %>%
  line_spacing(space = 2.5) %>% 
  autofit() %>%
  colformat_num(j = c( "Valorizzazione", "VP", "AI", "COSTI",  "RT", "RFTE" ), big.mark = ".", decimal.mark = ",", prefix = "€") %>%
  colformat_num(j= c("Prestazioni", "FTET"), big.mark = ".", decimal.mark = "," , digits = 2) %>% 
  colformat_num(j= c("FTp" ), big.mark = ".", decimal.mark = ",", digits = 2, suffix = "%") %>% 
  
  footnote(i=1, j=2:12,
           value = as_paragraph(
             c("Attività analitica/Altre Prestazioni",
               "Valorizzazione da Tariffario",
               "Fatturato da Vendita Prodotti",
               "Valorizzazione dell'Attività Interna",
               "Ricavo Totale",
               "Costi complessivi",
               "Full Time Equivalenti Dirigenza",
               "Full Time Equivalenti Comparto",
               "Full Time Equivalenti Totale",
               "Full Time Equivalenti Programmati per l'attività analitica",
               "Ricavo per Full Equivalenti Programmati")
           ),
           ref_symbols = c("a","b","c","d","e","f","g","h", "i", "l", "m"),
           part = "header", inline = T) %>%
  fontsize( i = NULL, j = NULL, size = 13, part = "footer") %>% View()
  
  
  htmltools_value() 


flextable(tdiprep)




IZSLER <- tizsler %>% 
                      filter(Anno == 2019)

pubs <- pub %>% 
                   filter(OA == 2019)



pr <- prj %>% 
                 mutate("Stato" = ifelse(annofine < 2019, "Archiviato", "Attivo")) %>% 
                 filter(Stato == "Attivo" & annoinizio <= 2019)

tdip <- #questo codice prepara la tabella complessiva dei dipartimenti
  IZSLER %>%
    left_join(
      (pubs %>%
         filter(articoliif == "IF") %>%
         # count(Dipartimento, NR) %>%
         group_by(Dipartimento) %>%  
         # count(NR) %>%
         summarise("Pubblicazioni" = nlevels(factor(NR)), 
                   "Impact Factor" = sum(IF, na.rm = TRUE))), by = "Dipartimento") %>%    
    left_join(
      (pr %>%
         group_by(Dipartimento) %>%
         summarise("Progetti di Ricerca"=nlevels(factor(Codice)))
      ),  by = "Dipartimento" ) %>% 
    left_join(#aggiungola tabella con i fte programmati per dipartimento
      ftepDIP, by="Dipartimento"
    ) %>% 
    mutate(RFTE = RT/(FTET*(FTp/100))) %>% View()#< questo calcola il ricavo fte usando solo la % di fte allocata alle attività istituzionali 



 