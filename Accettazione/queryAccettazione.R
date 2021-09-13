library(tidyverse)

#importazione accettazione----- da query esportata da SQLServer ( vedi sotto)

library(readr)
library(lubridate)

acc <- read_delim("C:/Users/vito.tranquillo/Desktop/Git Projects/COGEPERF/Accettazione/postazioniNew.txt", 
                         "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

names(acc) <- c("nconf", "strpropr", "settore", "finalità", "pagamento", 
                "dtprel", "dtreg", "dtacc", "dtrdp", "IstRDP",  "pc", "gruppoprova", "ncamp")



#

acc21 <- read_delim("C:/Users/vito.tranquillo/Desktop/Git Projects/COGEPERF/Accettazione/ACC21xxx.txt", 
                  "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)


names(acc21) <- c("Anno", "PC", "Nconf", "Stracc", "Strpr", "Stran")  

pc <- c('ACC-CENTR2', 'PC-47326', 'PC-40780', 'MP-ACC3', 'BS-ASS-N', 'PC-47327', 'CH-ACC4-N', 'CH-ACC2-N', 'MP-SIVARS7', 'PC-47499', 'PC-47929', 'OEVR-8-PORT')


gest <- acc21 %>% 
    filter(PC %in% pc)

#valorizzazione attività accettazione-----
acc %>% filter(gruppoprova!= "Parere Tecnico") %>% 
  mutate(tipoprove = ifelse(gruppoprova=="Prova Chimica", "Prova Chimica", 
                      ifelse(gruppoprova== "Prova Sierologica", "Prova Sierologica", "Prova Diagnostica/Alimenti"))) %>%  
  select(-gruppoprova) %>% 
  group_by(dtreg, nconf, pc, settore ) %>% 
  pivot_wider(names_from = "tipoprove", values_from = "tipoprove") %>% 
  mutate(`Prova Chimica` = ifelse(`Prova Chimica`!= "NULL", 2.46, 0), 
         `Prova Diagnostica/Alimenti` = ifelse(`Prova Diagnostica/Alimenti` != "NULL", 0.72, 0),
         `Prova Sierologica` = ifelse(`Prova Sierologica` != "NULL", 0.20, 0)) %>%  
  rowwise() %>% 
  mutate(valore= sum(`Prova Chimica` ,`Prova Diagnostica/Alimenti`, `Prova Sierologica`), 
         valore = 0.07*(valore)+valore) %>%
  ungroup() %>%  
  group_by(dtreg, pc) %>% 
  summarise(n.conf = n(), 
    valore = sum(valore)) %>% 
  mutate(Anno = year(dtreg)) %>%  
  group_by(Anno) %>% 
  summarise(n.conf = sum(n.conf), 
            valore = sum(valore)) %>% 
  tibble(Dipartimento = "Direzione sanitaria", Reparto = "GESTIONE CENTRALIZZATA DELLE RICHIESTE", 
         Laboratorio = "	GESTIONE CENTRALIZZATA DELLE RICHIESTE")  %>% View()
  saveRDS(here("data", "processed", "GCR.rds"))
  




# SELECT
# dbo.Conferimenti.Numero,
# dbo_Anag_Reparti_ConfProp.Descrizione,
# dbo.Anag_Registri.Descrizione,
# dbo_Anag_Finalita_Confer.Descrizione,
# dbo.Anag_TipoConf.Descrizione,
# dbo.Conferimenti.Data_Prelievo,
# dbo.Conferimenti.Data,
# dbo.Conferimenti.Data_Accettazione,
# dbo.Conferimenti.DataOra_Primo_RDP_Completo_Firmato,
# dbo.RDP_Date_Emissione.Istanza_RDP,
# dbo.Conferimenti.Nome_Stazione_Inserimento,
# dbo.Anag_Gruppo_Prove.Descrizione,
# dbo.Conferimenti.NrCampioni
# FROM
# { oj dbo.Anag_Reparti  dbo_Anag_Reparti_ConfProp INNER JOIN dbo.Laboratori_Reparto  dbo_Laboratori_Reparto_ConfProp ON ( dbo_Laboratori_Reparto_ConfProp.Reparto=dbo_Anag_Reparti_ConfProp.Codice )
#   INNER JOIN dbo.Conferimenti ON ( dbo.Conferimenti.RepLab=dbo_Laboratori_Reparto_ConfProp.Chiave )
#   LEFT OUTER JOIN dbo.Esami_Aggregati ON ( dbo.Conferimenti.Anno=dbo.Esami_Aggregati.Anno_Conferimento and dbo.Conferimenti.Numero=dbo.Esami_Aggregati.Numero_Conferimento )
#   LEFT OUTER JOIN dbo.Nomenclatore_MP ON ( dbo.Esami_Aggregati.Nomenclatore=dbo.Nomenclatore_MP.Codice )
#   LEFT OUTER JOIN dbo.Nomenclatore_Settori ON ( dbo.Nomenclatore_MP.Nomenclatore_Settore=dbo.Nomenclatore_Settori.Codice )
#   LEFT OUTER JOIN dbo.Nomenclatore ON ( dbo.Nomenclatore_Settori.Codice_Nomenclatore=dbo.Nomenclatore.Chiave )
#   LEFT OUTER JOIN dbo.Anag_Gruppo_Prove ON ( dbo.Nomenclatore.Codice_Gruppo=dbo.Anag_Gruppo_Prove.Codice )
#   INNER JOIN dbo.Anag_Registri ON ( dbo.Conferimenti.Registro=dbo.Anag_Registri.Codice )
#   INNER JOIN dbo.Anag_TipoConf ON ( dbo.Anag_TipoConf.Codice=dbo.Conferimenti.Tipo )
#   INNER JOIN dbo.Conferimenti_Finalita ON ( dbo.Conferimenti.Anno=dbo.Conferimenti_Finalita.Anno and dbo.Conferimenti.Numero=dbo.Conferimenti_Finalita.Numero )
#   INNER JOIN dbo.Anag_Finalita  dbo_Anag_Finalita_Confer ON ( dbo.Conferimenti_Finalita.Finalita=dbo_Anag_Finalita_Confer.Codice )
#   LEFT OUTER JOIN dbo.RDP_Date_Emissione ON ( dbo.RDP_Date_Emissione.Anno=dbo.Conferimenti.Anno and dbo.RDP_Date_Emissione.Numero=dbo.Conferimenti.Numero )
# }
# WHERE
# dbo.Esami_Aggregati.Esame_Altro_Ente = 0
# AND  (
#   {fn year(dbo.Conferimenti.Data_Accettazione)}  >=  2019
# 
# AND  dbo.Conferimenti.Nome_Stazione_Inserimento  IN  ('ACC-CENTR2', 'PC-47326', 'PC-40780', 'MP-ACC3', 'BS-ASS-N', 'PC-47327', 'CH-ACC4-N', 'CH-ACC2-N', 'MP-SIVARS7', 'PC-47499', 'PC-47929', 'OEVR-8-PORT'
# )



