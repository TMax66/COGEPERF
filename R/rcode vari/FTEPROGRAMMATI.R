# codice per la definizione della 5 di fte programmati nel 2021
# viene eseguito una sola volta per la costruzione degli ftp del 2021....
# non deve essere rilanciato ad ogni aggiornamento dei dati del controllo di gestione

obfted <- read_excel(here( "data", "raw", "obiettiviXSB.xlsx"), sheet = "FTEDdipsan")
obftec <- read_excel(here( "data", "raw", "obiettiviXSB.xlsx"), sheet = "FTECdipsan")

dtD <- obfted %>% 
  mutate(obcod = paste("OB", seq(1:nrow(.)))) %>%
  pivot_longer(3:37, names_to = "struttura", values_to = "FTED") %>% 
  mutate(
    struttura = recode(struttura, 
                       "STBO" = "Sede Territoriale di Bologna", 
                       "STFE" = "Sede Territoriale di Ferrara", 
                       "STMO" = "Sede Territoriale di Modena", 
                       "STPC" = "Sede Territoriale di Piacenza",
                       "STPR" = "Sede Territoriale di Parma",
                       "STFO" = "Sede Territoriale di Forlì",
                       "STRA" = "Sede Territoriale di Ravenna", 
                       "STBG" = "Sede Territoriale di Bergamo", 
                       "STBI" = "Sede Territoriale di Binago", 
                       "STSO" = "Sede Territoriale di Sondrio", 
                       "STLO" = "Sede Territoriale di Lodi", 
                       "STMI" = "Sede Territoriale di Milano", 
                       "STCR" = "Sede Territoriale di Cremona", 
                       "STMN" = "Sede Territoriale di Mantova", 
                       "STPV" = "Sede Territoriale di Pavia", 
                       "STBS" = "Sede Territoriale di Brescia", 
                       "STRE" = "Sede Territoriale di Reggio Emilia", 
                       
                       "RPP" = "Reparto Produzione Primaria", 
                       "RCABO" = "Reparto Chimico Alimenti (Bologna)", 
                       "RCA" = "Reparto Controllo Alimenti", 
                       "RCAM" = "Reparto Chimica degli Alimenti e Mangimi", 
                       
                       "RVIR" = "Reparto Virologia", 
                       "RVVPB" = "Reparto Virus Vescicolari e Produzioni Biotecnologiche", 
                       
                       "RTBA" = "Reparto Tecnologie Biologiche Applicate", 
                       "RPCMB" = "Reparto Produzione e Controllo Materiale Biologico", 
                       
                       "AREG" = "Analisi del Rischio ed Epidemiologia Genomica", 
                       "SORVEPIDEM" = "Sorveglianza Epidemiologica", 
                       "GESTCENT" = "Gestione Centralizzata delle Richieste", 
                       "FORMAZIONE" = "Formazione e Biblioteca", 
                       "SAQ" = "Servizio Assicurazione Qualità", 
                       "AFFGENLEG" = "U.O. Affari Generali Legali", 
                       "GESTRISUM" = "U.O. Gestione Risorse Umane", 
                       "UOTECPAT" = "U.O. Tecnico Patrimoniale", 
                       "PROVV" = "U.O. Provveditorato Economato e Vendite", 
                       "SERVCONT" = "U.O. Gestione Servizi Contabili"
                       
                       
    ), 
    
    reparto = recode(struttura, "Sede Territoriale di Bologna" = "SEDE TERRITORIALE DI BOLOGNA-MODENA-FERRARA", 
                     "Sede Territoriale di Ferrara" = "SEDE TERRITORIALE DI BOLOGNA-MODENA-FERRARA", 
                     "Sede Territoriale di Modena" = "SEDE TERRITORIALE DI BOLOGNA-MODENA-FERRARA", 
                     "Sede Territoriale di Piacenza" = "SEDE TERRITORIALE DI PIACENZA-PARMA", 
                     "Sede Territoriale di Parma" = "SEDE TERRITORIALE DI PIACENZA-PARMA",
                     "Sede Territoriale di Forlì" = "SEDE TERRITORIALE DI FORLI-RAVENNA", 
                     "Sede Territoriale di Ravenna" = "SEDE TERRITORIALE DI FORLI-RAVENNA", 
                     "Sede Territoriale di Bergamo" = "SEDE TERRITORIALE DI BERGAMO-BINAGO-SONDRIO", 
                     "Sede Territoriale di Binago" = "SEDE TERRITORIALE DI BERGAMO-BINAGO-SONDRIO", 
                     "Sede Territoriale di Sondrio" = "SEDE TERRITORIALE DI BERGAMO-BINAGO-SONDRIO", 
                     "Sede Territoriale di Lodi" = "SEDE TERRITORIALE DI LODI-MILANO", 
                     "Sede Territoriale di Milano" = "SEDE TERRITORIALE DI LODI-MILANO", 
                     "Sede Territoriale di Cremona" = "SEDE TERRITORIALE DI CREMONA-MANTOVA", 
                     "Sede Territoriale di Mantova" = "SEDE TERRITORIALE DI CREMONA-MANTOVA", 
                     "Sede Territoriale di Pavia" = "SEDE TERRITORIALE DI PAVIA", 
                     "Sede Territoriale di Brescia" = "SEDE TERRITORIALE DI BRESCIA", 
                     "Sede Territoriale di Reggio Emilia" = "SEDE TERRITORIALE DI REGGIO EMILIA", 
                     
                     "Reparto Produzione Primaria" = "Reparto Produzione Primaria", 
                     "Reparto Chimico Alimenti (Bologna)" = "Reparto Chimico Alimenti (Bologna)", 
                     "Reparto Controllo Alimenti" = "Reparto Controllo Alimenti", 
                     "Reparto Chimica degli Alimenti e Mangimi" = "Reparto Chimica degli Alimenti e Mangimi",
                     
                     "Reparto Virologia" = "Reparto Virologia", 
                     "Reparto Virus Vescicolari e Produzioni Biotecnologiche" = "Reparto Virus Vescicolari e Produzioni Biotecnologiche", 
                     "Reparto Tecnologie Biologiche Applicate" = "Reparto Tecnologie Biologiche Applicate", 
                     "Reparto Produzione e Controllo Materiale Biologico" = "Reparto Produzione e Controllo Materiale Biologico", 
                     
                     "Analisi del Rischio ed Epidemiologia Genomica" = "Analisi del Rischio ed Epidemiologia Genomica", 
                     "Sorveglianza Epidemiologica" = "Sorveglianza Epidemiologica", 
                     "Gestione Centralizzata delle Richieste" = "Gestione Centralizzata delle Richieste", 
                     "Formazione e Biblioteca" =  "Formazione e Biblioteca", 
                     "Servizio Assicurazione Qualità" = "Servizio Assicurazione Qualità", 
                     "U.O. Affari Generali Legali" = "U.O. Affari Generali Legali", 
                     "U.O. Gestione Risorse Umane" = "U.O. Gestione Risorse Umane", 
                     "U.O. Tecnico Patrimoniale" = "U.O. Tecnico Patrimoniale", 
                     "U.O. Provveditorato Economato e Vendite" = "U.O. Provveditorato Economato e Vendite", 
                     "U.O. Gestione Servizi Contabili" = "U.O. Gestione Servizi Contabili"
    ), 
    
    
    dipartimento = recode(reparto, "SEDE TERRITORIALE DI BOLOGNA-MODENA-FERRARA" = "Area Territoriale Emilia Romagna", 
                          "SEDE TERRITORIALE DI PIACENZA-PARMA" = "Area Territoriale Emilia Romagna", 
                          "SEDE TERRITORIALE DI FORLI-RAVENNA" = "Area Territoriale Emilia Romagna", 
                          "SEDE TERRITORIALE DI REGGIO EMILIA" = "Area Territoriale Emilia Romagna", 
                          "SEDE TERRITORIALE DI BERGAMO-BINAGO-SONDRIO" = "Area Territoriale Lombardia", 
                          "SEDE TERRITORIALE DI LODI-MILANO" = "Area Territoriale Lombardia", 
                          "SEDE TERRITORIALE DI CREMONA-MANTOVA" = "Area Territoriale Lombardia", 
                          "SEDE TERRITORIALE DI PAVIA" = "Area Territoriale Lombardia", 
                          "SEDE TERRITORIALE DI BRESCIA" = "Area Territoriale Lombardia", 
                          "Reparto Produzione Primaria" = "Dipartimento Sicurezza Alimentare", 
                          "Reparto Chimico Alimenti (Bologna)" = "Dipartimento Sicurezza Alimentare", 
                          "Reparto Controllo Alimenti" = "Dipartimento Sicurezza Alimentare", 
                          "Reparto Chimica degli Alimenti e Mangimi" = "Dipartimento Sicurezza Alimentare", 
                          "Reparto Virologia" = "Dipartimento Tutela Salute Animale", 
                          "Reparto Virus Vescicolari e Produzioni Biotecnologiche" = "Dipartimento Tutela Salute Animale", 
                          "Reparto Tecnologie Biologiche Applicate" =  "Dipartimento Tutela Salute Animale", 
                          "Reparto Produzione e Controllo Materiale Biologico" = "Dipartimento Tutela Salute Animale", 
                          
                          "Analisi del Rischio ed Epidemiologia Genomica" = "Direzione Sanitaria", 
                          
                          "Sorveglianza Epidemiologica" = "Direzione Sanitaria", 
                          "Gestione Centralizzata delle Richieste" = "Direzione Sanitaria", 
                          "Formazione e Biblioteca" = "Direzione Sanitaria", 
                          "Servizio Assicurazione Qualità" = "Direzione Sanitaria", 
                          
                          "U.O. Affari Generali Legali" = "Dipartimento Amministrativo", 
                          "U.O. Gestione Risorse Umane" = "Dipartimento Amministrativo", 
                          "U.O. Tecnico Patrimoniale" = "Dipartimento Amministrativo", 
                          "U.O. Provveditorato Economato e Vendite" = "Dipartimento Amministrativo", 
                          "U.O. Gestione Servizi Contabili" = "Dipartimento Amministrativo"
    )
  )   



dtC <- obftec %>% 
  mutate(obcod = paste("OB", seq(1:nrow(.)))) %>% 
  pivot_longer(3:37, names_to = "struttura", values_to = "FTEC") %>% 
  mutate(
    struttura = recode(struttura, 
                       "STBO" = "Sede Territoriale di Bologna", 
                       "STFE" = "Sede Territoriale di Ferrara", 
                       "STMO" = "Sede Territoriale di Modena", 
                       "STPC" = "Sede Territoriale di Piacenza",
                       "STPR" = "Sede Territoriale di Parma",
                       "STFO" = "Sede Territoriale di Forlì",
                       "STRA" = "Sede Territoriale di Ravenna", 
                       "STBG" = "Sede Territoriale di Bergamo", 
                       "STBI" = "Sede Territoriale di Binago", 
                       "STSO" = "Sede Territoriale di Sondrio", 
                       "STLO" = "Sede Territoriale di Lodi", 
                       "STMI" = "Sede Territoriale di Milano", 
                       "STCR" = "Sede Territoriale di Cremona", 
                       "STMN" = "Sede Territoriale di Mantova", 
                       "STPV" = "Sede Territoriale di Pavia", 
                       "STBS" = "Sede Territoriale di Brescia", 
                       "STRE" = "Sede Territoriale di Reggio Emilia", 
                       
                       "RPP" = "Reparto Produzione Primaria", 
                       "RCABO" = "Reparto Chimico Alimenti (Bologna)", 
                       "RCA" = "Reparto Controllo Alimenti", 
                       "RCAM" = "Reparto Chimica degli Alimenti e Mangimi", 
                       
                       "RVIR" = "Reparto Virologia", 
                       "RVVPB" = "Reparto Virus Vescicolari e Produzioni Biotecnologiche", 
                       
                       "RTBA" = "Reparto Tecnologie Biologiche Applicate", 
                       "RPCMB" = "Reparto Produzione e Controllo Materiale Biologico", 
                       
                       "AREG" = "Analisi del Rischio ed Epidemiologia Genomica", 
                       "SORVEPIDEM" = "Sorveglianza Epidemiologica", 
                       "GESTCENT" = "Gestione Centralizzata delle Richieste", 
                       "FORMAZIONE" = "Formazione e Biblioteca", 
                       "SAQ" = "Servizio Assicurazione Qualità", 
                       "AFFGENLEG" = "U.O. Affari Generali Legali", 
                       "GESTRISUM" = "U.O. Gestione Risorse Umane", 
                       "UOTECPAT" = "U.O. Tecnico Patrimoniale", 
                       "PROVV" = "U.O. Provveditorato Economato e Vendite", 
                       "SERVCONT" = "U.O. Gestione Servizi Contabili"
                       
                       
    ), 
    
    reparto = recode(struttura, "Sede Territoriale di Bologna" = "SEDE TERRITORIALE DI BOLOGNA-MODENA-FERRARA", 
                     "Sede Territoriale di Ferrara" = "SEDE TERRITORIALE DI BOLOGNA-MODENA-FERRARA", 
                     "Sede Territoriale di Modena" = "SEDE TERRITORIALE DI BOLOGNA-MODENA-FERRARA", 
                     "Sede Territoriale di Piacenza" = "SEDE TERRITORIALE DI PIACENZA-PARMA", 
                     "Sede Territoriale di Parma" = "SEDE TERRITORIALE DI PIACENZA-PARMA",
                     "Sede Territoriale di Forlì" = "SEDE TERRITORIALE DI FORLI-RAVENNA", 
                     "Sede Territoriale di Ravenna" = "SEDE TERRITORIALE DI FORLI-RAVENNA", 
                     "Sede Territoriale di Bergamo" = "SEDE TERRITORIALE DI BERGAMO-BINAGO-SONDRIO", 
                     "Sede Territoriale di Binago" = "SEDE TERRITORIALE DI BERGAMO-BINAGO-SONDRIO", 
                     "Sede Territoriale di Sondrio" = "SEDE TERRITORIALE DI BERGAMO-BINAGO-SONDRIO", 
                     "Sede Territoriale di Lodi" = "SEDE TERRITORIALE DI LODI-MILANO", 
                     "Sede Territoriale di Milano" = "SEDE TERRITORIALE DI LODI-MILANO", 
                     "Sede Territoriale di Cremona" = "SEDE TERRITORIALE DI CREMONA-MANTOVA", 
                     "Sede Territoriale di Mantova" = "SEDE TERRITORIALE DI CREMONA-MANTOVA", 
                     "Sede Territoriale di Pavia" = "SEDE TERRITORIALE DI PAVIA", 
                     "Sede Territoriale di Brescia" = "SEDE TERRITORIALE DI BRESCIA", 
                     "Sede Territoriale di Reggio Emilia" = "SEDE TERRITORIALE DI REGGIO EMILIA", 
                     
                     "Reparto Produzione Primaria" = "Reparto Produzione Primaria", 
                     "Reparto Chimico Alimenti (Bologna)" = "Reparto Chimico Alimenti (Bologna)", 
                     "Reparto Controllo Alimenti" = "Reparto Controllo Alimenti", 
                     "Reparto Chimica degli Alimenti e Mangimi" = "Reparto Chimica degli Alimenti e Mangimi",
                     
                     "Reparto Virologia" = "Reparto Virologia", 
                     "Reparto Virus Vescicolari e Produzioni Biotecnologiche" = "Reparto Virus Vescicolari e Produzioni Biotecnologiche", 
                     "Reparto Tecnologie Biologiche Applicate" = "Reparto Tecnologie Biologiche Applicate", 
                     "Reparto Produzione e Controllo Materiale Biologico" = "Reparto Produzione e Controllo Materiale Biologico", 
                     
                     "Analisi del Rischio ed Epidemiologia Genomica" = "Analisi del Rischio ed Epidemiologia Genomica", 
                     "Sorveglianza Epidemiologica" = "Sorveglianza Epidemiologica", 
                     "Gestione Centralizzata delle Richieste" = "Gestione Centralizzata delle Richieste", 
                     "Formazione e Biblioteca" =  "Formazione e Biblioteca", 
                     "Servizio Assicurazione Qualità" = "Servizio Assicurazione Qualità", 
                     "U.O. Affari Generali Legali" = "U.O. Affari Generali Legali", 
                     "U.O. Gestione Risorse Umane" = "U.O. Gestione Risorse Umane", 
                     "U.O. Tecnico Patrimoniale" = "U.O. Tecnico Patrimoniale", 
                     "U.O. Provveditorato Economato e Vendite" = "U.O. Provveditorato Economato e Vendite", 
                     "U.O. Gestione Servizi Contabili" = "U.O. Gestione Servizi Contabili"
    ), 
    
    
    dipartimento = recode(reparto, "SEDE TERRITORIALE DI BOLOGNA-MODENA-FERRARA" = "Area Territoriale Emilia Romagna", 
                          "SEDE TERRITORIALE DI PIACENZA-PARMA" = "Area Territoriale Emilia Romagna", 
                          "SEDE TERRITORIALE DI FORLI-RAVENNA" = "Area Territoriale Emilia Romagna", 
                          "SEDE TERRITORIALE DI REGGIO EMILIA" = "Area Territoriale Emilia Romagna", 
                          "SEDE TERRITORIALE DI BERGAMO-BINAGO-SONDRIO" = "Area Territoriale Lombardia", 
                          "SEDE TERRITORIALE DI LODI-MILANO" = "Area Territoriale Lombardia", 
                          "SEDE TERRITORIALE DI CREMONA-MANTOVA" = "Area Territoriale Lombardia", 
                          "SEDE TERRITORIALE DI PAVIA" = "Area Territoriale Lombardia", 
                          "SEDE TERRITORIALE DI BRESCIA" = "Area Territoriale Lombardia", 
                          "Reparto Produzione Primaria" = "Dipartimento Sicurezza Alimentare", 
                          "Reparto Chimico Alimenti (Bologna)" = "Dipartimento Sicurezza Alimentare", 
                          "Reparto Controllo Alimenti" = "Dipartimento Sicurezza Alimentare", 
                          "Reparto Chimica degli Alimenti e Mangimi" = "Dipartimento Sicurezza Alimentare", 
                          "Reparto Virologia" = "Dipartimento Tutela Salute Animale", 
                          "Reparto Virus Vescicolari e Produzioni Biotecnologiche" = "Dipartimento Tutela Salute Animale", 
                          "Reparto Tecnologie Biologiche Applicate" =  "Dipartimento Tutela Salute Animale", 
                          "Reparto Produzione e Controllo Materiale Biologico" = "Dipartimento Tutela Salute Animale", 
                          
                          "Analisi del Rischio ed Epidemiologia Genomica" = "Direzione Sanitaria", 
                          
                          "Sorveglianza Epidemiologica" = "Direzione Sanitaria", 
                          "Gestione Centralizzata delle Richieste" = "Direzione Sanitaria", 
                          "Formazione e Biblioteca" = "Direzione Sanitaria", 
                          "Servizio Assicurazione Qualità" = "Direzione Sanitaria", 
                          
                          "U.O. Affari Generali Legali" = "Dipartimento Amministrativo", 
                          "U.O. Gestione Risorse Umane" = "Dipartimento Amministrativo", 
                          "U.O. Tecnico Patrimoniale" = "Dipartimento Amministrativo", 
                          "U.O. Provveditorato Economato e Vendite" = "Dipartimento Amministrativo", 
                          "U.O. Gestione Servizi Contabili" = "Dipartimento Amministrativo"
    )
  )   

dtD %>% 
  right_join(dtC,  by = c( "dipartimento", "reparto", "struttura",   "obcod"))%>% 
  select(obcod, "Obiettivo" = Obiettivo.x, "Valorizzazione"= Valorizzazione.x, "Dipartimento"=dipartimento, 
         "Reparto" = reparto, "Struttura"= struttura, FTED, FTEC ) %>% 
  mutate(Dipartimento = factor(Dipartimento, levels = c("Direzione Sanitaria", 
                                                        "Dipartimento Amministrativo", 
                                                        "Dipartimento Sicurezza Alimentare", 
                                                        "Dipartimento Tutela Salute Animale", 
                                                        "Area Territoriale Lombardia", 
                                                        "Area Territoriale Emilia Romagna"))) %>%  
  saveRDS(., file = here("data",  "processed",  "datiSB.rds"))

