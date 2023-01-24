queryAcc <- ("SELECT DISTINCT 
                         Conferimenti.Nome_Stazione_Inserimento, Conferimenti.Matr_Ins, Conferimenti.Anno, Conferimenti.Numero AS Conferimento, Conferimenti.NrCampioni, Anag_TipoConf.Descrizione, 
                         Anag_Settori_Attività_Form.Descrizione AS settoreintervento, Anag_Settori_Attività_Form.Codice, Anag_Reparti.Descrizione AS repanalisi, 
                         Anag_Reparti.Codice AS strutturaaccettante_codice, Conferimenti.Consegnato_Portineria, CONVERT(VARCHAR(10), Conferimenti.Data_Accettazione, 111) AS Data_Registrazione, 
                         CONVERT(VARCHAR(10), Conferimenti.Data, 111) AS Data_Ricezione, CONVERT(VARCHAR(10), Conferimenti.Data_Prelievo, 111) AS Data_Prelievo, 
                         Anag_Tipo_Prel.Descrizione AS tipodiprelievo, Anag_Finalita.Descrizione AS FinalitaConferimento, dbo.Conferimenti.Temperatura_Campione_Sentinella_SiNo AS Possibile_Rilevare_T, 
                         Conferimenti.Temperatura_Campione_Sentinella AS T, dbo.Conferimenti.Temperatura_Campione_Sentinella_Fuori_Range AS Fuori_Range, dbo.Conferimenti.Stato_Campione, 
                         Anag_Referenti2.Ragione_Sociale AS Conferente, Anag_Referenti.Ragione_Sociale AS denominazazienda, Anag_Referenti.Indirizzo AS indirizzo, Anag_Comuni.Descrizione AS comune, 
                         Anag_Comuni.Provincia AS provincia, Conferimenti.Allevix_Proprietario AS codiceazienda, Anag_Referenti.Numero_Codice_Allevix AS Sottocodice, Conferimenti.Riferimenti AS Verbale, 
                         Anag_Referenti1.Ragione_Sociale AS Veterinario, Anag_Referenti3.Ragione_Sociale AS Dest_fattura, Anag_Referenti4.Ragione_Sociale AS Dest_Rapporto_Prova, 
                         Anag_Specie.Descrizione AS specie, Anag_Materiali.Descrizione AS Materiale, Anag_Matrici.Descrizione AS Matrice, Conferimenti.Smaltimento_Carcasse, 
                         Anag_Comuni.Regione AS regione, Anag_Asl.Descrizione AS ASL, Anag_Asl.CodiceMinistero, Anag_Asl.Cod_ISTAT AS CodiceASL, Conferimenti.Conferimento_Chiuso, 
                         Anag_Prove.Codice AS CodiceProva, Anag_Prove.Descrizione AS Prova, Anag_Gruppo_Prove.Descrizione AS gProva, Anag_Laboratori.Descrizione AS labanalisi, 
                         Laboratori_Reparto.Chiave, Anag_Laboratori_1.Descrizione AS labacc, Anag_Reparti_1.Descrizione AS strutturaaccettante, Anag_Reparti_1.Locazione
FROM         Anag_Esiti RIGHT OUTER JOIN
                         Risultati_Analisi RIGHT OUTER JOIN
                         Anag_Tecniche RIGHT OUTER JOIN
                         Anag_Prove RIGHT OUTER JOIN
                         Anag_Laboratori RIGHT OUTER JOIN
                         Anag_Settori_Attività_Form RIGHT OUTER JOIN
                         Anag_Finalita RIGHT OUTER JOIN
                         Anag_Comuni RIGHT OUTER JOIN
                         Programmazione INNER JOIN
                         Laboratori_Reparto AS Laboratori_Reparto_1 ON Programmazione.RepLab_analisi = Laboratori_Reparto_1.Chiave RIGHT OUTER JOIN
                         Laboratori_Reparto INNER JOIN
                         Conferimenti ON Laboratori_Reparto.Chiave = Conferimenti.RepLab_Conferente LEFT OUTER JOIN
                         Anag_Laboratori AS Anag_Laboratori_1 ON Laboratori_Reparto.Laboratorio = Anag_Laboratori_1.Codice ON Programmazione.Anno_Conferimento = Conferimenti.Anno AND 
                         Programmazione.Numero_Conferimento = Conferimenti.Numero LEFT OUTER JOIN
                         Anag_Reparti AS Anag_Reparti_1 ON Laboratori_Reparto.Reparto = Anag_Reparti_1.Codice LEFT OUTER JOIN
                         Anag_Referenti AS Anag_Referenti3 ON Conferimenti.Dest_Fattura = Anag_Referenti3.Codice LEFT OUTER JOIN
                         Anag_Referenti AS Anag_Referenti2 ON Conferimenti.Conferente = Anag_Referenti2.Codice LEFT OUTER JOIN
                         Anag_Referenti ON Conferimenti.Proprietario = Anag_Referenti.Codice LEFT OUTER JOIN
                         Anag_Referenti AS Anag_Referenti1 ON Conferimenti.Veterinario = Anag_Referenti1.Codice LEFT OUTER JOIN
                         Anag_Referenti AS Anag_Referenti4 ON Conferimenti.Dest_Rapporto_Prova = Anag_Referenti4.Codice ON Anag_Comuni.Codice = Anag_Referenti.Comune LEFT OUTER JOIN
                         Conferimenti_Finalita ON Conferimenti.Anno = Conferimenti_Finalita.Anno AND Conferimenti.Numero = Conferimenti_Finalita.Numero LEFT OUTER JOIN
                         Anag_Specie ON Conferimenti.Codice_Specie = Anag_Specie.Codice LEFT OUTER JOIN
                         Anag_TipoConf ON Conferimenti.Tipo = Anag_TipoConf.Codice LEFT OUTER JOIN
                         Anag_Materiali ON Conferimenti.Codice_Materiale = Anag_Materiali.Codice LEFT OUTER JOIN
                         Anag_Asl ON Conferimenti.Cod_Asl = Anag_Asl.codice LEFT OUTER JOIN
                         Anag_Matrici ON Conferimenti.Matrice = Anag_Matrici.Codice ON Anag_Finalita.Codice = Conferimenti_Finalita.Finalita LEFT OUTER JOIN
                         Anag_Tipo_Prel ON Conferimenti.Tipo_Prelievo = Anag_Tipo_Prel.Codice ON Anag_Settori_Attività_Form.Codice = Conferimenti.Registro LEFT OUTER JOIN
                         Anag_Reparti ON Laboratori_Reparto_1.Reparto = Anag_Reparti.Codice ON Anag_Laboratori.Codice = Laboratori_Reparto_1.Laboratorio LEFT OUTER JOIN
                         Nomenclatore_MP ON Programmazione.Nomenclatore = Nomenclatore_MP.Codice LEFT OUTER JOIN
                         Nomenclatore_Settori ON Nomenclatore_MP.Nomenclatore_Settore = Nomenclatore_Settori.Codice LEFT OUTER JOIN
                         Anag_Gruppo_Prove INNER JOIN
                         Nomenclatore ON Anag_Gruppo_Prove.Codice = Nomenclatore.Codice_Gruppo ON Nomenclatore_Settori.Codice_Nomenclatore = Nomenclatore.Chiave ON 
                         Anag_Prove.Codice = Nomenclatore.Codice_Prova ON Anag_Tecniche.Codice = Nomenclatore.Codice_Tecnica ON 
                         Risultati_Analisi.Anno_Conferimento = Programmazione.Anno_Conferimento AND Risultati_Analisi.Numero_Conferimento = Programmazione.Numero_Conferimento AND 
                         Risultati_Analisi.Codice = Programmazione.Codice ON Anag_Esiti.Codice = Risultati_Analisi.Esito LEFT OUTER JOIN
                         Nomenclatore_Range ON Risultati_Analisi.Range = Nomenclatore_Range.Codice
WHERE     (Conferimenti.Anno >= 2021) AND (Conferimenti.Conferimento_Chiuso = 1) AND (Conferimenti.Nome_Stazione_Inserimento = 'ACC-CENTR2' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'PC-47326' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'PC-40780' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'MP-ACC3' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'BS-ASS-N' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'PC-47327' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'CH-ACC4-N' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'CH-ACC2-N' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'PC-49676' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'PC-47499' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'PC-49694' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'OEVR-8-PORT' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'PC-50901' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'NB-50619' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'NB-50696' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'MP-SIVARS7-N' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'PC-49702' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'PC-49703' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'PC-47929' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'PC-51010' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'PC-50957' OR
                         Conferimenti.Nome_Stazione_Inserimento = 'PC-46945')")



queryAtt <- ("SELECT DISTINCT 
                         Gest_Altre_Attivita.Anno, Gest_Altre_Attivita.Codice, CONVERT(VARCHAR(10), Gest_Altre_Attivita.data, 111) AS Data_Attività, Gest_Altre_Attivita.Struttura AS CodiceRepartoAiitivtà, 
                         Anag_Reparti.Descrizione AS Reparto_Attività, Anag_Settori_Attività_Form.Descrizione AS SettoreIntervento, Anag_TipoConf.Descrizione AS TipoAttività, 
                         Anag_Altre_Attività.Descrizione AS TipoPrestazione, Anag_Referenti.Ragione_Sociale AS Proprietario, Anag_Comuni.Descrizione AS Comune, Gest_Altre_Attivita.quantita, 
                         dbo.Gest_Altre_Attivita.Note
FROM         Anag_TipoConf LEFT OUTER JOIN
                         Anag_Reparti RIGHT OUTER JOIN
                         Gest_Altre_Attivita_Matricole RIGHT OUTER JOIN
                         Gest_Altre_Attivita ON Gest_Altre_Attivita_Matricole.Anno = Gest_Altre_Attivita.Anno AND Gest_Altre_Attivita_Matricole.codice = Gest_Altre_Attivita.Codice LEFT OUTER JOIN
                         Laboratori_Reparto ON Gest_Altre_Attivita.Struttura = Laboratori_Reparto.Chiave LEFT OUTER JOIN
                         Anag_Laboratori ON Laboratori_Reparto.Laboratorio = Anag_Laboratori.Codice ON Anag_Reparti.Codice = Laboratori_Reparto.Reparto ON 
                         Anag_TipoConf.Codice = Gest_Altre_Attivita.Tipo_Pagamento LEFT OUTER JOIN
                         Anag_Referenti ON Gest_Altre_Attivita.Contatto_Visitato = Anag_Referenti.Codice LEFT OUTER JOIN
                         Anag_Comuni ON Gest_Altre_Attivita.Localita = Anag_Comuni.Codice LEFT OUTER JOIN
                         Anag_Settori_Attività_Form ON Gest_Altre_Attivita.Settore = Anag_Settori_Attività_Form.Codice LEFT OUTER JOIN
                         Anag_Altre_Attività ON Gest_Altre_Attivita.nomenclatore = Anag_Altre_Attività.codice LEFT OUTER JOIN
                         Operatori_di_sistema ON Gest_Altre_Attivita_Matricole.matricola = Operatori_di_sistema.Ident_Operatore
WHERE     (Gest_Altre_Attivita.Anno = 2022) AND (Gest_Altre_Attivita.Struttura = 285)")



