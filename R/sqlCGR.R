queryAcc <- ("SELECT DISTINCT
                         dbo.Conferimenti.Nome_Stazione_Inserimento, dbo.Conferimenti.Matr_Ins, dbo.Conferimenti.Anno, dbo.Conferimenti.Numero AS Conferimento, dbo.Conferimenti.NrCampioni, dbo.Anag_TipoConf.Descrizione,
                         dbo.Anag_Settori_Attività_Form.Descrizione AS settoreintervento, dbo.Anag_Settori_Attività_Form.Codice, dbo.Anag_Reparti.Descrizione AS strutturaaccettante, dbo.Anag_Reparti.Codice AS strutturaaccettante_codice,
                         dbo.Anag_Reparti.Locazione AS Locazione_strutturaaccettante, dbo.Conferimenti.Consegnato_Portineria, CONVERT(VARCHAR(10), dbo.Conferimenti.Data_Accettazione, 111) AS Data_Registrazione, CONVERT(VARCHAR(10),
                         dbo.Conferimenti.Data, 111) AS Data_Ricezione, CONVERT(VARCHAR(10), dbo.Conferimenti.Data_Prelievo, 111) AS Data_Prelievo, dbo.Anag_Tipo_Prel.Descrizione AS tipodiprelievo,
                         dbo.Anag_Finalita.Descrizione AS FinalitaConferimento, dbo.Conferimenti.Temperatura_Campione_Sentinella_SiNo AS Possibile_Rilevare_T, dbo.Conferimenti.Temperatura_Campione_Sentinella AS T,
                         dbo.Conferimenti.Temperatura_Campione_Sentinella_Fuori_Range AS Fuori_Range, dbo.Conferimenti.Stato_Campione, Anag_Referenti2.Ragione_Sociale AS Conferente,
                         dbo.Anag_Referenti.Ragione_Sociale AS denominazazienda, dbo.Anag_Referenti.Indirizzo AS indirizzo, dbo.Anag_Comuni.Descrizione AS comune, dbo.Anag_Comuni.Provincia AS provincia,
                         dbo.Conferimenti.Allevix_Proprietario AS codiceazienda, dbo.Anag_Referenti.Numero_Codice_Allevix AS Sottocodice, dbo.Conferimenti.Riferimenti AS Verbale, Anag_Referenti1.Ragione_Sociale AS Veterinario,
                         Anag_Referenti3.Ragione_Sociale AS Dest_fattura, Anag_Referenti4.Ragione_Sociale AS Dest_Rapporto_Prova, dbo.Anag_Specie.Descrizione AS specie, dbo.Anag_Materiali.Descrizione AS Materiale,
                         dbo.Anag_Matrici.Descrizione AS Matrice, dbo.Conferimenti.Smaltimento_Carcasse, dbo.Anag_Comuni.Regione AS regione, dbo.Anag_Asl.Descrizione AS ASL, dbo.Anag_Asl.CodiceMinistero,
                         dbo.Anag_Asl.Cod_ISTAT AS CodiceASL, dbo.Conferimenti.Conferimento_Chiuso, dbo.Anag_Prove.Codice AS CodiceProva, dbo.Anag_Prove.Descrizione AS Prova, dbo.Anag_Gruppo_Prove.Descrizione AS gProva
FROM            dbo.Anag_Gruppo_Prove INNER JOIN
                         dbo.Nomenclatore ON dbo.Anag_Gruppo_Prove.Codice = dbo.Nomenclatore.Codice_Gruppo RIGHT OUTER JOIN
                         dbo.Conferimenti LEFT OUTER JOIN
                         dbo.Conferimenti_Finalita ON dbo.Conferimenti.Anno = dbo.Conferimenti_Finalita.Anno AND dbo.Conferimenti.Numero = dbo.Conferimenti_Finalita.Numero LEFT OUTER JOIN
                         dbo.Programmazione ON dbo.Conferimenti.Anno = dbo.Programmazione.Anno_Conferimento AND dbo.Conferimenti.Numero = dbo.Programmazione.Numero_Conferimento LEFT OUTER JOIN
                         dbo.Nomenclatore_MP ON dbo.Programmazione.Nomenclatore = dbo.Nomenclatore_MP.Codice LEFT OUTER JOIN
                         dbo.Nomenclatore_Settori ON dbo.Nomenclatore_MP.Nomenclatore_Settore = dbo.Nomenclatore_Settori.Codice ON dbo.Nomenclatore.Chiave = dbo.Nomenclatore_Settori.Codice_Nomenclatore LEFT OUTER JOIN
                         dbo.Anag_Prove ON dbo.Nomenclatore.Codice_Prova = dbo.Anag_Prove.Codice LEFT OUTER JOIN
                         dbo.Anag_Tecniche ON dbo.Nomenclatore.Codice_Tecnica = dbo.Anag_Tecniche.Codice LEFT OUTER JOIN
                         dbo.Laboratori_Reparto ON dbo.Programmazione.RepLab_analisi = dbo.Laboratori_Reparto.Chiave LEFT OUTER JOIN
                         dbo.Anag_Reparti ON dbo.Laboratori_Reparto.Reparto = dbo.Anag_Reparti.Codice LEFT OUTER JOIN
                         dbo.Anag_Laboratori ON dbo.Laboratori_Reparto.Laboratorio = dbo.Anag_Laboratori.Codice LEFT OUTER JOIN
                         dbo.Anag_Specie ON dbo.Conferimenti.Codice_Specie = dbo.Anag_Specie.Codice LEFT OUTER JOIN
                         dbo.Risultati_Analisi ON dbo.Programmazione.Anno_Conferimento = dbo.Risultati_Analisi.Anno_Conferimento AND dbo.Programmazione.Numero_Conferimento = dbo.Risultati_Analisi.Numero_Conferimento AND
                         dbo.Programmazione.Codice = dbo.Risultati_Analisi.Codice LEFT OUTER JOIN
                         dbo.Anag_Esiti ON dbo.Risultati_Analisi.Esito = dbo.Anag_Esiti.Codice LEFT OUTER JOIN
                         dbo.Anag_Referenti ON dbo.Conferimenti.Proprietario = dbo.Anag_Referenti.Codice LEFT OUTER JOIN
                         dbo.Anag_Referenti AS Anag_Referenti1 ON dbo.Conferimenti.Veterinario = Anag_Referenti1.Codice LEFT OUTER JOIN
                         dbo.Anag_Referenti AS Anag_Referenti2 ON dbo.Conferimenti.Conferente = Anag_Referenti2.Codice LEFT OUTER JOIN
                         dbo.Anag_Referenti AS Anag_Referenti3 ON dbo.Conferimenti.Dest_Fattura = Anag_Referenti3.Codice LEFT OUTER JOIN
                         dbo.Anag_Referenti AS Anag_Referenti4 ON dbo.Conferimenti.Dest_Rapporto_Prova = Anag_Referenti4.Codice LEFT OUTER JOIN
                         dbo.Anag_Comuni ON dbo.Anag_Referenti.Comune = dbo.Anag_Comuni.Codice LEFT OUTER JOIN
                         dbo.Nomenclatore_Range ON dbo.Risultati_Analisi.Range = dbo.Nomenclatore_Range.Codice LEFT OUTER JOIN
                         dbo.Anag_TipoConf ON dbo.Conferimenti.Tipo = dbo.Anag_TipoConf.Codice LEFT OUTER JOIN
                         dbo.Anag_Materiali ON dbo.Conferimenti.Codice_Materiale = dbo.Anag_Materiali.Codice LEFT OUTER JOIN
                         dbo.Anag_Asl ON dbo.Conferimenti.Cod_Asl = dbo.Anag_Asl.codice LEFT OUTER JOIN
                         dbo.Anag_Matrici ON dbo.Conferimenti.Matrice = dbo.Anag_Matrici.Codice LEFT OUTER JOIN
                         dbo.Anag_Finalita ON dbo.Conferimenti_Finalita.Finalita = dbo.Anag_Finalita.Codice LEFT OUTER JOIN
                         dbo.Anag_Tipo_Prel ON dbo.Conferimenti.Tipo_Prelievo = dbo.Anag_Tipo_Prel.Codice LEFT OUTER JOIN
                         dbo.Anag_Settori_Attività_Form ON dbo.Conferimenti.Registro = dbo.Anag_Settori_Attività_Form.Codice
WHERE        (dbo.Conferimenti.Anno >= 2021) AND (dbo.Conferimenti.Conferimento_Chiuso = 1) AND (dbo.Conferimenti.Nome_Stazione_Inserimento = 'ACC-CENTR2' OR
                         dbo.Conferimenti.Nome_Stazione_Inserimento = 'PC-47326' OR
                         dbo.Conferimenti.Nome_Stazione_Inserimento = 'PC-40780' OR
                         dbo.Conferimenti.Nome_Stazione_Inserimento = 'MP-ACC3' OR
                         dbo.Conferimenti.Nome_Stazione_Inserimento = 'BS-ASS-N' OR
                         dbo.Conferimenti.Nome_Stazione_Inserimento = 'PC-47327' OR
                         dbo.Conferimenti.Nome_Stazione_Inserimento = 'CH-ACC4-N' OR
                         dbo.Conferimenti.Nome_Stazione_Inserimento = 'CH-ACC2-N' OR
                         dbo.Conferimenti.Nome_Stazione_Inserimento = 'PC-49676' OR
                         dbo.Conferimenti.Nome_Stazione_Inserimento = 'PC-47499' OR
                         dbo.Conferimenti.Nome_Stazione_Inserimento = 'PC-49694' OR
                         dbo.Conferimenti.Nome_Stazione_Inserimento = 'OEVR-8-PORT' OR
                         dbo.Conferimenti.Nome_Stazione_Inserimento = 'PC-50901' OR
                         dbo.Conferimenti.Nome_Stazione_Inserimento = 'NB-50619' OR
                         dbo.Conferimenti.Nome_Stazione_Inserimento = 'NB-50696' OR
                         dbo.Conferimenti.Nome_Stazione_Inserimento = 'MP-SIVARS7-N' OR
                         dbo.Conferimenti.Nome_Stazione_Inserimento = 'PC-49702' OR
                         dbo.Conferimenti.Nome_Stazione_Inserimento = 'PC-49703' OR
                         dbo.Conferimenti.Nome_Stazione_Inserimento = 'PC-47929')")




