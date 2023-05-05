dtmensili %>%
  select(Dipartimento, Anno, MESE, RT, FTET,FTp, RFTE, RFTEc) %>%  
  filter(Dipartimento == "DIPARTIMENTO TUTELA E SALUTE ANIMALE" & Anno == 2022)
