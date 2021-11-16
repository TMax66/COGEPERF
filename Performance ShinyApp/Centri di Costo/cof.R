
x <- dtanalisi %>% filter(CDC =="SEDE TERRITORIALE DI BOLOGNA" & Costi=="Ricavo") %>% 
                     group_by(CDC,  ANNO,  Quarter) %>% 
                     summarise(N.Esami = sum(Determinazioni, na.rm = TRUE), 
                               EUff = sum(AttUff, na.rm = TRUE), 
                               ENUff =sum(AttNUff, na.rm = TRUE), 
                               EPag = sum(AttPag, na.rm = TRUE), 
                               Egrat = sum(AttGrat, na.rm = TRUE), 
                               PI = sum(AI , na.rm = TRUE), 
                               Prodv = sum(VP, na.rm = TRUE)) %>% 
                     ungroup %>% 
filter(ANNO <= 2020) %>% 
  group_by(Quarter) %>% 
  mutate(variazione = (N.Esami[ANNO == 2020] - N.Esami[ANNO == 2019]) / N.Esami[ANNO == 2019] * 100)
  
p1 <- ggplot(x,aes(x=ANNO))+
  geom_ribbon(aes(ymin = 0, ymax = N.Esami), alpha=0.35)+
  geom_point(aes(y = N.Esami)) +
  facet_wrap(facets = ~Quarter, nrow=1)+
  scale_x_continuous(breaks = unique(x$ANNO), expand=c(0.16, 0))+
  geom_text(data = dplyr::filter(x, ANNO == 2020), aes(label = sprintf('%+0.1f%%', variazione)), 
            x = 2019.5, y = 0, vjust = -1, fontface = 'bold', size=5)+
  geom_text(aes(label = sprintf('%0.1f',N.Esami), y = N.Esami), vjust = -1, size=3)+
  theme_bw()
  
  


 




variazione = (x$N.Esami[x$ANNO == 2020] - x$N.Esami[x$ANNO == 2019]) / x$N.Esami[x$ANNO == 2019] * 100


xx <- ddply(x, .(ANNO, Quarter), mutate, 
                    is.increasing = N.Esami[ANNO == 2020] >= N.Esami[ANNO == 2019],
                    percent = (N.Esami[ANNO == 2020] - N.Esami[ANNO == 2019]) / N.Esami[ANNO == 2019] * 100
)







                    
                     mutate(VarEsami = round((N.Esami/lag(N.Esami)-1)*100, 2), 
                            VarEUff = round((EUff/lag(EUff)-1)*100, 2), 
                            VarENUff = round((ENUff/lag(ENUff)-1)*100, 2), 
                            VarEPag = round((EPag/lag(EPag)-1)*100, 2),
                            VarEgrat = round((Egrat/lag(Egrat)-1)*100, 2),
                            VarPI = round((PI/lag(PI)-1)*100, 2), 
                            VarProdv = round((Prodv/lag(Prodv)-1)*100, 2)
                     ) %>%  View()
  