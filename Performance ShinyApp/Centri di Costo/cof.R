
# df <- dtanalisi %>% filter(CDC =="SEDE TERRITORIALE DI BOLOGNA" & Costi=="Ricavo") %>% 
#                      group_by(CDC,  ANNO,  Quarter) %>% 
#                      summarise(N.Esami = sum(Determinazioni, na.rm = TRUE), 
#                                EUff = sum(AttUff, na.rm = TRUE), 
#                                ENUff =sum(AttNUff, na.rm = TRUE), 
#                                EPag = sum(AttPag, na.rm = TRUE), 
#                                Egrat = sum(AttGrat, na.rm = TRUE), 
#                                PI = sum(AI , na.rm = TRUE), 
#                                Prodv = sum(VP, na.rm = TRUE)) %>% 
#                      ungroup %>% 
#   arrange(Quarter) %>%  
#   mutate(VarEsami = round((N.Esami/lag(N.Esami)-1)*100, 2), 
#          VarEUff = round((EUff/lag(EUff)-1)*100, 2), 
#          VarENUff = round((ENUff/lag(ENUff)-1)*100, 2), 
#          VarEPag = round((EPag/lag(EPag)-1)*100, 2),
#          VarEgrat = round((Egrat/lag(Egrat)-1)*100, 2),
#          VarPI = round((PI/lag(PI)-1)*100, 2), 
#          VarProdv = round((Prodv/lag(Prodv)-1)*100, 2)
#   ) %>% 
#   filter(ANNO <=2020)


  
  
  
  
 # filter(ANNO <= 2020) %>% 
 #  group_by(Quarter) %>% 
 #  mutate(variazione = (N.Esami[ANNO == 2020] - N.Esami[ANNO == 2019]) / N.Esami[ANNO == 2019] * 100)  



  
# df2 <- dtanalisi %>% filter(CDC =="SEDE TERRITORIALE DI BOLOGNA" & Costi=="Ricavo") %>% 
#   group_by(CDC,  ANNO,  Quarter) %>% 
#   summarise(N.Esami = sum(Determinazioni, na.rm = TRUE), 
#             EUff = sum(AttUff, na.rm = TRUE), 
#             ENUff =sum(AttNUff, na.rm = TRUE), 
#             EPag = sum(AttPag, na.rm = TRUE), 
#             Egrat = sum(AttGrat, na.rm = TRUE), 
#             PI = sum(AI , na.rm = TRUE), 
#             Prodv = sum(VP, na.rm = TRUE)) %>% 
#   ungroup %>%  
#   #filter(ANNO >2019) %>% 
#   group_by(Quarter) %>%   View()
#   mutate(variazione = (N.Esami[ANNO == 2021] - N.Esami[ANNO == 2020]) / N.Esami[ANNO == 2020] * 100)



# ggplot(df)+
#  aes( x = .data[["ANNO"]])+
#   geom_ribbon(aes(ymin = 0, ymax = .data[["N.Esami"]]), alpha=0.35)+
#   geom_point(aes(y = .data[["N.Esami"]])) +
#   facet_wrap(facets = ~Quarter, nrow=1)+
#   scale_x_continuous(breaks = unique(x$ANNO), expand=c(0.16, 0))+
#   geom_text(data = dplyr::filter(df, ANNO == 2020), aes(label = sprintf('%+0.1f%%', VarEsami)), 
#             x = 2019.5, y = 0, vjust = -1, fontface = 'bold', size=5)+
#   geom_text(aes(label = sprintf('%0.1f',N.Esami), y = N.Esami), vjust = -1, size=3)+
#   theme_bw()
#   

df2 <- dtanalisi %>% filter(CDC =="SEDE TERRITORIALE DI BOLOGNA" & Costi=="Ricavo") %>% 
  group_by(CDC,  ANNO,  Quarter) %>% 
  summarise(N.Esami = sum(Determinazioni, na.rm = TRUE), 
            EUff = sum(AttUff, na.rm = TRUE), 
            ENUff =sum(AttNUff, na.rm = TRUE), 
            EPag = sum(AttPag, na.rm = TRUE), 
            Egrat = sum(AttGrat, na.rm = TRUE), 
            PI = sum(AI , na.rm = TRUE), 
            Prodv = sum(VP, na.rm = TRUE)) %>% 
  ungroup %>% 
  arrange(Quarter) %>%  
  mutate(VarEsami = round((N.Esami/lag(N.Esami)-1)*100, 2), 
         VarEUff = round((EUff/lag(EUff)-1)*100, 2), 
         VarENUff = round((ENUff/lag(ENUff)-1)*100, 2), 
         VarEPag = round((EPag/lag(EPag)-1)*100, 2),
         VarEgrat = round((Egrat/lag(Egrat)-1)*100, 2),
         VarPI = round((PI/lag(PI)-1)*100, 2), 
         VarProdv = round((Prodv/lag(Prodv)-1)*100, 2)
  ) 

  

ggplot(dtT)+
  aes( x = .data[["ANNO"]])+
  geom_ribbon(aes(ymin = 0, ymax = .data[["TotRic"]]), alpha=0.05)+
  geom_point(aes(y = .data[["TotRic"]])) +
  geom_line(aes(y = .data[["TotRic"]]))+
  facet_wrap(facets = ~Quarter, nrow=1)+
  scale_x_continuous(breaks = unique(dtT$ANNO), expand=c(0.16, 0))+
  geom_text(data = dplyr::filter(dtT, ANNO == 2020), aes(label = sprintf('%+0.1f%%', VarTot)), 
            x = 2019.5, y = 0, vjust = -1, fontface = 'bold', size=5)+
  geom_text(data = dplyr::filter(dtT, ANNO == 2021), aes(label = sprintf('%+0.1f%%', VarTot)), 
            x = 2020.5, y = 0, vjust = -1, fontface = 'bold', size=5)+
  geom_text(aes(label = sprintf('%0.1f',TotRic), y = TotRic), vjust = -1, size=3.5)+
  labs(y = "", x = " ",
       title = "")+
  theme_ipsum_pub()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 15),
        
        strip.text.x = element_text(size = 15))



Tplot <- function(df, y_par, y_par2)
{    
  ggplot(df)+ 
    aes(
      y = .data[[y_par]],
      x = .data[["ANNO"]])+  
      
      geom_ribbon(aes(ymin = 0, ymax = .data[[y_par]]), alpha=0.05)+
        geom_point(aes(y = .data[[y_par]])) +
        geom_line(aes(y = .data[[y_par]]))+
        facet_wrap(facets = ~Quarter, nrow=1)+
        scale_x_continuous(breaks = unique(df2$ANNO), expand=c(0.16, 0))+
        geom_text(data = dplyr::filter(df, ANNO == 2020), aes(label = sprintf('%+0.1f%%',.data[[y_par2]])), 
                  x = 2019.5, y = 0, vjust = -1, fontface = 'bold', size=5)+
        geom_text(data = dplyr::filter(df, ANNO == 2021), aes(label = sprintf('%+0.1f%%', .data[[y_par2]])), 
                  x = 2020.5, y = 0, vjust = -1, fontface = 'bold', size=5)+
        geom_text(aes(label = sprintf('%0.1f',.data[[y_par]]), y = .data[[y_par]]), vjust = -1, size=3.5)+
        labs(y = "", x = " ",
             title = "")+
        theme_bw()+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              
              axis.text.y = element_blank(),
              axis.text.x = element_text(size = 15),
              
              strip.text.x = element_text(size = 15))
}
      
      



dtT <- dtanalisi %>% filter(CDC =="SEDE TERRITORIALE DI BOLOGNA" & Costi=="Ricavo") %>% 
                  group_by(CDC,  ANNO,  Quarter) %>% 
                  summarise(Ufficiali = sum(TUff, na.rm = T), 
                            NonUfficiali = sum(TNonUff, na.rm = T), 
                            Gratuiti = sum(TGratuito, na.rm = T),
                            Pagamento = sum(TPagamento, na.rm = T), 
                            Vprod = sum(TVP, na.rm= T), 
                            AI = sum(TAI, na.rm = T)) %>% ungroup() %>% 
  ungroup %>% 
  arrange(Quarter) %>% 
                  mutate(VarUff = round((Ufficiali/lag(Ufficiali)-1)*100, 2), 
                         VarNUff = round((NonUfficiali/lag(NonUfficiali)-1)*100, 2), 
                         VarGrat = round((Gratuiti/lag(Gratuiti)-1)*100, 2), 
                         VarPag = round((Pagamento/lag(Pagamento)-1)*100, 2), 
                         VarVP = round((Vprod/lag(Vprod)-1)*100, 2), 
                         VarAI = round((AI/lag(AI)-1)*100,2)) %>% 
                  rowwise() %>% 
                  mutate(TotRic = round(sum(Ufficiali, NonUfficiali, na.rm = T),2)) %>% ungroup() %>% 
                  mutate(VarTot = round((TotRic/lag(TotRic)-1)*100, 2)) #%>%   
  #                 group_by(CDC,  ANNO,  Quarter) %>% 
  # ungroup()
 



dtCostiT <- dtanalisi %>% filter(CDC =="SEDE TERRITORIALE DI BOLOGNA" & Costi=="Costo") %>% 
                       group_by(CDC,  ANNO,  Quarter) %>% 
                       summarise(Costi = round(sum(Costo, na.rm = TRUE),2)) %>% 
  ungroup %>% 
  arrange(Quarter) %>%  
                       mutate(VarCosti = round((Costi/lag(Costi)-1)*100),2)





Tplot(df = dtT, y_par = "Ufficiali", y_par2 = "VarUff")
      
Tplot(df = df2, y_par = "N.Esami", y_par2 = "VarEsami") 

Tplot(df = dtCostiT, y_par = "Costi", y_par2 = "VarCosti" )
