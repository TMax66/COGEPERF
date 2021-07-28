 dtanalisi %>% filter(`Centro di Costo`== "LABORATORIO BENESSERE ANIMALE, BIOCHIMICA CLINICA, IMMUNOLOGIA VETERINARIA E STABULARI" & `Costo o Ricavo`=="Ricavo") %>% 
            group_by(`Centro di Costo`,  Anno,  Quarter) %>% 
            summarise(N.Esami = sum(Determinazioni, na.rm = TRUE), 
                      Ufficiali = sum(AttUff, na.rm = TRUE), 
                      "Non Ufficiali" =sum(AttNUff, na.rm = TRUE), 
                      "Esami A Pagamento" = sum(AttPag, na.rm = TRUE), 
                      "Esami Gratuiti" = sum(AttGrat, na.rm = TRUE), 
                      "Produzione Interna" = sum(AI , na.rm = TRUE), 
                      N.Prodotti = sum(VP, na.rm = TRUE))



   dtanalisi %>% 
   filter(`Centro di Costo`== "LABORATORIO BENESSERE ANIMALE, BIOCHIMICA CLINICA, IMMUNOLOGIA VETERINARIA E STABULARI" & `Costo o Ricavo`=="Ricavo") %>% 
   group_by(`Centro di Costo`,  Anno,  Quarter) %>% 
   summarise(N.Esami = sum(Determinazioni, na.rm = TRUE), 
             EUff = sum(AttUff, na.rm = TRUE), 
             ENUff =sum(AttNUff, na.rm = TRUE), 
             EPag = sum(AttPag, na.rm = TRUE), 
             Egrat = sum(AttGrat, na.rm = TRUE), 
             PI = sum(AI , na.rm = TRUE), 
             Prodv = sum(VP, na.rm = TRUE)) %>% 
   ungroup() %>% 
   mutate(VarEsami = round((N.Esami/lag(N.Esami)-1)*100, 2), 
          VarEUff = round((EUff/lag(EUff)-1)*100, 2), 
          VarENUff = round((ENUff/lag(ENUff)-1)*100, 2), 
          VarEPag = round((EPag/lag(EPag)-1)*100, 2),
          VarEgrat = round((Egrat/lag(Egrat)-1)*100, 2),
          VarPI = round((PI/lag(PI)-1)*100, 2), 
          VarProdv = round((Prodv/lag(Prodv)-1)*100, 2), 
   ) %>% View()












dtT <-  dtanalisi %>% filter(`Centro di Costo`== "SEDE TERRITORIALE DI BERGAMO" & `Costo o Ricavo`=="Ricavo") %>% 
                   group_by(`Centro di Costo`,  Anno,  Quarter) %>% 
                   summarise(Ufficiali = sum(TUff, na.rm = T), 
                             NonUfficiali = sum(TNonUff, na.rm = T), 
                             Gratuiti = sum(TGratuito, na.rm = T),
                             Pagamento = sum(TPagamento, na.rm = T)) %>% ungroup() %>% 
                   mutate(VarUff = round((Ufficiali/lag(Ufficiali)-1)*100, 2), 
                          VarNUff = round((NonUfficiali/lag(NonUfficiali)-1)*100, 2), 
                          VarGrat = round((Gratuiti/lag(Gratuiti)-1)*100, 2), 
                          VarPag = round((Pagamento/lag(Pagamento)-1)*100, 2)) %>% 
                   rowwise() %>% 
                   mutate(TotRic = sum(Ufficiali, NonUfficiali, na.rm = T)) %>% ungroup() %>% 
                   mutate(VarTot = round((TotRic/lag(TotRic)-1)*100, 2)) %>%   
                   group_by(`Centro di Costo`,  Anno,  Quarter)
 


Tplot <- function(df, y_par, y_par2)
{    
  p1 <- ggplot(df)+ 
         aes(
           y = .data[[y_par]],
           x = .data[["Quarter"]],  
           label=paste(as.character(.data[[y_par]]), "€"))+
  geom_line(group = 1, aes(color = Anno ==max(Anno)), size= 1.1,  )+
  geom_label(size = 4.5, aes(color = Anno == max(Anno)))+
  scale_color_manual(values = c("grey", "blue"), guide = "none") +
  facet_grid(~Anno, switch = "x", scales = "free")+
  geom_hline(yintercept = 0, size = 0.5)+
  labs(y = "", x = " ",
       title = "")+
  theme_ipsum_rc()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 15),
        
        strip.text.x = element_text(size = 18))
  
  p2 <- ggplot(df)+ 
    aes(
      y = .data[[y_par2]],
      x = .data[["Quarter"]],  
      label=paste(as.character(.data[[y_par2]]), "€"))+
    geom_line(group = 1, aes(color = Anno ==max(Anno)), size= 1.1,  )+
    geom_label(size = 4.5, aes(color = Anno == max(Anno)))+
    scale_color_manual(values = c("grey", "blue"), guide = "none") +
    facet_grid(~Anno, switch = "x", scales = "free")+
    geom_hline(yintercept = 0, size = 0.5)+
    labs(y = "", x = " ",
         title = "")+
    theme_ipsum_rc()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 15),
          
          strip.text.x = element_text(size = 18))
  
  p1|p2
  
  
}


  Tplot(dtT,  "TotRic", "VarTot" )
