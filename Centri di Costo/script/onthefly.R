
dtanalisi %>% 
  mutate(TUff = ifelse(ClassAnalisi == "Ufficiale a Pagamento", Fatturato, 
                       ifelse(ClassAnalisi == "Ufficiale Gratuito", `A Tariffario`, 0)), 
         TNonUff = ifelse(ClassAnalisi == "Non Ufficiale a Pagamento", Fatturato, 
                          ifelse(ClassAnalisi == "Non Ufficiale Gratuito", `A Tariffario`, 0))) %>% View()
  
  
  
  
                 summarise(Tariffato = round(sum(`A Tariffario`, na.rm = TRUE), 0), 
                           Fatturato = round(sum(Fatturato, na.rm = TRUE), 0)) %>% View()
                 mutate(VarVal = round((Tariffato/lag(Tariffato) - 1) * 100, 2 ), 
                        VarFatt= round((Fatturato/lag(Fatturato)-1)*100, 2)) %>% 
                 filter(`Centro di Costo` == input$CC) %>% 
                 pivot_longer(cols = 7:10, names_to = "Parametro", values_to = "metrica") %>% 
                 to_be(Pagamento = input$paga) %>%  
                 filter(Pagamento == input$paga & Uff == input$uff) %>%  
                 pivot_wider(names_from = "Parametro", values_from = "metrica") %>% 
                 data.frame()






                 lab <- reactive(   
                   analisi %>% 
                     filter(`Centro di Costo`== input$CC) %>% 
                     select(Laboratorio, `Codice Livello 3 Centro di Costo` ) %>% 
                     data.frame())


                 sidebarPanel(
                   h4(textOutput("struttura")),
                   
                   selectInput("CC", "Seleziona il Centro di Costo", 
                               choices = c("", as.character(unique(factor(dtanalisi$`Centro di Costo`))))),
                   selectInput("uff", "Ufficiale/Non Ufficiale", 
                               choices = c("","Ufficiale", "Non Ufficiale")), 
                   selectInput("paga", "Gratuita/Pagamento", 
                               choices = c("", "Gratuita", "Pagamento"))
                   
                 ),














dt <- dtanalisi %>% 
  group_by(`Centro di Costo`, Pagamento, ClassAnalisi,Uff, Anno,  Quarter) %>% 
  summarise(Valorizzato = round(sum(`A Tariffario`, na.rm = TRUE), 0), 
            Fatturato = round(sum(Fatturato, na.rm = TRUE), 0)) %>% 
  mutate(VarVal = round((Valorizzato/lag(Valorizzato) - 1) * 100, 2 ), 
         VarFatt= round((Fatturato/lag(Fatturato)-1)*100, 2)) %>% 
  filter(`Centro di Costo` == "SEDE TERRITORIALE DI BERGAMO") %>% 
  pivot_longer(cols = 7:10, names_to = "Parametro", values_to = "metrica") %>% 
  to_be(Pagamento = "Pagamento") %>%  
  filter(Pagamento == "Pagamento" & Uff == "Ufficiale") %>%  
  pivot_wider(names_from = "Parametro", values_from = "metrica") %>% 
  data.frame()  


  ggplot(dt, 
         aes(y = dt[,7], x = Quarter,  label=as.character(dt[,7])))+
    geom_line(aes(group = ClassAnalisi, color = Anno ==max(Anno)), size= 1.1,  )+ 
    geom_label(size = 4, aes(color = Anno == max(Anno)))+
    scale_color_manual(values = c("grey", "blue"), guide = "none") +
    
    facet_grid(~Anno, switch = "x", scales = "free")+
     
    
    geom_hline(yintercept = 0, size = 0.5)+
    labs(y = "", x = " ", 
         title = paste( "Andamento trimestrale del", names(dt[7]), "in €"
         ))+
    theme_ipsum_rc()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 15), 
          
          strip.text.x = element_text(size = 18))

  
  l <- analisi %>% 
    filter(`Centro di Costo`== "LABORATORIO LATTE") %>% 
    select(Laboratorio) %>% data.frame()   
    
    
    
 
   

 

  