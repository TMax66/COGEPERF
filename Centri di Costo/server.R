server<-function(input, output) {
  
  dt <- reactive(dtanalisi %>% 
                   group_by(`Centro di Costo`, Pagamento, ClassAnalisi,Uff, Anno,  Quarter) %>% 
                   summarise(Valorizzato = round(sum(`A Tariffario`, na.rm = TRUE), 0), 
                             Fatturato = round(sum(Fatturato, na.rm = TRUE), 0)) %>% 
                   mutate(VarVal = round((Valorizzato/lag(Valorizzato) - 1) * 100, 2 ), 
                          VarFatt= round((Fatturato/lag(Fatturato)-1)*100, 2)) %>% 
                   filter(`Centro di Costo` == input$CC) %>% 
                   pivot_longer(cols = 7:10, names_to = "Parametro", values_to = "metrica") %>% 
                   to_be(Pagamento = input$paga) %>%  
                   filter(Pagamento == input$paga & Uff == input$uff) %>%  
                   pivot_wider(names_from = "Parametro", values_from = "metrica") %>% 
                   data.frame()
  )
  
 titolo <- reactive(
   paste(input$uff,input$paga)
 )
  
 output$Titolo <- renderText({
   req(input$uff, input$paga)
   titolo()})
 
  output$plot1 <- renderPlot({
    req(input$CC, input$paga, input$uff)
 
    p1 <- ggplot(dt(), 
                 aes(y = dt()[,7], x = Quarter,  label=paste(as.character(dt()[,7]), "€")))+
      geom_line(aes(group = ClassAnalisi, color = Anno ==max(Anno)), size= 1.1,  )+ 
      geom_label(size = 5, aes(color = Anno == max(Anno)))+
      scale_color_manual(values = c("grey", "blue"), guide = "none") +
      facet_grid(~Anno, switch = "x", scales = "free")+
      geom_hline(yintercept = 0, size = 0.5)+
      labs(y = "", x = " ", 
           title = paste( "Andamento trimestrale del", names(dt()[7])))+
      theme_ipsum_rc()+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
             
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 15), 
            
            strip.text.x = element_text(size = 18))
            
    
    
    p2 <- ggplot(dt(), 
                 aes(y = dt()[,8], x = Quarter,  label=paste(dt()[,8], "%")))+
      geom_line(aes(group = ClassAnalisi, color = Anno ==max(Anno)), size= 1.1,  )+ 
      geom_label(size = 5, aes(color = Anno == max(Anno)))+
      scale_color_manual(values = c("grey", "blue"), guide = "none") +
      facet_grid(~Anno, switch = "x", scales = "free")+
      geom_hline(yintercept = 0, size = 0.5)+
      labs(y = "" , x = " ", 
           title = paste("Variazione % del", names(dt()[7]), "rispetto al trimestre precedente")
           )+
      theme_ipsum_rc()+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.title.y = element_text(size= 18),
            #axis.text.y = element_text(size= 16),
            axis.text.y = element_blank(),
            
            axis.text.x = element_text(size = 15),
            strip.text.x = element_text(size = 18))
    
    p1/p2
    
  })
  
  
  
}
