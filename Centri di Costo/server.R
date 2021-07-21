server<-function(input, output) {
  
  dt <- reactive(dtanalisi %>% 
                   group_by(`Centro di Costo`, Pagamento, ClassAnalisi, Quarter) %>% 
                   summarise(Valorizzato = sum(`A Tariffario`, na.rm = TRUE), 
                             Fatturato = sum(Fatturato, na.rm = TRUE)) %>% 
                   mutate(VarVal = round((Valorizzato/lag(Valorizzato) - 1) * 100, 2 ), 
                          VarFatt= round((Fatturato/lag(Fatturato)-1)*100, 2)) %>% 
                   filter(`Centro di Costo` == input$CC) %>% 
                   pivot_longer(cols = 5:8, names_to = "Parametro", values_to = "metrica") %>% 
                   to_be(Pagamento = input$paga) %>%  
                   filter(Pagamento == input$paga) %>%  
                   pivot_wider(names_from = "Parametro", values_from = "metrica") %>% 
                   data.frame()
  )
  
 
  
  output$varchange <- renderPlot({
    req(input$CC, input$paga)
 
    p1 <- ggplot(dt(), 
      aes(y = dt()[,5], x = Quarter,  label=dt()[,5]))+
      geom_line(aes(group = ClassAnalisi), size= 1.1, col = "darkgrey")+ 
      geom_label(size = 4, col = "blue")+
      facet_wrap(.~ClassAnalisi, ncol = 2, scales = "free")+
      geom_hline(yintercept = 0, size = 0.5)+
      labs(y = names(dt()[5]), x = "Anno.Trimestre", 
           title = names(dt()[5]))+
      theme_ipsum_rc()+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    p2 <- ggplot(dt(), 
                 aes(y = dt()[,6], x = Quarter,  label=dt()[,6]))+
      geom_line(aes(group = ClassAnalisi), size= 1.1, col = "darkgrey")+ 
      geom_label(size = 4, col = "blue")+
      facet_wrap(.~ClassAnalisi, ncol = 2, scales = "free")+
      geom_hline(yintercept = 0, size = 0.5)+
      labs(y = names(dt()[6]), x = "Anno.Trimestre", 
           title = names(dt()[6]))+
      theme_ipsum_rc()+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    p1/p2
    
  })
  
  
  
}
