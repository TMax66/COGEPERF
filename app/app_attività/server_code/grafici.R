# grafici x dipartimento

# conferimenti annuali


# funzioni

tsyear <- function(dt, set, dip){
  
  dt  %>% 
    group_by(tempo = floor_date(as.Date(Data_Accettazione), unit = "quarter")) %>%  
    filter(settore == set, 
           Dipartimento == dip) %>%   
    summarise(conferimenti = sum(conferimenti)) %>%   
    mutate(trimestre = rollmean(conferimenti, k = 4, fill = NA)) %>% 
    ggplot()+
    aes(x = tempo,
        y = conferimenti)+
    geom_line(
      aes(x = tempo,
          y = trimestre), col = "blue", size = 1.5)+
    geom_point()+
    geom_line()+
    labs(x = "", y = "n.conferimenti per trimestre")
  
}


tsmonth <-  function(dt, set, dip){
  
  dt %>% 
    mutate(
           anno = year(Data_Accettazione)) %>%
    filter(settore == set,
           Dipartimento == dip) %>%   
    group_by(tempo = floor_date(as.Date(Data_Accettazione), unit = "week")) %>%  
    filter(anno == year(lubridate::floor_date(Sys.Date(), unit = "years"))) %>% 
    summarise(conferimenti = sum(conferimenti)) %>%   
    mutate(sett = rollmean(conferimenti, k = 4, fill = NA)) %>% 
    ggplot()+
    aes(x = tempo,
        y = conferimenti)+
    geom_line(
      aes(x = tempo, 
          y = sett), col = "blue", size = 1.5)+
    geom_point()+
    geom_line()+
    scale_x_date(NULL, date_labels = "%b %d", breaks = "week")+
    theme(axis.text.x = element_text(angle =45, hjust = 1))
    

}


tsweek <- function(dt, set, dip){
  
  dt %>% 
    mutate( anno = year(Data_Accettazione), 
            sett= week(Data_Accettazione)) %>%   
    filter(settore == set,
           Dipartimento == dip,  
           anno == year(lubridate::floor_date(Sys.Date(), unit = "years")), 
           sett == week(lubridate::floor_date(Sys.Date(), unit = "weeks"))-1) %>%
    group_by(Data_Accettazione) %>%  
    summarise(conferimenti = sum(conferimenti)) %>%   
    ggplot()+
    aes(x = Data_Accettazione,
        y = conferimenti)+
    geom_point()+
    geom_line()+
    theme(axis.text.x = element_text(angle =45, hjust = 1))+
    labs(x = "data di accettazione", y= "n.conferimenti giornalieri" )

}





# grafici lomb

output$clombY <-  renderPlot(
  tsyear(conf, set = "Sanità Animale", dip = "Dipartimento area territoriale Lombardia")
)

output$clombM <-  renderPlot(
  tsmonth(conf, set = "Sanità Animale", dip = "Dipartimento area territoriale Lombardia")
)


output$clombW <-  renderPlot(
  tsweek(conf, set = "Sanità Animale", dip = "Dipartimento area territoriale Lombardia")
)









