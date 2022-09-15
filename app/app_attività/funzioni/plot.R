tsyear <- function(dt, dip, set = NULL){
  
  if(is.null(set)){ 
    
    dt  %>% 
      group_by(tempo = floor_date(as.Date(Data_Accettazione), unit = "quarter")) %>%  
      filter(Dipartimento == dip) %>%   
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
    
    } else { 
  
  dt  %>% 
    group_by(tempo = floor_date(as.Date(Data_Accettazione), unit = "quarter")) %>%  
    filter( settore == set,
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
  
}


tsmonth <-  function(dt,  dip, set = NULL){
  
  if(is.null(set)){  
  
  
    conf %>% 
      mutate(
        anno = year(Data_Accettazione)) %>%
      filter(
        Dipartimento == dip) %>%   
      group_by(tempo = floor_date(as.Date(Data_Accettazione), unit = "month")) %>%  
      filter(anno == year(lubridate::floor_date(Sys.Date(), unit = "years"))) %>%  
      summarise(conferimenti = sum(conferimenti)) %>% 
      ggplot()+
      aes(x = tempo,
          y = conferimenti)+
      geom_point()+
      geom_line()+
      scale_x_date(NULL, date_labels ="%B", breaks = "month")
  
  
} else { 

  dt %>% 
    mutate(
      anno = year(Data_Accettazione)) %>%
    filter( settore == set,
      Dipartimento == dip) %>%   
    group_by(tempo = floor_date(as.Date(Data_Accettazione), unit = "month")) %>%  
    filter(anno == year(lubridate::floor_date(Sys.Date(), unit = "years"))) %>% 
    summarise(conferimenti = sum(conferimenti)) %>%   
    #mutate(sett = rollmean(conferimenti, k = 4, fill = NA)) %>% 
    ggplot()+
    aes(x = tempo,
        y = conferimenti)+
    geom_point()+
    geom_line()+
    scale_x_date(NULL, date_labels ="%B", breaks = "month")
  
  
}
}  
  
  
tsweek <- function(dt, dip, set = NULL){
  
  if(is.null(set)){ 
  
    dt %>% 
      mutate(
        anno = year(Data_Accettazione), 
        mese = month(Data_Accettazione)) %>%
      filter(
        Dipartimento == dip) %>%   
      group_by(tempo = floor_date(as.Date(Data_Accettazione), unit = "weeks")) %>%  
      filter(anno == year(lubridate::floor_date(Sys.Date(), unit = "years")), 
             mese == month(lubridate::floor_date(Sys.Date(), unit = "months"))) %>%   
      summarise(conferimenti = sum(conferimenti)) %>%    
    
    ggplot()+
      aes(x = tempo,
          y = conferimenti)+
      
      geom_point()+
      geom_line()+
      scale_x_date(NULL, date_labels =c("%d") , breaks = "week")+
    labs(x = "data di accettazione", y= "n.conferimenti settimanali" )
  
  } else { 

    dt %>% 
      mutate(
        anno = year(Data_Accettazione), 
        mese = month(Data_Accettazione)) %>%
      filter( settore == set,
        Dipartimento == dip) %>%   
      group_by(tempo = floor_date(as.Date(Data_Accettazione), unit = "weeks")) %>%  
      filter(anno == year(lubridate::floor_date(Sys.Date(), unit = "years")), 
             mese == month(lubridate::floor_date(Sys.Date(), unit = "months"))) %>%   
      summarise(conferimenti = sum(conferimenti)) %>%    
    
    ggplot()+
      aes(x = tempo,
          y = conferimenti)+
      
      geom_point()+
      geom_line()+
      scale_x_date(NULL, date_labels =c("%d") , breaks = "week")+
      labs(x = "data di accettazione", y= "n.conferimenti settimanali" )
}
}
