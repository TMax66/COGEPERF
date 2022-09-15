# grafici x dipartimento

# conferimenti 


# grafici Home




# grafici lombardia----

### attività complessiva----
output$clombTotY <- renderPlot(
  tsyear(conf, dip = "Dipartimento area territoriale Lombardia")
)

output$clombTotM <- renderPlot(
  tsmonth(conf, dip = "Dipartimento area territoriale Lombardia")
)


output$clombTotW <- renderPlot(
  tsweek(conf, dip = "Dipartimento area territoriale Lombardia")
)


### santà animale----
output$salombY <-  renderPlot(
  tsyear(conf, set = "Sanità Animale", dip = "Dipartimento area territoriale Lombardia")
)

output$salombM <-  renderPlot(
  tsmonth(conf, set = "Sanità Animale", dip = "Dipartimento area territoriale Lombardia")
)


output$salombW <-  renderPlot(
  tsweek(conf, set = "Sanità Animale", dip = "Dipartimento area territoriale Lombardia")
)

### alimenti uomo----
output$aulombY <-  renderPlot(
  tsyear(conf, set = "Alimenti Uomo", dip = "Dipartimento area territoriale Lombardia")
)

output$aulombM <-  renderPlot(
  tsmonth(conf, set = "Alimenti Uomo", dip = "Dipartimento area territoriale Lombardia")
)


output$aulombW <-  renderPlot(
  tsweek(conf, set = "Alimenti Uomo", dip = "Dipartimento area territoriale Lombardia")
)




### alimenti zootecnici----
output$azlombY <-  renderPlot(
  tsyear(conf, set = "Alimenti Zootecnici", dip = "Dipartimento area territoriale Lombardia")
)

output$azlombM <-  renderPlot(
  tsmonth(conf, set = "Alimenti Zootecnici", dip = "Dipartimento area territoriale Lombardia")
)


output$azlombW <-  renderPlot(
  tsweek(conf, set = "Alimenti Zootecnici", dip = "Dipartimento area territoriale Lombardia")
)








