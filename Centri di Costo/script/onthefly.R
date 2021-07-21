dt <- dtanalisi %>%
  group_by(`Centro di Costo`, Pagamento, ClassAnalisi,Anno, Quarter) %>%
  summarise(Valorizzato = sum(`A Tariffario`, na.rm = TRUE),
            Fatturato = sum(Fatturato, na.rm = TRUE)) %>%
  mutate(VarVal = round((Valorizzato/lag(Valorizzato) - 1) * 100, 2 ),
         VarFatt= round((Fatturato/lag(Fatturato)-1)*100, 2)) %>%
  filter(`Centro di Costo` == "SEDE TERRITORIALE DI BERGAMO") %>%
  pivot_longer(cols = 6:9, names_to = "Parametro", values_to = "metrica") %>%
  to_be(Pagamento = "Gratuito") %>%
  filter(Pagamento == "Gratuito") %>%
  pivot_wider(names_from = "Parametro", values_from = "metrica") %>%
  data.frame()




ggplot(dt, 
       aes(y = dt[,7], x = Quarter,  label=dt[,7]))+
  geom_line(aes(group = ClassAnalisi), size= 1.1, col = "darkgrey")+ 
  geom_label(size = 4, col = "blue")+
  facet_grid(ClassAnalisi~Anno, switch = "x", scales = "free")+
  geom_hline(yintercept = 0, size = 0.5)+
  labs(y = names(dt[7]), x = "Anno.Trimestre", 
       title = names(dt[7]))+
  theme_ipsum_rc()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

  