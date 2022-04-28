Scimago <- 
   ERPI %>% 
   filter(Type.document == "ART") %>% 
   select(Year, QScimago) %>% 
   replace_na( list( QScimago =  'No Quartile')) %>% 
   group_by(Year, QScimago) %>% 
   tally(name = "Quantity") 
   #drop_na()

#Scimago$QScimago %>% as.factor() %>% levels()
Scimago <- 
   Scimago %>% 
   mutate(QScimago = 
             factor(QScimago, 
                    levels = c("Q1", "Q2", "Q3", "Q4", "No Quartile"),
                    labels = c("Q1", "Q2", "Q3", "Q4", "No Quartile"))
      )



Scimago <- 
   Scimago %>%    
   ggplot(aes(x=Year, y=Quantity, fill = QScimago, label = Quantity )) +
   geom_bar(stat = "identity") +
   #geom_line() + geom_point() +
   labs(x="Années", 
        y="Nombre des documents", 
        title = "Production scientifique ERPI", 
        subtitle = paste0("Articles scientifiques: ", sum(Scimago$Quantity)),
        caption =  paste0("Denière mise à jour: ", format(Sys.time(), '%d/%m/%Y'))) + 
   scale_x_continuous(breaks = c(2016:2022))  +
   scale_fill_manual(name  = "Quartiles\nScimago",
                     values = rev(brewer.pal(6, "Blues")[2:6]))+   
   # ?scale_fill_brewer(name  = "Quartiles\nScimago",
   #                   palette = "Blues",
   #                   direction = -1)  +
   theme_fabio() +
   coord_cartesian(ylim = c(0, 30)) +
   geom_text(size = 3, 
             position = position_stack(vjust = 0.5))

#   my_colors <- RColorBrewer::brewer.pal(6, "Blues")[2:6]