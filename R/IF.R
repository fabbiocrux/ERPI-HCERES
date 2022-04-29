# Grafica de Evolution ----
IF.evolution <- 
   Articles %>% 
   #select(Year, Impact.Factor) %>% 
   #replace_na( list( Impact.Factor =  'No Impact Factor')) %>% 
   group_by(Year, IF) %>% 
   tally(name = "Quantity") 



# Mean IF value
IF.media <- 
Articles %>% 
   filter(IF == "Avec FI") %>% 
   group_by(Year) %>% 
   summarise(IF.mean = mean(Impact.Factor)) 



# Value used to transform the data

IF.evolution <- 
   IF.evolution %>%    
   ggplot(aes(x=Year)) +
   annotate("rect", xmin = 2015.5, xmax = 2021.5, ymin = -1, ymax = 30,
            alpha = .1,fill = "blue") +
   geom_bar(aes(y=Quantity, fill = IF, label = Quantity), stat="identity") + #aes(y=Quantity, fill = IF, label = Quantity),
   geom_line(data =IF.media,   aes(y = IF.mean*5)) +
   geom_point(data =IF.media,   aes(y = IF.mean*5)) +
   scale_y_continuous(
      # Features of the first axis
      name = "Nombre des documents",
      # Add a second axis and specify its features
      sec.axis = sec_axis(~./5, 
                          name="Facteur d'Impact moyenne",
                          breaks = c(0:5))
     )+ 
   labs(x="Années", 
        #y="", 
        title = "Production scientifique ERPI", 
        subtitle = paste0("ACL 2016-2021: ", nrow(Articles %>% filter(Year >= 2016, Year<=2021))),
        caption =  paste0("Denière mise à jour: ", format(Sys.time(), '%d/%m/%Y'))) + 
   scale_x_continuous(breaks = c(2016:2022))  +
   scale_fill_manual(name  = "Facteur d'Impact",
                     values = brewer.pal(6, "Blues")[c(2,5)])+   
   #scale_fill_brewer(name  = "Quartiles\nScimago",
   #                   palette = "Blues",
   #                   direction = 1)  +
   theme_fabio() +
   coord_cartesian(ylim = c(0, 30)) +
   geom_text(aes(y=Quantity, fill = IF, label = Quantity),
             size = 5, 
             position = position_stack(vjust = 0.5)) +
   geom_segment(aes(x = 2020.7, y = 25, xend = 2021.2, yend = 25),
                arrow = arrow(length = unit(0.2, "cm"))) 
   


