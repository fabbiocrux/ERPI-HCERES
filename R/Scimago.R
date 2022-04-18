
Scimago <- 
   ERPI %>% 
   filter(Type.document == "ART") %>% 
   group_by(Year, `Quartile-Scimago-Engineering`) %>% 
   tally(name = "Quantity") %>% 
   drop_na()

Scimago <- 
   Scimago %>%    
   ggplot(aes(x=Year, y=Quantity, fill = `Quartile-Scimago-Engineering` )) +
   geom_bar(stat = "identity") +
   #geom_line() + geom_point() +
   labs(x="Années", 
        y="Nombre des documents \n QuArtiles", 
        title = "Production scientifique ERPI", 
        subtitle = paste0("Articles réferences: ", sum(Scimago$Quantity))
   )+
   scale_x_continuous(breaks = c(2016:2022))  +
   scale_fill_brewer(name  = "Scimago (Engineering)",
                     palette = "Blues",
                     direction = -1
   )  +
   
   theme_fabio() +
   coord_cartesian(ylim = c(0, 8))

