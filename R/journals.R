# JOurnals de ERPI
Articles <- ERPI %>% filter(Type.document == "ART") %>% arrange(desc(Year))
Journals <- Articles  %>% group_by(Journal) %>% summarise(Quantity=n()) %>% mutate(Journal=factor(Journal, Journal))


Journals <- 
   Journals %>%
   arrange(Quantity) %>%
   filter(Quantity >= 2) %>%
   mutate(Journal=factor(Journal, Journal)) %>%
   ggplot( aes(x=Journal, y=Quantity) ) +
   geom_segment( aes(x=Journal ,xend=Journal, y=0, yend=Quantity), color="grey") +
   geom_point(size=3, color="#69b3a2") +
   coord_flip() +
   theme_fabio() +
   #theme_minimal(base_size = 15, base_family = "Palatino") +
   labs(x = "", y = "Quantité  d'article", title = "Journaux scientifiques principaux",
        subtitle = "Minimum 2 articles dans ces Journaux") +
   #    theme_ipsum() +
   theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position="none"
   ) +
   xlab("")


rm(Articles)
