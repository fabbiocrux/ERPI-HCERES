# Filtering by Article, Conference and THesis
Production <- ERPI %>% filter(Type.document == "ART" | Type.document == "COMM" | Type.document == "THESE")

# Counting the total production
total <- Production %>% group_by(Type.document) %>% summarise(total = sum(n()))

# Total of articles
Production <- 
   Production %>% group_by(Year, Type.document) %>% tally(name = "Quantity") %>% 
   ggplot(aes(x=Year, y=Quantity, color = Type.document )) +
   geom_line() + geom_point() +
   labs(x="Années", 
        y="Nombre des documents", 
        title = "Production scientifique ERPI", 
        subtitle = "Articles, Conferences  et Thèses doctorat",
        caption =  paste0("Denière mise à jour: ", format(Sys.time(), '%d/%m/%Y') )) +
   scale_colour_discrete(name  ="Total par type\nde publication",
                         breaks=c("ART", "COMM", "THESE"),
                         labels=c(paste0("Articles = ", total$total[1]),
                                  paste0("Conferences = ", total$total[2]),
                                  paste0("Phd Thèses = ", total$total[3])
                         )) +
   # scale_shape_discrete(name  ="Type de document",
   #                       breaks=c("ART", "COMM", "THESE"),
   #                       labels=c("Article", "Conferences", "Phd Thèses"))
   coord_cartesian(ylim = c(0, 30)) +
   theme_fabio()

rm(total)