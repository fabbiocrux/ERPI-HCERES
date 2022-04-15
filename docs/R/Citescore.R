
# Obtaining the CIteScore  
Citescore <-  Scopus %>% select(Journal, CiteScore) %>% unique()

Articles <- 
   HAL %>% 
   filter(Type.document == "ART") %>% 
   filter(Scopus == "Oui") %>% 
   left_join(Citescore, by= "Journal")  

Articles$Year <- as.factor(Articles$Year)

Articles %>% 
   ggplot( aes(x=Year, y=CiteScore)) +
   geom_boxplot(fill="slateblue", alpha=0.5) +
   labs(x="Années", y="CiteScore", title = "Profile CiteScore de l'ERPI", subtitle = "") +
   theme_fabio()