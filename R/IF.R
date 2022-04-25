# Impact Factor
JCR <- read_csv("JCR/JCR.csv", skip = 1) %>% 
   rename(Journal=`Full Journal Title`,
          Impact_factor_2019= `Journal Impact Factor`) %>% 
   select(Journal, Impact_factor_2019)
JCR$Journal = tolower(JCR$Journal)



# IF manual ----
IF_manual <- tibble(
   Journal=c("technological forecasting and social change",
             "geoderma",
             "resources, conservation and recycling",
             "international journal of disaster risk reduction",
             "the canadian journal of chemical engineering",
             "creativity and innovation management",
             "ai edam",
             "international review of administrative sciences",
             "industrial marketing management",
             "renewable and sustainable energy reviews",
             "chemical engineering research and design",
             "ecological indicators",
             "thinking skills and creativity"
   ),
   Impact_factor_2019 = c(5.846,
                          4.848,
                          8.086,
                          2.896,
                          1.687,
                          2.113,
                          1.119,
                          2.219,
                          4.695,
                          12.110,
                          3.350,
                          4.229,
                          2.068)
)

JCR <- JCR %>% rbind(IF_manual)

# Selectiing articles
Articles <- 
   ERPI %>% filter(Type.document == "ART") %>% 
   arrange(desc(Year)) %>% 
   select(Year, Title, Authors, Journal)
#names(ERPI)

Articles <- Articles %>% left_join(JCR, by= "Journal")

library(stringr)


# Impact Factor ----
IF <- 
   Articles  %>% 
   drop_na(Impact_factor_2019) %>% 
   group_by(Journal) %>% 
   summarise(Quantity=n()) 

IF <- 
   IF %>% 
   left_join(Articles %>% select(Journal,Impact_factor_2019) %>%
                unique(), by="Journal") %>% 
   arrange(Impact_factor_2019) 

IF <- 
   IF %>% mutate(Journal=factor(Journal, Journal))

IF$Journal <- str_to_title(IF$Journal) 

Impact.Factor <- 
   IF %>%
   #filter(Quantity >= 2) %>%
   mutate(Journal=factor(Journal, Journal)) %>%
   ggplot( aes(x=Journal, y=Impact_factor_2019) ) +
   geom_segment( aes(x=Journal ,xend=Journal, y=0, yend=Impact_factor_2019), color="grey") +
   geom_point(size=3, color="#69b3a2") +
   geom_text( label=paste0("(", IF$Quantity, ")"), nudge_x = 0, nudge_y = 0.45, check_overlap = T, size=3, color="#69b3a2" )+
   coord_flip() +
   theme_fabio() +
   #theme_minimal(base_size = 15, base_family = "Palatino") +
   labs(x = "", y = "Impact Factor", title = "Journaux scientifiques principaux",
        subtitle = "(Quantité d'articles publies par ERPI)",
        caption =  paste0("Denière mise à jour: ", format(Sys.time(), '%d/%m/%Y'))) + 
   theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position="none"
   ) +
   xlab("")

rm(IF, Articles)