# Impact Factor ----
IF <- 
   Articles  %>% 
   drop_na(IF2020) %>% 
   group_by(Journal) %>% 
   summarise(Quantity=n()) 

IF <- 
   IF %>% 
   left_join(Articles %>% select(Journal,IF2020) %>% unique(), by="Journal") %>% 
   arrange(IF2020) 

#IF <- IF %>% mutate(Journal=factor(Journal, Journal))

IF$Journal <- str_to_title(IF$Journal) 
Impact.Factor <- 
   IF %>%
   #filter(Quantity >= 2) %>%
   mutate(Journal=factor(Journal, Journal)) %>%
   ggplot( aes(x=Journal, y=IF2020) ) +
   geom_segment( aes(x=Journal ,xend=Journal, y=0, yend=IF2020), color="grey") +
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

# ggsave(Impact.Factor, 
#         path = "Figures/Impact.Factor.png",
#         width = 8, height = 7, dpi="print" )

rm(IF)